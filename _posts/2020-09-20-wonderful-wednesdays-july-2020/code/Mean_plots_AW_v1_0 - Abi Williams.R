## PSI Wonderful Wednesday June 
## Mean plots by change > 2 v1_0
## Abi Williams

## Load packages

library(tidyverse)
library(plotly)
library(plotrix)
library(metR) 

## Add chosen colours

Orange <- "#EF7F04"
Green <- "#68B937"
Blue <- "#00A6CB"
Grey <- "#4E5053"
Darkblue <- "#003569"
Yellow <- "#FFBB2D"

# Read in data ====

rawdat <- read_csv("hgb_data.csv")
sankdat <- read_csv("Sankey.csv")

## add visit and treatment names to sankey data

sankdat2 <- sankdat %>% 
  left_join(rawdat) %>% 
  replace_na(list(CHG = 0, CHG_FROM_BL = 0))

sankdat2$TRT01P <- as.factor(sankdat2$TRT01P)
sankdat2$AVISIT <- as.factor(sankdat2$AVISIT)

sankdat2$AVISIT <- fct_relevel(sankdat2$AVISIT, "Week 4", "Week 8", after = 1)


## Create flag for patients who have change >2 at any point =====

patsub1 <- sankdat2 %>% 
  filter(CHG > 2) %>% 
  select(USUBJID) %>% 
  distinct() %>% 
  mutate(anychg2FLN = 1) %>% 
  mutate(anychg2FLC = "Change > 2 between any two consecutive visits")


flagged_dat <- sankdat2 %>% 
  left_join(patsub1) %>% 
  replace_na(list(anychg2FLN = 0, anychg2FLC = "No change > 2 between any two consecutive visits"))



summary_flagged <- flagged_dat %>%
  group_by(AVISIT, TRT01P, anychg2FLC) %>% 
  summarise(mean_chg_BL = mean(CHG_FROM_BL),
            mean_chg_BL_ci   = std.error(CHG_FROM_BL),
            mean_chg_vis = mean(CHG),
            mean_chg_vis_ci = std.error(CHG),
            mean_raw = mean(AVAL),
            mean_raw_ci = std.error(AVAL),
            n = n())


ref <- summary_flagged %>% 
  filter(TRT01P == "Treatment C") %>% 
  ungroup() %>% 
  mutate(mean_raw = mean_raw - 0.75) %>% 
  mutate(TRT01P = "Non-inferiority boundary") %>% 
  mutate(mean_raw_ci = 0)


data <- summary_flagged %>% 
  ungroup() %>% 
  add_row(ref)

data$TRT01P <- as.factor(data$TRT01P) 

data$TRT01P <- fct_relevel(data$TRT01P, "Non-inferiority boundary", after = Inf)

## Make base plot ====

meanplot <- ggplot(data = data, aes(x = AVISIT, y = mean_raw, group = TRT01P, color = TRT01P)) ## plot object

pd <- position_dodge(width = 0.1) ## position adjustment to use

plota <- meanplot +
  # Line plot and error bars
  geom_line(aes(linetype = TRT01P), show.legend = FALSE, position = pd) +
  geom_errorbar(aes(ymin = mean_raw - mean_raw_ci, ymax = mean_raw + mean_raw_ci),
                width = .1, linetype = 1, position = pd) +
  geom_point(position = pd) +
  
  facet_wrap(vars(anychg2FLC)) +
  # Add chosen colours
  scale_color_manual(name = "Treatment", values = c(Blue, Green, Grey)) +
  scale_linetype_manual(name = "Treatment 2", values = c(1,1,2)) +
  
  
  # Title and labels
  ggtitle("Target Haemoglobin level is (on average) not reached on Treatment C without concerning (>2 Hgb g/dL) level of change between visits") +
  ylab("Mean (Standard Error) haemoglobin level [g/dL]") +
  xlab("Visit") +
  
  # Add lines/text for target range
  geom_hline(yintercept = c(10, 11.5), colour = Darkblue, linetype = 2, size = .5) +
  annotate(geom = "text", x = c(1.5), y = c(10.75), colour = Darkblue, label = "Target Range", fontface = 2) +
  
  # Add interpretation arrows (https://stackoverflow.com/questions/17032393/how-to-draw-arrow-in-ggplot2-with-annotation)
  annotate(geom = "segment", x = 1.2, y = c(10.6, 10.9), xend = 1.2, yend = c(10.2, 11.3), colour = Darkblue, size = 1.25,
           arrow = arrow(length = unit(0.4, "cm"))) +
  
  # Specify theme
  theme_bw()
# theme(plot.margin = unit(c(1,3,7,5), "lines")) 

## Add some n labels =====
dat_text <- data.frame(
  label = c("\n\n  Treatment C (n = 91)", "\n  Treatment E (n = 12)", "Non-inferiority boundary (Control - 0.75)", "\n\nTreatment C (n = 59)", "\nTreatment E (n = 138)"),
  anychg2FLC   = c(rep("Change > 2 between any two consecutive visits", times = 3), rep("No change > 2 between any two consecutive visits", times=2)),
  TRT01P = c("Treatment C", "Treatment E", "Non-inferiority boundary", "Treatment C", "Treatment E"),
  xx = rep(3.5, times =5),
  yy = rep(8.5, times = 5)
)

## Combine plot with text ====
plota + geom_text(
  data    = dat_text,
  mapping = aes(x = xx, y = yy, label = label, colour = TRT01P),
  hjust   = -0.1,
  vjust   = -1,
  inherit.aes = FALSE,
  show.legend = FALSE
) +
  guides(colour = "none")

