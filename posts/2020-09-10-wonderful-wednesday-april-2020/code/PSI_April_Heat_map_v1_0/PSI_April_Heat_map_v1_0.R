## Heatmap for PSI Wonderful Wednesday - April

## Load required packages ====

library(tidyverse)
library(plotly)
library(survival)

## Read in data and assign colours ====

Orange <- "#EF7F04"
Green <- "#68B937"
Blue <- "#00A6CB"
Grey <- "#4E5053"
Darkblue <- "#003569"
Yellow <- "#FFBB2D"

## Assumes data saved in same working directory
ADTTE <- read_csv('2020-04-08-psi-vissig-adtte_v2.csv')

## change variable types as necessary
ADTTE$STR01L <- as.factor(ADTTE$STR01L)
ADTTE$STR02L <- as.factor(ADTTE$STR02L)
ADTTE$TRT01P <- as.factor(ADTTE$TRT01P)
ADTTE$DCTREAS <- as.factor(ADTTE$DCTREAS)
ADTTE$EVNTDESC <- as.factor(ADTTE$EVNTDESC)


## Fit survival model and reformat ====
fita <- survfit(Surv(AVAL, CNSR == 0) ~ TRT01P, data = ADTTE)

## Pull out survival function data
surv <- as.data.frame(fita$surv)

## Add treatment to survival data
treatment <- c(rep("TMB+VMB 52wks", times = 433), 
               rep("TMB 12wks,VMB 34wks", times = 454), 
               rep("TMB 52wks", times = 427), 
               rep("VMB 52wks", times = 445))

## Combine survival function data, treatment arm and time
surv_time <- surv %>%
  add_column(Days=fita$time) %>%
  add_column(Treatment_Arm=treatment) %>%
  rename(survival=`fita$surv`) %>%
  arrange(Days)

## create shell with all possible days in

shell <- tibble(Days = rep(1:1944, times = 4),
                Treatment_Arm = c(rep("TMB+VMB 52wks", times = 1944), 
                                  rep("TMB 12wks,VMB 34wks", times = 1944), 
                                  rep("TMB 52wks", times = 1944), 
                                  rep("VMB 52wks", times = 1944)))

## merge on from surv time for when we have a survival value
## Add 1 for first day at each (nobody's dead yet)
## Copy survival function values down into empty rows to replicate KM curve horizontal line

full_surv <- shell %>% 
  left_join(surv_time) %>% 
  mutate(survival = if_else(Days == 1, 1, survival)) %>% ## add 1 for first day as all alive
  group_by(Treatment_Arm) %>%  ## make sure we don't copy from one trt to the next
  arrange(Days, .by_group = TRUE) %>%  ## make sure in correct analysis day order
  fill(survival) %>%  ## fills in empty survival function values
  ungroup()

testdf <- full_surv %>% 
  pivot_wider(id_cols = Days, names_from = Treatment_Arm, values_from = survival)

Flag <-  case_when(
  testdf$`TMB 12wks,VMB 34wks`> testdf$`TMB 52wks` & 
    testdf$`TMB 12wks,VMB 34wks`> testdf$`TMB+VMB 52wks` & 
    testdf$`TMB 12wks,VMB 34wks`> testdf$`VMB 52wks` ~ 3,
  
  testdf$`TMB 52wks`> testdf$`TMB 12wks,VMB 34wks` & 
    testdf$`TMB 52wks`> testdf$`TMB+VMB 52wks` & 
    testdf$`TMB 52wks`> testdf$`VMB 52wks` ~ 1,
  
  testdf$`TMB+VMB 52wks`> testdf$`TMB 12wks,VMB 34wks` & 
    testdf$`TMB+VMB 52wks`> testdf$`TMB 52wks` & 
    testdf$`TMB+VMB 52wks`> testdf$`VMB 52wks` ~ 4,
  
  testdf$`VMB 52wks`> testdf$`TMB 12wks,VMB 34wks` & 
    testdf$`VMB 52wks`> testdf$`TMB 52wks` & 
    testdf$`VMB 52wks`> testdf$`TMB+VMB 52wks` ~ 2
)



## Creating text labels for hover functionality ====
## Add in text for labels, adding extra blank space for alignment
full_surv2 <- full_surv %>% 
  mutate(text1 = paste(Treatment_Arm, round(survival, digits = 4), sep = ": ")) %>% 
  mutate(text2 = if_else(condition = Treatment_Arm == "TMB 12wks,VMB 34wks", paste(Treatment_Arm, round(survival, digits = 4), sep = ":"),
                         false = if_else(condition = Treatment_Arm == "TMB+VMB 52wks", paste(Treatment_Arm, round(survival, digits = 4), sep = ":          "),
                                         false = if_else(Treatment_Arm == "VMB 52wks", paste(Treatment_Arm, round(survival, digits = 4), sep = ":                   "),
                                                         false = paste(Treatment_Arm, round(survival, digits = 4), sep = ":                   ")))))


## Remove survival values (now in text2) and reformat data to wide format
full_surv3 <- full_surv2 %>% 
  select(-survival) %>% 
  pivot_wider(id_cols = Days, names_from = Treatment_Arm, values_from = text2) 

full_surv3$Flag <- Flag

full_surv3 <- full_surv3 %>% 
  mutate(Flag = replace_na(Flag, 99))

## Merge formatted labels (full_surv3) back to data with survival values (so all labels for each observation) 
## Combines label with /n separator to ensure each on different line
## Makes the value that is hovered over bold

full_surv4 <- full_surv2 %>% 
  left_join(full_surv3) %>% 
  mutate(text2 = paste(`TMB 52wks`,`VMB 52wks`,`TMB 12wks,VMB 34wks`,  `TMB+VMB 52wks`, sep = "\n")) %>% 
  mutate(boldtrt = paste("<b>", if_else(Flag ==1, `TMB 52wks`, 
                                        false = if_else(Flag==2, `VMB 52wks`, 
                                                        false = if_else(Flag==3, `TMB 12wks,VMB 34wks`,
                                                                        false = if_else(Flag==4,`TMB+VMB 52wks`,
                                                                                        false = "")))),"</b>", sep = "")) %>% 
  mutate(text3 = if_else(Flag == 1, paste(boldtrt, `VMB 52wks`,`TMB 12wks,VMB 34wks`,  `TMB+VMB 52wks`, sep = "\n"),
                         false = if_else(Flag == 2, paste(`TMB 52wks`,boldtrt,`TMB 12wks,VMB 34wks`,  `TMB+VMB 52wks`, sep = "\n"),
                                         false = if_else(Flag == 3, paste(`TMB 52wks`,`VMB 52wks`,boldtrt,  `TMB+VMB 52wks`, sep = "\n"),
                                                         false = if_else(Flag == 4, paste(`TMB 52wks`, `VMB 52wks`,`TMB 12wks,VMB 34wks`,  boldtrt, sep = "\n"),
                                                                         false = paste(`TMB 52wks`,`VMB 52wks`,`TMB 12wks,VMB 34wks`, `TMB+VMB 52wks`, sep = "\n"))))))

## Reorder factors so that axis will match with label order ====
full_surv4$Treatment_Arm <- as.factor(full_surv4$Treatment_Arm)

full_surv4$Treatment_Arm <- fct_relevel(full_surv4$Treatment_Arm, "TMB+VMB 52wks", "TMB 12wks,VMB 34wks", "VMB 52wks","TMB 52wks")

## Create plot ====

## Create ggplot object
plot <- ggplot(data = full_surv4, aes(x = Days, y = Treatment_Arm, text = text3)) +
  ggplot2::scale_fill_gradient2(
    low = Darkblue,
    high = Green,
    mid = 'white', 
    midpoint = 0.5,
    limits = c(0, 1),
    name = "Survival\nFunction") +
  labs(y = "Treatment Arm") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

## Create static heatmap and add titles
plot <- plot + geom_tile(aes(fill = survival))

plot <-  plot + 
  annotate(geom = "text", x = 1000, y = 5.2, label = " ", hjust = "centered", vjust = "top", size=7) + 
  annotate(geom = "text", x = 1000, y = 5.05, label = "Combining therapies leads to better outcomes after one year", hjust = "centered", vjust = "top", size=7) +
  annotate(geom = "text", x = 1000, y = 4.6, label = "...but simultaneous treatment has a better overall median survival", hjust = "centered", vjust = "top", size=6) +
  geom_segment(x=365, xend=365, y=0.5, yend=4.5, linetype='dotted', col = "black") +
  geom_segment(x=1095, xend=1095, y=0.5, yend=4.5, linetype='dashed', col = "black") +
  geom_segment(x=1825, xend=1825, y=0.5, yend=4.5, linetype='solid', col = "black") +
  annotate(geom = "text", x = 375, y = 0.4, label = "Year 1", hjust = "left", vjust = "bottom", size = 3) +
  annotate(geom = "text", x = 1105, y = 0.4, label = "Year 3", hjust = "left", vjust = "bottom", size = 3) +
  annotate(geom = "text", x = 1835, y = 0.4, label = "Year 5", hjust = "left", vjust = "bottom", size = 3) +
  annotate(geom = "text", x = 1835, y = 0.3, label = " ", hjust = "left", vjust = "bottom", size = 3) 


## Transform to plotly to add hover text

ggplotly(plot, tooltip = c("text3")) %>% layout(hoverlabel = list(align = "left"))
