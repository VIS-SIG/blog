
# Load required packages
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggtext)
library(grid)
library(gridExtra)

# Load Data and Create Numeric Variable for x-axis
# Create numeric variable for AVAL
df <- read_excel("WWW_AUG2025.xlsx") %>%
  mutate(AVISITN = case_when(AVISIT=="BASE" ~ 0,
                             AVISIT!="BASE" ~ 
                               as.numeric(substring(AVISIT, nchar(AVISIT)-1))))

# Variable for Sample Size of Each Arm
N_A <- as.numeric(length(unique(df[df$TRT=="Treatment A",]$USUBJID)))
N_B <- as.numeric(length(unique(df[df$TRT=="Treatment B",]$USUBJID)))



# For subjects experiencing an ICE computing mean score by visit
# relative to last visit prior to ICE

# Setting up data

df_ICEvis <- df %>%
  filter(ICE == "Y") %>%
  group_by(USUBJID) %>%
  slice_head() %>%
  ungroup() %>%
  mutate(nbasevis = AVISITN - 1) %>%
  select(USUBJID, nbasevis)

df_ICE <- df %>%
  inner_join(df_ICEvis, by = "USUBJID") %>%
  mutate(nvis = AVISITN - nbasevis)

df_ICE_mean <- aggregate(as.numeric(df_ICE$AVAL),
                     by=list(df_ICE$TRT, df_ICE$nvis),
                     FUN=mean) %>%
               rename(Treatment = Group.1,
                      RelMonth = Group.2,
                      Mean = x)

df_ICE_ns <- df_ICE %>%
  count(TRT, nvis) %>%
  mutate(ypos = case_when(TRT=="Treatment A" ~ -20,
                          TRT=="Treatment B" ~ -30)) %>%
  rename(Treatment = TRT)

# Number of subjects experiencing an ICE on each arm
# Variable for Sample Size of Each Arm
N_ICE_A <- as.numeric(length(
  unique(df[df$TRT=="Treatment A"&df$ICE=="Y",]$USUBJID)))
N_ICE_B <- as.numeric(length(
  unique(df[df$TRT=="Treatment B"&df$ICE=="Y",]$USUBJID)))

ggplot(df_ICE_mean, aes(x=RelMonth, y=Mean, colour=Treatment)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(-40, 110),
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(limits = c(-12, 12),
                     expand = c(0, 0),
                     breaks = seq(-11, 11, by = 1)) +
  labs(title = paste("Mean VAS Score Relative to Intercurrent Event"),
       subtitle = "Following intercurrent events, subjects receiving 
       <span style = 'color: #619CFF;'>Treatment B</span>
       eventually reach similar VAS scores to those reached after intercurrent
       events on 
       <span style = 'color: #F8766D;'>Treatment A</span>",
       caption = paste("Data are analysed as observed. Scores are grouped",
                       "by number of visits relative to a subject's last",
                       "before they experience an intercurrent event. \n",
                       "Subjects experiencing an intercurrent event -",
                       "Treatment A: N = ", N_ICE_A,
                       "Treatment B: N = ", N_ICE_B),
       x = "Visit Relative to Last Before Intercurrent Event") +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5),
        axis.title.y = element_blank(),
        plot.title = element_markdown(size = 15),
        plot.subtitle = element_markdown(size = 11,
                                         margin = margin(0, 0, 15, 0)),
        plot.caption = element_text(hjust = 0,
                                    margin = margin(15, 0, 0, 0)),
        legend.position = "none") +
  geom_hline(yintercept = 0, color = "black") +
  geom_text(data = df_ICE_ns, aes(x = nvis, y = ypos, label = n)) +
  geom_segment(x = 0, xend = 0,
               y = 0, yend = 100,
               colour = "black",
               linetype = 2) +
  annotate("text", x = -10, y = -10,
           label = "Number of Observations") +
  annotate("text", x = 0, y = 105,
           label = "Last Observation Prior to Intercurrent Event")


