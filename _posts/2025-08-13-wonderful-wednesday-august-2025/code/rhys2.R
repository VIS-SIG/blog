
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

# Mean Scores Over Time Ignoring ICEs
df_mean <- aggregate(as.numeric(df$AVAL),
                     by=list(df$TRT, df$AVISITN),
                     FUN=mean) %>%
  rename(Treatment = Group.1,
         Month = Group.2,
         Mean = x)

p1 <- 
  ggplot(df_mean, aes(x=Month, y=Mean, colour=Treatment)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(limits = c(0, 12.1),
                     expand = c(0, 0),
                     breaks = seq(0, 12, by = 1)) +
  labs(title = "Ignoring Intercurrent Events",
       subtitle = "Mean Scores Increase With Both Arms but are Higher for 
       <span style = 'color: #F8766D;'>Treatment A</span>
       vs 
       <span style = 'color: #619CFF;'>Treatment B</span>",
       caption = paste("Data analysed as observed \n (i.e.,", 
       "data preceded by intercurrent events included as recorded). \n",
       "Treatment A: N =", N_A, "Treatment B: N =", N_B)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5),
        axis.title.y = element_blank(),
        plot.title = element_markdown(size = 15, hjust = 0.5),
        plot.subtitle = element_markdown(size = 11,
                                         margin = margin(0, 0, 15, 0),
                                         hjust = 0.5),
        plot.caption = element_text(hjust = 0,
                                    margin = margin(15, 0, 0, 0)),
        legend.position = "none",
        plot.margin = margin(15, 5, 0, 0))

# Repeating with scores set to missing and imputed with LOCF following ICEs
df_locf <- df %>%
  group_by(USUBJID) %>%
  mutate(AVALlocf = case_when(ICE == "N" ~ as.numeric(AVAL))) %>%
  fill(AVALlocf, .direction = "down") %>%
  ungroup()

df_locf_mean <- aggregate(as.numeric(df_locf$AVALlocf),
                     by=list(df_locf$TRT, df_locf$AVISITN),
                     FUN=mean) %>%
                rename(Treatment = Group.1,
                       Month = Group.2,
                       Mean = x)

p2 <-
  ggplot(df_locf_mean, aes(x=Month, y=Mean, colour=Treatment)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0, 0),
                     breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(limits = c(0, 12.1),
                     expand = c(0, 0),
                     breaks = seq(0, 12, by = 1)) +
  labs(title = paste("Imputing Post-Intercurrent Event Visits With LOCF"),
       subtitle = "Mean Scores Increase With 
       <span style = 'color: #F8766D;'>Treatment A</span>
       But Not With
       <span style = 'color: #619CFF;'>Treatment B</span>",
       caption = paste("Data preceded by an intercurrent event set to missing",
                       "and imputed using \n last observation carried forward",
                       "(LOCF). \n",
                       "Treatment A: N =", N_A, "Treatment B: N =", N_B)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5),
        axis.title.y = element_blank(),
        plot.title = element_markdown(size = 15, hjust = 0.5),
        plot.subtitle = element_markdown(size = 11,
                                         margin = margin(0, 0, 15, 0),
                                         hjust = 0.5),
        plot.caption = element_text(hjust = 0,
                                    margin = margin(15, 0, 0, 0)),
        legend.position = "none",
        plot.margin = margin(15, 0, 0, 5))


# Adding to plots to single plot with common title
meanplot <- grid.arrange(p1, p2, ncol=2,
                         top = textGrob("Mean VAS Scores by Visit",
                                        gp=gpar(fontsize=20)))


