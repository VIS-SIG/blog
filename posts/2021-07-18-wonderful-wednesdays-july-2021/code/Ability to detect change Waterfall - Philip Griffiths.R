
library(tidyverse)
library(dplyr)


#Clear environment
rm(list=ls())

#Colour scheme
Turquoise100 <- "#00a3e0"
Turquoise75 <- "#40bae8"
Turquoise50 <- "#7fd1ef"
Turquoise25 <- "#bfe8f7"
Blue100 <- "#005587"
Blue50 <- "#7FAAC3"
Green100 <- "#43b02a"
Green50 <- "#a1d794"
Purple100 <-"#830065"
Purple50 <- "#c17fb2"

Names <- c(
  'Dry Cough',
  'Loss of Smell',
  'Skin Rash',
  'Fever',
  'Headache',
  'Short of Breath',
  'Diarrhoea',
  'Sore Throat',
  'Fatigue',
  'Runny Nose',
  'Ocular Issues',
  'Loss of Taste',
  'SDQ TOTAL',
  'FLU PRO',
  'Fatigue PRO')



#Set data and results areas up
sourcedata <- "C:/Users/q1062810/OneDrive - IQVIA/Wonderful Wednesday/Psychometrics/"

setwd(sourcedata)
dat <-read.csv("PSI_WW_psychometric.csv")

dat_t4 <- dat %>%
  head(2000) %>%
  select(c(starts_with("T4"), "PGIC_T4" )) %>%
  mutate(Group = if_else(PGIC_T4<=3, "Improved",
                         if_else(PGIC_T4 == 4, "No change", 
                                 if_else(PGIC_T4 == 5, "Worsened a little", "Worsened a lot")))) %>%
  arrange(desc(T4_SDQ_PRO_SUM_CHG)) %>%
  mutate(x = seq(1:2000))

dat_t4$Group <- as.factor(dat_t4$Group)
dat_t4$x <- as.factor(dat_t4$x)


Plot <- dat_t4 %>%
  ggplot() + 
  geom_col(aes(x=x, y=T4_SDQ_PRO_SUM_CHG, fill=Group), width = 1,  na.rm = TRUE) +
  ggtitle("Patients self-reported improvement and worsening \nmatched their SDQ-12 change scores", subtitle = "Supporting the ability of the SDQ-12 to detect change") +
  xlab("Patients, in order of SDQ-12 change score") + 
  ylab("SDQ-12 change score") +
  scale_fill_manual(breaks = c("Improved", "No change", "Worsened a little", "Worsened a lot"), 
                    values=c(Green50, Turquoise100, Blue100, Purple100), name = "Self-Reported Condition") +
  scale_y_continuous(limits = c(-35,30), breaks=c(-35, -15, 0, 15)) +
  theme_classic() + #formatting
  theme(panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16),
        legend.text=element_text(size=14),
        legend.title=element_text(size=16),
        plot.title = element_text(size=22, face="bold"),
        plot.subtitle = element_text(size=14, face="bold")) +
  theme(axis.text.x=element_blank(), # remove the participant numbers 
        axis.ticks.x=element_blank())  # remove x-axis ticks 
Plot

png(filename = "Ability_to_detect_change.png",  width = 920, height = 540, units = "px", pointsize = 10,bg = "white")
Plot
dev.off()
