
library(tidyverse)
library(dplyr)
library(qgraph)
library(GGally)
library(network)

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
'Fatigue PRO',
'FLU PRO')



#Set data and results areas up
sourcedata <- "C:/Users/q1062810/OneDrive - IQVIA/Wonderful Wednesday/Psychometrics/"

setwd(sourcedata)
dat <-read.csv("PSI_WW_psychometric.csv")

dat_t1 <- dat %>%
  select(c(ends_with("_T1"), starts_with("T1") )) 
  

colnames(dat_t1) <- Names
cor(dat_t1)
dat_t1 <- dat_t1 # %>%
  #select(-`SDQ TOTAL`)
groups <- factor(c(
  rep("SDQ_ITEMS", 12),
  rep("SDQ_TOTAL", 1),
  rep("FATIGUE_PRO", 1),
  rep("FLU_PRO", 1)))


qgraph(cor(dat_t1), layout = "spring", labels =
         colnames(dat_t1),
       groups = groups, graph="glasso", sampleSize=2000)

