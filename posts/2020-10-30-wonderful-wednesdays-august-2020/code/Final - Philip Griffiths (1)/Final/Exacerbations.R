library(tidyverse)
library(RColorBrewer)
library(readxl)

#Colour scheme
Orange <- "#EF7F04"
Green <- "#68B937"
Blue <- "#00A6CB"
Grey <- "#4E5053"
Darkblue <- "#003569"
Yellow <- "#FFBB2D" 
GreyedOut <- "#D3D3D3"

setwd("C:/Users/philip.griffiths/OneDrive - OneWorkplace/Documents/Wonderful Wednesday/Exacerbations")

#Read in a dataset and create a flag value for exacerbations >=1
exacerbations <- read_csv("2020-07-08-COPD-PSI-data.csv")  %>%
  mutate(flag = as.numeric(ifelse(ANTHONISEN >= 1, "1", "0")))

#linear regression to adjust number of exacerbations based on Baseline values
Model <- lm(ANTHONISEN ~ FEV1_RV + PREV_EXAC + AGE_C + GENDER + SMOKER + AIRFLOW, data = exacerbations)

#logistic regression to determine the probability of >=1 exacerbation adjusted for baseline
LogisticModel <- glm(flag ~ FEV1_RV + PREV_EXAC + AGE_C + GENDER + SMOKER + AIRFLOW,data = exacerbations,family = binomial)

#Check out the models. I actually removed region. it was highly sig, but I dont have a theoretical reason for that, and also just loads of US
summary(Model)
summary(LogisticModel)

#Make an "adjusted" exacerbation rate and a probability exacerbation
Adjusted <- predict(Model, data = exacerbations)
Prob <-predict(LogisticModel, type = "response")

#add the ajusted and probability columns to the main dataset. make adjusted exacerbations per year
#Sequence1 is a consecutively ordered variable for the waterfall plot later on
exacerbations_pred <- add_column(exacerbations, adjust=Adjusted) %>%
  add_column(prob=Prob) %>%
  mutate(years=STUDY_DAYS_N/365) %>%
  mutate(ratio=adjust / years) %>%
  arrange(ratio) %>%
  add_column(sequence1 = 1:nrow(exacerbations))

#create two datasets (one for treat and one for control) and test the difference between them
treat <-exacerbations_pred %>%
  filter(RAND_TRT=="ICS/LABA")

control <- exacerbations_pred %>%
  filter(RAND_TRT=="LABA")
  
wilcox.test(treat$ratio, control$ratio,paired=FALSE)

whichmedian <- function(x) which.min(abs(x - median(x)))

#Median patient on treatment for reference line
TreatMed <- exacerbations_pred %>%
  filter(RAND_TRT=="ICS/LABA") %>%
  slice(whichmedian(ratio))

#Median patient on control for refeence line
ControlMed <- exacerbations_pred %>%
  filter(RAND_TRT=="LABA") %>%
  slice(whichmedian(ratio))

#create plot
p <- exacerbations_pred %>%
  ggplot() +
  geom_col(aes(x=sequence1, y=ratio, fill=RAND_TRT, color=RAND_TRT)) + #column - aes takes seq1 for the x as this is the reordered Ps. Need fill and colour so that the col outlines are the correct colour
  ggtitle("Group level advantage for ICS/LABA patients was replicated at the\npatient level using demographic adjusted yearly exacerbation rate") + #titles etc
  xlab("Patients, in order of adjusted exacerbation probability") +
  xlab("Patients, in order of adjusted exacerbations per year") + 
  ylab("Adjusted exacerbations per year") +
  geom_vline(aes(xintercept = sequence1), linetype='dashed', col = Darkblue, TreatMed)  + #reference lines for median treatment response and median control response
  geom_vline(aes(xintercept = sequence1), linetype='dashed', col = Green, ControlMed)  +
  scale_fill_manual(values=c(Darkblue, Green), name = "Condition", labels = c("Treatment\n(ICS/LABA)", "Control\n(LABA)")) + #make sure that the col and legend have the chosen colors. Update legend text
  scale_colour_manual(values=c(Darkblue, Green)) + #make sure the outlines for the cols are also coloured in line with the above
  guides(color = FALSE, size = FALSE) + # remove legend for the col outline
  theme(axis.text.x=element_blank(), # remove the participant numbers 
        axis.ticks.x=element_blank()) + # remove x-axis ticks
  theme (plot.title = element_text(family = "sans", color=Grey, face="bold", size=22, hjust=0.5)) + #Edit font
  theme (axis.title = element_text(family = "sans", color=Grey, face="bold", size=22)) + #Edit font
  theme(panel.background = element_blank()) # blank out the background

p + annotate(geom = "text", x = (ControlMed$sequence1 + 3), y = 3, label = "Median difference", hjust = "left", size = 6) + #add text higlighting the median
  annotate(geom = "segment", y = 3, yend = 3, x = (TreatMed$sequence1 + 3), xend = (ControlMed$sequence1 - 3), 
           arrow = arrow(length = unit(3, "mm"))) + #this controls the arrow heads
  annotate(geom = "segment", y = 3, yend = 3, x = (ControlMed$sequence1 - 3), xend = (TreatMed$sequence1 + 3), 
           arrow = arrow(length = unit(3, "mm")))

#additional manipulation to reorder the dataset based on probability of exacerbation and creaion of a consecutive numbering (sequence2)
exacerbations_pred <- exacerbations_pred %>%
  arrange(prob) %>%
  add_column(sequence2 = 1:nrow(exacerbations))

#create two datasets (one for treat and one for control) and test the difference between them
treat <-exacerbations_pred %>%
  filter(RAND_TRT=="ICS/LABA")

control <- exacerbations_pred %>%
  filter(RAND_TRT=="LABA")

wilcox.test(treat$prob, control$prob,paired=FALSE)

#Median patient on treatment for ref line
TreatMed <- exacerbations_pred %>%
  filter(RAND_TRT=="ICS/LABA") %>%
  slice(whichmedian(prob))

#Median patient on control for ref line
ControlMed <- exacerbations_pred %>%
  filter(RAND_TRT=="LABA") %>%
  slice(whichmedian(prob))

#see above plot for comments
q <- exacerbations_pred %>%
  ggplot() +
  geom_col(aes(x=sequence2, y=prob, fill=RAND_TRT, color=RAND_TRT)) +
  ggtitle("Group level advantage for ICS/LABA patients was replicated at the patient \n level showing higher probability of Exacerbations for Control patients") + 
  ylab("Adjusted probability of >=1 exacerbation") +
  xlab("Patients, in order of adjusted exacerbation probability") + 
  geom_vline(aes(xintercept = sequence2), linetype='dashed', col = Darkblue, TreatMed)  +
  geom_vline(aes(xintercept = sequence2), linetype='dashed', col = Green, ControlMed)  +
  scale_fill_manual(values=c(Darkblue, Green), name = "Condition", labels = c("Treatment\n(ICS/LABA)", "Control\n(LABA)"))+
  scale_colour_manual(values=c(Darkblue, Green)) + 
  guides(color = FALSE, size = FALSE) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) + 
  theme (plot.title = element_text(family = "sans", color=Grey, face="bold", size=22, hjust=0.5)) + 
  theme (axis.title = element_text(family = "sans", color=Grey, face="bold", size=22)) + 
  theme(panel.background = element_blank()) 

 
q + annotate(geom = "text", x = (ControlMed$sequence2 + 3), y = 0.6, label = "Median difference", hjust = "left", size = 6) +
  annotate(geom = "segment", y = 0.6, yend = 0.6, x = (TreatMed$sequence2 + 3), xend = (ControlMed$sequence2 - 3), 
           arrow = arrow(length = unit(3, "mm"))) +
  annotate(geom = "segment", y = 0.6, yend = 0.6, x = (ControlMed$sequence2 - 3), xend = (TreatMed$sequence2 + 3), 
           arrow = arrow(length = unit(3, "mm")))


