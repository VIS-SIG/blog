library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

### Simulation setup (default) ###

# Maximumnumberofcohorts (7) : Max number of cohorts allowed to enter the platform
# CohortInclusionRate (0.03) : Rate at which new cohorts enter the platform (higher levels indicate faster entry)
# TreatmentEfficacySetting (1) " Treatment effect scenario
# FinalCohortSampleSize (500)
# TypeofDataSharing : How is data shared across cohorts
# InterimFutilityStopping :  Binding early futility stopping yes/no

### Other variables ###

# ShortTermEndpointQuality : Correlation between surrogate interim and final endpoint

### Output (operating characteristics) ###

# PTP : per cohort power
# PTT1ER : per cohort type I error
# FWER 
# FWER_BA : ignores sims where only efficacious cohorts
# FDR : false discovery rate
# Disj_Power : disjoint power
# Disj_Power_BA : ignores sims where no efficacious cohorts 

### Other output
# Avg_Pat = Average number of participants enrolled in the trial

indata <- read_csv("./ExampleDataNASH.csv")
derived <- indata %>%
  mutate(PTT1ER_log = log10(PTT1ER)) %>%
  mutate(FWER_log = log10(FWER))

default <- derived %>%
  filter(TreatmentEfficacySetting == 1) %>%
  filter(Maximumnumberofcohorts == 7) %>% 
  filter(CohortInclusionRate == 0.03) %>%
  filter(FinalCohortSampleSize == 500)

### Q1: How can we best show the effect of sample size on the power/type 1 error?

ggplot(derived, aes(x=factor(FinalCohortSampleSize), y=Disj_Power)) +
  geom_violin() +
  geom_beeswarm(cex=0.3) 

ggplot(derived, aes(x=factor(FinalCohortSampleSize), y=FWER)) +
  # geom_violin() +
  geom_beeswarm(cex=0.3, method="square") +
  facet_wrap(~TypeofDataSharing)

### Q2: Does sample size affect PTP and Disj_Power in the same way? 
# How about in interaction with the type of data sharing?

temp <- derived %>%
  arrange(FinalCohortSampleSize) %>%
  select(FinalCohortSampleSize, PTP, Disj_Power, TypeofDataSharing) %>%
  filter(!is.na(PTP))

ss <- temp$FinalCohortSampleSize  
ptp <- temp$PTP
disj <- temp$Disj_Power
ds <- temp$TypeofDataSharing

ss2 <- rep(ss, 2)
ds2 <- rep(ds,2)

t <- c(ptp, disj)
t2 <- rep("PTP", length(ptp))
t3 <- rep("DISJ", length(disj))
t4 <- c(t2, t3)
new <- cbind(ss2, ds2, t, t4) %>%
  as.data.frame() %>%
  mutate(tn = as.numeric(t))

my_lgrey <-"#f0f0f0" 
my_dgrey <- "#636363"

cohort_text <- "all: At interim/final analysis, all SoC and backbone monotherapy data available from all cohorts; 
cohort: No sharing occurs; 
concurrent: At interim/final analysis, all SoC and backbone monotherapy data that was collected during the active enrollment time of the cohort under investigation are used; 
dynamic: Whenever in any cohort an interim or final analysis is performed, the degree of data sharing of SoC and backbone monotherapy data from
other cohorts increases with the homogeneity of the observed response rate of the respective arms"
ggplot(new, aes(x=ss2, y=tn, color=factor(t4))) +
  geom_beeswarm(cex=0.6, alpha=0.5, method="hex") +
  facet_wrap(~ds2) +
  scale_x_discrete("Cohort Sample Size") +
  scale_y_continuous("Power", limits=c(0, 1)) +
  scale_color_manual(" ", labels = c("Disjunctive", "Per Cohort"), values = c("#1b9e77", "#d95f02")) +
  labs(title = "Effect of Sample Size on Disjunctive and Per Cohort Power, by Data Sharing Category",
       caption = cohort_text , align="left") +
  theme(text=element_text(
    colour = my_dgrey,
    size = 14),
    panel.background=element_rect(fill="white"),
    panel.border=element_rect(fill=NA),
        panel.grid=element_line(colour = my_lgrey,
                                      linewidth = 0.5,
                                      linetype = 1),
        axis.line=element_line(colour = my_dgrey,
                               linewidth = 0.5,
                               linetype = 1),
        axis.text=element_text(
          colour = my_dgrey,
          size = 14),
        axis.title=element_text(
          colour = my_dgrey,
          size =14),
    plot.caption = element_text(hjust = 0, face= "italic")
        ) 


### Q3: What is the relationship between number of cohorts, cohort inclusion rate and power?

sum <- derived %>%
  group_by(Maximumnumberofcohorts, CohortInclusionRate) %>%
  filter(!is.na(Disj_Power)) %>%
  summarise(power_mn = mean(Disj_Power))

ggplot(sum, aes(x=factor(Maximumnumberofcohorts), y=power_mn, fill=factor(CohortInclusionRate))) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_discrete() 

### Q4: How can we investigate the effectiveness of early stopping for futility?

sum <- derived %>%
  group_by(InterimFutilityStopping) %>%
  filter(!is.na(Disj_Power)) %>%
  summarise(power_mn = mean(Disj_Power))

ggplot(sum, aes(x=factor(InterimFutilityStopping), y=power_mn)) +
  geom_bar(stat="identity") 

### Q5: What is the relationship between the quality of the surrogate endpoint and the average 
# number of patients enrolled? Is there an interaction with futility stopping and treatment effect scenario?

sum <- derived %>%
  group_by(ShortTermEndpointQuality, InterimFutilityStopping, TreatmentEfficacySetting) %>%
  filter(!is.na(Avg_Pat)) %>%
  summarise(mn = mean(Avg_Pat))

cohort_text <- "_______________________________________________________________________________________________________________________________________________________________________________________________________________________
1: Backbone monotherapy superior to SoC, add-on monotherapy has 50:50 chance to be superior to SoC; in case add-on monotherapy not superior to SoC, combination therapy as effective as backbone monotherapy, otherwise combination therapy significantly
better than monotherapies; 
2: Backbone monotherapy superior to SoC, but add-on monotherapy not superior to SoC and combination therapy not better than backbone monotherapy
3: Backbone monotherapy superior to SoC and combination therapy superior to backbone monotherapy, but add-on monotherapy not superior to SoC 
4: Backbone monotherapy superior to SoC and combination terapy superior to backbone monotherapy (increased combination treatment effect compared to setting 4), but add-on monotherapy not superior to SoC 
5: Both monotherapies are superior to SoC, but combination therapy is not better than monotherapies
6: Both monotherapies are superior to SoC and combination therapy is better than monotherapies 
7: Both monotherapies are superior to SoC and combination therapy is superior to monotherapies (increased combination treatment effect compared to setting 7) 
8: Global null hypothesis; 9: Global null hypothesis with higher response rates 
10: Backbone monotherapy superior to SoC, add-on monotherapy has 50:50 chance to be superior to SoC; combination therapy interaction effect can either be antagonistic/non-existent, additive or synergistic (with equal probabilities) 
11: Time-trend null scenario; every new cohort (first cohort c = 1, second cohort c = 2, ...) will have SoC response rate that is by 3%-points higher than that of the previous cohort 
12: Time-trend scenario, whereby monotherapies superior to SoC and combination therapy superior to monotherapies; every new cohort (first cohort c = 1, second cohort c = 2, ...) will have SoC response rate that is by 3%-points higher than that of the previous cohort 
13: Analogous to setting 7, but SoC response rate is 20% 
14: Analogous to setting 8, but SoC response rate is 20%"

ggplot(sum, aes(x=factor(ShortTermEndpointQuality), y=mn, fill=factor(InterimFutilityStopping))) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual("Futility Stopping? ", labels = c("No", "Yes"), 
                    values = c("#1b9e77", "#d95f02")) +
  scale_x_discrete("Short Term Endpoint Quality") +
  scale_y_continuous("Average Number of Patients Enrolled") +
  facet_wrap(~TreatmentEfficacySetting) +
  labs(title = "Effect of Short Term Endpoint Quality on Average Number of Patients Enrolled, by Treatment Effect Scenario",
       caption = cohort_text , align="left") +
  theme(text=element_text(
    colour = my_dgrey,
    size = 14),
    panel.background=element_rect(fill="white"),
    panel.border=element_rect(fill=NA),
    panel.grid=element_line(colour = my_lgrey,
                            linewidth = 0.5,
                            linetype = 1),
    axis.line=element_line(colour = my_dgrey,
                           linewidth = 0.5,
                           linetype = 1),
    axis.text=element_text(
      colour = my_dgrey,
      size = 14),
    axis.title=element_text(
      colour = my_dgrey,
      size =14),
    plot.caption = element_text(hjust = 0, face= "italic")
  ) 
