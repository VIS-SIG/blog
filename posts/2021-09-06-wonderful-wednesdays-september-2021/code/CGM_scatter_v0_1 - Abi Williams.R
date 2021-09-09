## Continuous glucose monitoring data challenge

## Load packages and colours ----
library(tidyverse)

Orange <- "#EF7F04"
Green <- "#68B937"
Blue <- "#00A6CB"
Grey <- "#4E5053"
Darkblue <- "#003569"
Yellow <- "#FFBB2D"

## Read in dataset ----

data <- read_csv('simulated_data.csv')

data$VISITNUM <- as.character(data$VISITNUM)
data$TREATMENT <- as.factor(data$TREATMENT)

## Set up data frame for changing visitnum to visit name

visits <- data.frame(VISITNUM = c('3', '17', '21'),
                     Visit = c('Baseline', 'Week 26', 'Week 52'))

## join it on

dataa <- data %>% 
  full_join(visits)

levels(data$TREATMENT)

## reorder factor for treatment


dataa$Treatment <- factor(dataa$TREATMENT, levels = c("Rx high", "Rx medium", "Rx low", "SOC"))

## Add in mean and sd for each patient and visit combination ----

data1 <- dataa %>% 
  group_by(SUBJID, Visit, Treatment) %>% 
  summarise(mean_cgmsim = mean(Simulated_CGMValue), sd_cgmsim = sd(Simulated_CGMValue), 
            mean_cgm = mean(Original_CGMValue), sd_cgm = sd(Simulated_CGMValue),
            med_cgm = median(Original_CGMValue), iqr_cgm = IQR(Original_CGMValue),
            med_cgmsim = median(Simulated_CGMValue), iqr_cgmsim = IQR(Simulated_CGMValue))




# plotdata <- ggplot(data= data1, aes(x=mean_cgmsim, y = sd_cgmsim, colour= TREATMENT, shape= Visit))
# plotdata <- ggplot(data= data1, aes(x=mean_cgm, y = sd_cgm, colour= TREATMENT, shape= VISITNUM))
plotdata <- ggplot(data= data1, aes(x=mean_cgmsim, y = sd_cgmsim, colour= Treatment))
# plotdata <- ggplot(data= data1, aes(x=med_cgmsim, y = iqr_cgmsim, colour= TREATMENT))


plotdata + geom_point() +
  ggtitle('Medium treatment dose leads to glucose levels within a safe range,') +
  xlab('Mean glucose levels per patient (mg/dL)') +
  ylab('Variablity of glucose measurements per patient (Standard Deviation)') +
  scale_colour_manual(values = c(Green, Blue, Darkblue, Orange)) +
  geom_vline(aes(xintercept=140), linetype=2, colour=Grey) +
  facet_wrap(vars(Visit),labeller = labeller(Visit = label_wrap_gen(55))) +
  labs(caption = "Glucose levels between 72 and 140 mg/dL are considered to indicate good glycemic control",
       subtitle = "and decreased variability compared to SOC and other treatment doses") +
  theme_bw() +
  theme(panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        # legend.position="none",
        legend.title = element_text(size=14),
        legend.text= element_text(size=12),
        plot.title = element_text(size=18, face="bold"),
        plot.subtitle = element_text(size=14, face="bold"))
