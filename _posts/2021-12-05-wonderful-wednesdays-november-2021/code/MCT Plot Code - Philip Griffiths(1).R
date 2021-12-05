

library(tidyverse)
if(!require(polycor)){
  install.packages("polycor")
  library(polycor)
}

if(!require(ggridges)){
  install.packages("ggridges")
  library(ggridges)
}


if(!require(grid)){
  install.packages("grid")
  library(grid)
}

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
Green100a <- rgb(67, 176, 42, alpha = 0.5 * 255, maxColorValue = 255)
Green50 <- "#a1d794"
Purple100 <-"#830065"
Purple100a <- rgb(131, 0, 101, alpha = 0.5 * 255, maxColorValue = 255)
Purple50 <- "#c17fb2"

#Set data and results areas up
sourcedata <- "C:/Users/phili/Documents/WonWed/MCT/sourcedata/"
outputdata <-"C:/Users/phili/Documents/WonWed/MCT/outputdata/"
tables <- "C:/Users/phili/Documents/WonWed/MCT/tables/"
figures <- "C:/Users/phili/Documents/WonWed/MCT/figures/"

setwd(sourcedata)
dat <- read_csv("WWW_example_minimal_clinical_improvement.csv")

glimpse(dat)

dat$USUBJID <- c(1:nrow(dat)) #MAKE A NEW SUBJECT ID COLUMN

dat <- dat %>%
  rename("Time1" = `total score baseline`, "Time2" = `total score follow up`, "CGI" =`CGI-I`) %>% #RENAME COLUMNS
  mutate(CHANGE = Time2 - Time1) # MAKE CHANGE SCORES. NEGATIVE SCORE IS IMPROVEMENT
dat$CGI <- as.factor(dat$CGI)  
  
corr <- round(polyserial(dat$CHANGE, dat$CGI),3)

#CREATE A PLOT SHOWING TH CORRELATION BETWEEN THE ANCHOR AND PRO MEASURE
p <- ggplot(dat, aes(x=CGI, y=CHANGE)) +
  geom_jitter(width = 0.15, height = 0.5,alpha = 0.6) + 
  ggtitle("The CGI-I is Appropriate for Meaningful Change Derivation", subtitle = "A correlation between an anchor and a COA change score of >0.37 is recommended")+      
  labs(x="CGI-I Score at Time 2", y = "COA Change Score from Baseline")+
  scale_y_continuous(breaks=-40:+40*10) +         # SET TICK EVERY 10
  scale_x_discrete(labels=c("1\nVery much improved", "2", "3","4\nNo change", "5", "6","7\nVery much worse")) +
  theme(panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        plot.title = element_text(size=22, face="bold"),
        plot.subtitle = element_text(size=16, face="bold")) +
  annotate("text", x=6, y=-35, label=paste("polyserial correlation = ", corr), size=4, hjust=0)
p

#### MCT PLOT

#FIND LOWER TAIL OF THE "NO CHANGE GROUP"

dat_tail <- dat %>%
  filter(CGI == 4) %>%
  summarise(quantile = scales::percent(c(0.025)),
           value = quantile(CHANGE, c(0.025)))

glimpse(dat_tail)

text_grob <- textGrob(paste(round(dat_tail$value,1)), gp=gpar(fontsize=16, col = Green100)) #Needed to draw on axis 


ridge <- ggplot(dat, aes(x = CHANGE, y = CGI, fill =  ifelse(..x..< dat_tail$value, "lower", "rest"))) +
  stat_density_ridges(geom = "density_ridges_gradient",quantiles = c(0.025)) +
  scale_fill_manual(
  values = c(Green100a, Purple100a),
  ) +
  ggtitle(paste("A threshold of ",round(dat_tail$value)," may be an appropriate patient-level improvement threshold" ), subtitle = 'Derived using the value of the lower tail of the "No change" Group')+      
  labs(x="PRO Change Score from Baseline")+
  scale_x_continuous(breaks=-40:+40*20) +         # SET TICK EVERY 10
  scale_y_discrete(labels=c("Very much improved - 1", "2", "3","No change - 4", "5", "6","Very much worse - 7")) +
  theme(panel.background = element_blank(), axis.line = element_blank(),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        legend.position = "none",
        plot.title = element_text(size=22, face="bold"),
        plot.subtitle = element_text(size=16, face="bold")) +
  annotate("segment", x=dat_tail$value, xend=dat_tail$value, y=4, yend = 0, colour = "black", linetype = "dashed") +
  annotation_custom(text_grob, xmin=dat_tail$value,xmax=dat_tail$value,ymin=-0.27,ymax=-0.27) +
  coord_cartesian(clip = "off")
    

ridge

