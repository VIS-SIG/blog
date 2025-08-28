##################################################################################################################
## Program:   WW_GCdose_APR2025.R                                                              		
##                                                                 						
## Study:     None                                             						
##                                                                 						
## Purpose:   Wonderful Wednesday PSI challenge 
##                                                                 						
## Inputs:    WW_GCdose.csv
##
## Outputs:   StackedBarchartGCdoseGroups.png
##                                                                                             			
## Revision                                                                                                     
## History:      Version     Date        Author                  Description                                    
##                -------     ---------   -------------------     -------------------------------------------   
##                    1.0     6MAY2025   Baerbel Maus           Initial version 
##################################################################################################################


## cleanup
rm(list=ls())

# projectRoot <- "C:/Users/"

library(ggplot2)
library(tidyverse)

# dat <- read.csv(paste0(projectRoot,"WW_GCdose.csv"))
dat <- read.csv("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/refs/heads/master/data/2025/2025-04-09/WW_GCdose.csv")

datWeeks <- unique(dat[,c("SUBJID","AVISITN","AVISIT","AVAL2")])

plot(NA,NA,xlab="Days", ylab="	Daily glucocorticoid dose (mg/kg/day)",xlim=c(0, 56), ylim=c(0, 40))
for (subj in unique(dat$SUBJID)[1:10]){
  lines(dat[dat$SUBJID == subj,"ASTDY"],dat[dat$SUBJID == subj,"AVAL1"])
}

plot(NA,NA,xlab="Weeks", ylab="Weekly glucocorticoid dose (mg/kg/day)",xlim=c(0,8), ylim=c(0, 40))
for (subj in unique(dat$SUBJID)[1:10]){
  lines(dat[dat$SUBJID == subj,"ASTDY"],dat[dat$SUBJID == subj,"AVAL2"])
  abline(a = 1,b= 0, col = "blue")
  abline(a = 0.5,b= 0, col = "blue")
  abline(a = 0.2,b= 0, col = "blue")
}

# Calculate the percentage of subjects per day with GC values between 0.2, 0.5, 1 or above 1
percDay <- matrix(NA, nrow = 57,ncol = 5)
j = 0
for (i in unique(dat$ASTDY)){
  j = j + 1
  datDay <- dat[dat$ASTDY == i,]
  percDay[j,1] <- i
  percDay[j,2] <- sum(datDay$AVAL1 < 0.2)/nrow(datDay) *100
  percDay[j,3] <- sum(0.2 <= datDay$AVAL1 & datDay$AVAL1 < 0.5)/nrow(datDay)*100
  percDay[j,4] <- sum(0.5 <= datDay$AVAL1 & datDay$AVAL1 < 1)/nrow(datDay)*100
  percDay[j,5] <- sum(datDay$AVAL1 >= 1)/nrow(datDay)*100
  
}

# transform data to long format as needed for stacked barchart
percDay2 <- as.data.frame(percDay)
data_long <- percDay2 %>%
  gather(key = "GClevel", value = "Value",-1)

# Create the stacked bar chart
png(filename ="StackedBarchartGCdoseGroups.png")
ggplot(data_long, aes(x = V1, y = Value, fill = GClevel)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("V5" =  "#08519C", "V4" = "#4292C6","V3" = "#9ECAE1",V2 = "#DEEBF7"),
                    labels = c("V5" = ">= 1", "V4" = "0.5-1","V3" = "0.2-0.5","V2" = "<0.2")) +
  labs(x = "Days", y = "Percentage of Subjects") +
  labs(fill = "GC doses (mg/kg/day)")+
  scale_x_continuous(
    breaks = seq(0,60,10),
    limits = c(0, 60) # Specify the positions of the ticks
  ) +
  scale_y_continuous(
    breaks = seq(0,100,10),limits = c(0, 101)  # Specify the positions of the ticks
  ) +
  theme_minimal() +
  theme(
    # panel.background = element_blank(), # Remove the gray background
    # panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    
    axis.ticks.y.right = element_blank(),  # Remove ticks on the right
    axis.ticks.x.top = element_blank(),    # Remove ticks on the top
    axis.line.y.right = element_blank(),   # Remove axis line on the right
    axis.line.x.top = element_blank()      # Remove axis line on the top
  )
dev.off() 
 