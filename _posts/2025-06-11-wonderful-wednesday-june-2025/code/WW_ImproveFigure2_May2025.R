##################################################################################################################
## Program:   WW_ImproveFigure_May2025.R                                                              		
##                                                                 						
## Study:     None                                             						
##                                                                 						
## Purpose:   Wonderful Wednesday PSI challenge May 2025
##                                                                 						
## Inputs:    fig2data.csv
##
## Outputs:   ImproveFigure_V1_LineChart.png
##            ImproveFigure_V2_change.png
##
## Revision                                                                                                     
## History:      Version     Date        Author                  Description                                    
##                -------     ---------   -------------------     -------------------------------------------   
##                    1.0     23MAY2025   Baerbel Maus           Initial version 
##################################################################################################################


## cleanup
rm(list=ls())

library(ggplot2)
# prepare data
projectRoot <- "C:/Temp/"
setwd(projectRoot)

# dat <- read.csv(paste0(projectRoot,"fig2data.csv"))
dat <- read.csv("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/refs/heads/master/data/2025/2025-05-14/fig2data.csv")

dat2 <- dat[,c(-1,-4)] # remove unnecessary columns
names(dat2)[c(1,2)] <- c("visitNum","serumK_exact")

### create barchart with changes, add lines for 2 weeks and 6 weeks

datChange <- cbind(dat2,dat2$serumK - 5)
names(datChange) <- c(names(dat2),"change")
datChange$visitNum2 <- c()
datChange$visitNum2[datChange$visit == "Baseline"] <- 0
datChange$visitNum2[datChange$visit == "1st week"] <- 1
datChange$visitNum2[datChange$visit == "2nd week"] <- 2
datChange$visitNum2[datChange$visit == "4th week"] <- 4
datChange$visitNum2[datChange$visit == "6th week"] <- 6
datChange$visitNum2[datChange$visit == "8th week"] <- 8

png(filename ="ImproveFigure_V2_change.png")
ggplot(datChange[datChange$stat == "mean",], aes(x = visitNum2, y = change, fill = treat)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(name = "Group",values = c("SPS group" =  "blue", "SZC group" =  "orange")) +
  labs(title = "SZC treatment resolves Hyperkalemia (> 5 mEq/l) faster and more effective", x = "Week", y = "Serum K (mEq/l)") +
  scale_x_continuous(
    breaks = c(0,1,2,4,6,8),
    labels = c("Baseline","1","2","4","6","8"),
    minor_breaks = NULL,
    limits = c(-0.5,8.5) 
  )  +
  scale_y_continuous(
    breaks = seq(-1,1,0.25),
    labels = seq(4,6,0.25),
    minor_breaks = NULL,
    limits = c(-1, 1) 
  ) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 1) +
  geom_vline(xintercept = 2, color = "orange", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 2.5, y = -0.5, label = "SZC group reaches Normokalemia at 2 weeks", colour = "orange") +
  geom_vline(xintercept = 6, color = "blue", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 5.5, y = -0.75, label = "SPS group reaches Normokalemia at 6 weeks", colour = "blue") +
  theme_minimal()
  

dev.off()