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


### plot line chart
jitter = 0.1

png(filename ="ImproveFigure_V1_LineChart.png",width = 500, height = 480)

# add results for SZC group
plot(x = c(0,1,2,4,6,8) - jitter, y = dat2$serumK_exact[dat2$treat == "SZC group" & dat2$stat == "mean"],
     xlab = "Week",xaxt = "n",ylab = "Serum K (mEq/l)", type = "l",xlim = c(-0.5,8.5), ylim = c(0,6), col = "orange",lwd = 2)
points(x = c(0,1,2,4,6,8)-jitter, dat2$serumK_exact[dat2$treat == "SZC group" & dat2$stat == "mean"],
      col = "orange",pch = 16)
arrows(c(0,1,2,4,6,8)-jitter, dat2$serumK_exact[dat2$treat == "SZC group" & dat2$stat == "ulc"]
  ,c(0,1,2,4,6,8)-jitter, dat2$serumK_exact[dat2$treat == "SZC group" & dat2$stat == "llc"],
       angle=90, code=3, lwd=2, length=.025, col = "orange")

# add results for SPS group
points(x = c(0,1,2,4,6,8)+jitter, dat2$serumK_exact[dat2$treat == "SPS group" & dat2$stat == "mean"],
     type = "l",col = "blue",lwd = 2)

points(x = c(0,1,2,4,6,8)+jitter, dat2$serumK_exact[dat2$treat == "SPS group" & dat2$stat == "mean"],
       col = "blue",pch = 16)
arrows(c(0,1,2,4,6,8)+jitter, dat2$serumK_exact[dat2$treat == "SPS group" & dat2$stat == "ulc"]
       ,c(0,1,2,4,6,8)+jitter, dat2$serumK_exact[dat2$treat == "SPS group" & dat2$stat == "llc"],
       angle=90, code=3, lwd=2, length=.025, col = "blue")

abline(a = 5, b= 0, col = "black" )
text(2,4,"Normokalemia", col = "black")
text(2,6,"Hyperkalemia", col = "black")

# modify axes
axis(1, at = c(0,1,2,4,6,8),labels = c("Baseline","1","2","4","6","8"),xlim = c(-0.5,8))

# add legend and title

legend(x= 1,y=1,legend = c("SZC group (N = 60) (mean and 95% CI)","SPS group (N = 60) (mean and and 95% CI)"),col = c("orange","blue"),lwd = 2,bty = 'n')
title(expression(bold(paste(phantom("SZC"), " treatment resolves Hyperkalemia faster and more effective"))),line = 3)
title(main = expression(bold(paste("SZC",phantom(" treatment resolves Hyperkalemia faster and more effective")))), col.main = "orange", line = 3)

title(expression(paste("Mean serum potassium levels were significantly lower in the ",phantom("SZC")," compared to ",phantom("SPS"),
" group (p < 0.05)")), line = 2, cex.main = 0.8)
title(expression(paste(phantom("Mean serum potassium levels were significantly lower in the "),"SZC",
                       phantom(" compared to SPS group (p < 0.05)"))), line = 2, cex.main = 0.8,col.main = "orange")
title(expression(paste(phantom("Mean serum potassium levels were significantly lower in the SZC compared to "),"SPS",
                       phantom(" group (p < 0.05)"))), line = 2, cex.main = 0.8,col.main = "blue")

dev.off()
