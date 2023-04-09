library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(readxl)
library(cowplot)
library(gridExtra)
library(patchwork)
library(reshape2)
library(grid)

X <- read.csv(url("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2022/2022-09-14/ww%20eortc%20qlq-c30.csv"))
X[X==0] <- NA;
X1 <- X[1:(nrow(X)/2), ]; # standard
Xe <- X[(nrow(X)/2+1):nrow(X),]; # experimental

X1 <-  X1 %>%
  group_by(USUBJID) %>%
  mutate(QL_diff = QL - QL[1], PF_diff = PF - PF[1], RF_diff = RF - RF[1], EF_diff = EF - EF[1])
X1_a <- X1 #save for second plot

Xe <-  Xe %>%
  group_by(USUBJID) %>%
  mutate(QL_diff = QL - QL[1], PF_diff = PF - PF[1], RF_diff = RF - RF[1], EF_diff = EF - EF[1])
Xe_a <- Xe #save for second plot

#need to replace NA's with 0's again when finding the mean
X1$QL_diff[is.na(X1$QL_diff)] <- 0 #We will only use QL in the plot below, but can use any sub-scale.
X1$PF_diff[is.na(X1$PF_diff)] <- 0
X1$RF_diff[is.na(X1$RF_diff)] <- 0
X1$EF_diff[is.na(X1$EF_diff)] <- 0
Xe$QL_diff[is.na(Xe$QL_diff)] <- 0
Xe$PF_diff[is.na(Xe$PF_diff)] <- 0
Xe$RF_diff[is.na(Xe$RF_diff)] <- 0
Xe$EF_diff[is.na(Xe$EF_diff)] <- 0

A <- aggregate(X1[, 20:23], list(X1$AVISITN), mean)
colnames(A) <- c("AVISITN", "QL_mean", "PF_mean", "RF_mean", "EF_mean")
B <- aggregate(X1[, 20:23], list(X1$AVISITN), sd)
B <- B[, 2:ncol(B)]/sqrt(100)
colnames(B) <- c("QL_se", "PF_se", "RF_se", "EF_se")
A <- data.frame(A, B)

Ae <- aggregate(Xe[, 20:23], list(Xe$AVISITN), mean)
colnames(Ae) <- c("AVISITN", "QL_mean", "PF_mean", "RF_mean", "EF_mean")
Be <- aggregate(Xe[, 20:23], list(Xe$AVISITN), sd)
Be <- Be[, 2:ncol(Be)]/sqrt(100)
colnames(Be) <- c("QL_se", "PF_se", "RF_se", "EF_se")
Ae <- data.frame(Ae, Be)

library(plyr) #call this library here, not before.

A = A[c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,9),] 
A[nrow(A)+nrow(X1)-nrow(A),] <- NA;
A1 <- cbind(X1_a, A);

Ae = Ae[c(1,2,3,4,5,6,7,8,10,11,12,13,14,15,16,17,9),] 
Ae[nrow(Ae)+nrow(Xe)-nrow(Ae),] <- NA;
A1e <- cbind(Xe_a, Ae);

X1count <- X1[complete.cases(X1$QL),] #To count dropout rate/N (number) per visit
Xecount <- Xe[complete.cases(Xe$QL),]
X1count <- count(X1count, "AVISITN")
Xecount <- count(Xecount, "AVISITN")

#Standard treatment plot
GG1<-ggplot(data=A1, aes(x=AVISITN...19, y=QL_diff, group=USUBJID, color="red")) +
  geom_line(size=1, alpha=0.15, color="tomato")+guides(color = "none")+
  xlab("Visit Number")+ylab("Change from Baseline")+
  scale_x_continuous(breaks=0:17)+
  scale_y_continuous(breaks=seq(-55, 55, by=10))+
  geom_hline(yintercept=c(-6,0,6), linetype='dotted', col = 'black')+
  geom_line(aes(x=AVISITN...24, y = QL_mean, colour = "Standard"),size=1) + 
  geom_point(aes(x=AVISITN...24, y = QL_mean, colour = "Standard"),size=2)+
  geom_errorbar(aes(ymin=QL_mean-QL_se, ymax=QL_mean+QL_se, colour = "Standard"),width=25, position=position_dodge(0))+
  scale_colour_manual(values="violet")+
  annotate(geom = "text", x = 1:17, y = -68, label = X1count$freq, size = 3)+
  coord_cartesian(ylim = c(-55, 55), expand = FALSE, clip = "off")+theme_bw()

GG1 <- GG1+theme(axis.text=element_text(size=8), axis.title=element_text(size=10),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())

#Experimental treatment plot
GG2<-ggplot(data=A1e, aes(x=AVISITN...19, y=QL_diff, group=USUBJID, color="red")) +
  geom_line(size=1, alpha=0.15, color="dodgerblue")+guides(color = "none")+
  xlab("Visit Number")+ylab("")+
  scale_x_continuous(breaks=0:17)+
  scale_y_continuous(breaks=seq(-55, 55, by=10))+
  geom_hline(yintercept=c(-6,0,6), linetype='dotted', col = 'black')+
  geom_line(aes(x=AVISITN...24, y = QL_mean, colour = "Experimental"),size=1) + 
  geom_point(aes(x=AVISITN...24, y = QL_mean, colour = "Experimental"),size=2)+
  geom_errorbar(aes(ymin=QL_mean-QL_se, ymax=QL_mean+QL_se, colour = "Experimental"),width=25, position=position_dodge(0))+
  scale_colour_manual(values="aquamarine4")+
  annotate(geom = "text", x = 1:17, y = -68, label = Xecount$freq, size = 3)+
  coord_cartesian(ylim = c(-55, 55), expand = FALSE, clip = "off")+theme_bw()

GG2 <- GG2+theme(axis.text=element_text(size=8), axis.title=element_text(size=10),panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_blank())

#Combining two plots
p1 <- plot_grid(GG1, GG2, labels=c("Standard", "Experimental"),hjust = -1.0, vjust = 0.5,label_size = 8)
title <- ggdraw() + draw_label("This looks nothing like real QLQ-C30 Global Heath Status data, real individual patient data has much more variability", size=15)
subtitle <- ggdraw() + draw_label("Higher Values & Positive Change from Baseline values for QLQ-C30 Global Heath Status indicate better Quality of Life", size=10)
footnote <- ggdraw() + draw_label("Individual patient data with arithmetic mean (+/- SE) overlaid. \n Horizontal Reference lines indicate the Minimal Important Change (MIC), at the group level, of -6 and 6 based on the 2012 Cocks publication.\n Line opacity is proportional to the number of patients, with more transparent lines indicating less patients. Total number of patients remaining per visit is given below the x-axis.", fontface='italic', size=10)

#plot results
plot_grid(title, subtitle, p1, footnote, ncol=1, rel_heights=c(0.1, 0.1, 1, 0.4, -0.1))

#By Samiar Ashtiany