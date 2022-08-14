#/*****************************************************************************\
#         O
#        /
#   O---O     _  _ _  _ _  _  _|
#        \ \/(/_| (_|| | |(/_(_|
#         O
# ______________________________________________________________________________          
# Program: wwrank.R
# Purpose: Creates visualisations for treatment ranks
#_______________________________________________________________________________                            

#/*****************************************************************************\

# Clear environment and set working directory
rm(list = ls())
setwd("~/")

# Read in packages
library(tibble)
require(ggplot2)
require(gganimate)
library(tidyverse)
library(dplyr)
library("ggpubr")
library(modelr)
library(broom)
library(grid)
library(gridExtra)
library("transformr")
library(ggtext)
library('ggforce')
library("readxl")
library(stringr)

#data processing

# Set working directory using setwd()
rnk1 <- read_excel("rankdata.xlsx", sheet="rankdata")
rnk2<-tibble(rnk1)
rnk3 <- mutate(rnk2, rowid_to_column(rnk2, "ID"))

rnk4 <- rnk3 %>%
  pivot_longer(!c(Treatment,ID), names_to = "ranks", values_to = "prop")
rnk5 <- mutate(rnk4, ranker=str_replace_all(string=ranks, pattern="Rank", replacement="")) 
rnk6 <- mutate(rnk5, rnk=as.numeric(ranker), r1=rnk-0.5, r2=rnk+0.5, p2=prop*12, rnkr=1/rnk, clr=letters[4])
rnk <- rnk6 %>% group_by(Treatment) %>% mutate(cp=cumsum(prop))

#drawing

p<-
ggplot() +
  ggforce::geom_link2(data = rnk,
                      aes(x = rnk, y = Treatment, color=clr, alpha = prop, size=5)) +
  #scale_colour_manual(values = "green") +
  theme(legend.position="none") + 
  scale_x_discrete(name ="Rank", limits=c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))
p
pdf("wwranklp.pdf")
print(p)     
dev.off()
png("wwranklp.png")
print(p)     
dev.off()

q<-
ggplot() +
  ggforce::geom_link2(data = rnk,
                      aes(x = rnk, y = Treatment, colour='green', alpha = cp, size=5))  +
  #scale_colour_manual(values = "green") +
  theme(legend.position="none") + 
  scale_x_discrete(name ="Rank", limits=c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'))
q
png("wwranklpc.png")
print(q)     
dev.off() 


r1 <- 
  ggplot(data = rnk, aes(x = factor(1), group=factor(rnkr), fill=p2)) +
  geom_bar(width = 1) +
  scale_fill_continuous(low='white', high='green1', guide="none") +
  facet_wrap(~Treatment, ncol=4) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank())  
r1
r <- r1 + coord_polar(theta = "x")
r
png("wwrankcp.png")
print(r)     
dev.off() 

s1 <- ggplot(data = rnk, aes(x = factor(1), group=factor(rnkr), fill=cp)) +
  geom_bar(width = 1) +
  scale_fill_continuous(low='white', high='green1', guide="none") +
  facet_wrap(~Treatment, ncol=4) + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())
s1
s<- s1 + coord_polar(theta = "x")
s
png("wwrankcpc.png")
print(s)     
dev.off() 
