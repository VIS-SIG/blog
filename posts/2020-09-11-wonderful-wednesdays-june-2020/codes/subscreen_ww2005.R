###############################################################################
# Bayer AG
# Study            :
# Proj/Subst/GIAD  :
###############################################################################
# Name of program #############################################################
# Name             : SGS_WW2005.R
#
# Purpose          : Run subgroup screening  
# Programming Spec :
# Validation Level : 1 - Verification by Review
# R Version        : 4.0.0 (64 bit) on simulation server
###############################################################################
# Pre-conditions   : data source: ALLBUS dataset 2014/subset
# Post-conditions  :
# Comments         :
###############################################################################
# Author(s)        : Bodo Kirsch  03JUN2020
# Reference prog   : SGS
###############################################################################
# Changed by       : 
# Reason           :
###############################################################################



### delete workspace
rm(list=ls())


### AutoSub and load libraries
#setwd("c:/temp")


library(shiny)
library(subscreen)


### import data
D=read.csv2(file="Data_wW2005.csv", sep=",", dec=".", header=TRUE)


auwe <- function(D){
  
    # --- satisfaction mean ----------------------------------------------------------------
  
      satisfact  <- round(mean(D$satisfaction, na.rm = TRUE), 2)   # TRUE:  NA values stripped before the computation proceeds
      
      number.of.subjects     <- sum(!is.na(D$satisfaction))
  
    return(data.frame(number.of.subjects, satisfact))
}


HH <- subscreencalc(data=D,
           eval_function = auwe,
           endpoints   = "satisfaction",  
           treat       = "",            
           subjectid   = "ID",
           factors     = c("gender", "employed", "smoker", "graduat", "graduat_f", "graduat_m", 
                           "high_grad", "high_grad_f", "high_grad_m", "age", "w_hours", "todoctor", "bmi", "income") ,
           min_comb    = 1,
           max_comb    = 3,
           verbose     = T)



#write.csv(HH$sge, file = "Data_H.csv", na="")    

subscreenshow(HH, 
              host = "0.0.0.0", port = 1234)
