library(adepro)
library(tidyverse)
library(lubridate)

#change to the working directory where the files were extracted
setwd("C:/Users/UHLMALO1/OneDrive - Novartis Pharma AG/My_Data/PSI_visualization/Wonderful_Wednesdays_Webinars/2020-09-09/Einreichungen/Final")

#Run this code to put the variables in the dataset provided into the correct format
AE <- read.csv("2020-08-12_ae_example_dataset.csv")%>%
  mutate(TRTSDT=ymd(rando_date)) %>%
  mutate(SUBJIDN = usubjid) %>%
  mutate(AEDECOD = aept) %>%
  mutate(TRT01A = arm) %>%
  mutate(TRT01A = if_else(TRT01A=="Intervention","Experimental","Control")) %>%
  mutate(LVDT = 365) %>%
  mutate(DTHDT = "NA") %>%
  mutate(SAFFN = 1) %>%
  mutate(AETRTEMN = 1) %>%
  mutate(AESERN = aesern) %>%
  mutate(AERELN = 1) %>%
  mutate(AE_Start = ymd(aestdat)) %>%
  mutate(AESTDY = AE_Start - TRTSDT + 1)%>%
  mutate(AEENDY = AESTDY + dur + 1) %>%
  mutate(AESEVN = aesevn) %>%
  mutate(TRTSDT=0) %>%
  mutate(AGE="NA") %>%
  mutate(SEX="NA") %>%
  mutate(REGION="NA") %>%
  select(c(SUBJIDN,AEDECOD,AESTDY, AEENDY,AESEVN,AETRTEMN,AESERN,AERELN, TRT01A, TRTSDT, LVDT, DTHDT,AGE, SEX, REGION, SAFFN))

#write this back to CSV
write.csv(AE,"AE.csv",row.names=FALSE)

#Run the following line to run the app.
#CSV must be loaded into the app
launch_adepro()
  