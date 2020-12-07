
# EC subscreen presentation

rm(list=ls())
setwd("o:/1_Global_Biostatistics/Biostatistics Innovation Center/BIC Project - Subgroup Analyses/Screening/_archive/WW/")

suppressPackageStartupMessages(library(dplyr)) 
library(survival)
library(subscreen)

A=read.csv(file="BIG_DATA_PSI_WW_DEC2020.csv", sep=",", dec=".", na.strings = "", header=TRUE)

A$CHG <- as.numeric(A$CHG)
A$PCHG <- as.numeric(A$PCHG)
A$AVALCAT1N <- as.numeric(A$AVALCAT1N)
A$AVALCAT2N <- as.numeric(A$AVALCAT2N)

A %>% group_by(Base_SBP) %>% count()

#dychotomisation/categorisation of continuous variables

A$HEIGHT <- as.numeric(A$HEIGHT)
hei.median <- median(A$HEIGHT, na.rm=TRUE)
A$Height_gr[A$HEIGHT <  171] <- "A) < 171 cm (median)"
A$Height_gr[A$HEIGHT >= 171] <- "B) >=171 cm"
A$Height_gr[is.na(A$HEIGHT)] <- "C) missing"

A$WEIGHT <- as.numeric(A$WEIGHT)
wei.median <- median(A$WEIGHT, na.rm=TRUE)
A$Weight_gr[A$WEIGHT <  60] <- "A) < 60 kg"
A$Weight_gr[A$WEIGHT >= 60] <- "B) 60-90 cm"
A$Weight_gr[A$WEIGHT >  90] <- "C) > 90 cm"
A$Weight_gr[is.na(A$WEIGHT)] <- "D) missing"

A$BMI <- as.numeric(A$BMI)
bmi.median <- median(A$BMI, na.rm=TRUE)
A$BMI_gr[A$BMI <  18.5] <- "A) < 18.5 (WHO)"
A$BMI_gr[A$BMI >= 18.5] <- "B) 18.5-24.9 (WHO)"
A$BMI_gr[A$BMI >=  25]  <- "C) 25-29.9 (WHO)"
A$BMI_gr[A$BMI >=  30]  <- "D) >=30 (WHO)"
A$BMI_gr[is.na(A$BMI)]  <- "E) missing"

A$Age_gr[A$AGE <  30] <- "A) < 30 years"
A$Age_gr[A$AGE >= 30] <- "B) 30-59 cm"
A$Age_gr[A$AGE >= 60] <- "C) 60-75 cm"
A$Age_gr[A$AGE >  75] <- "D) > 75 cm"

A$CHD10R1[A$CHD10R1=="High (>20%)"] <- "Mx High (>20%)"

A$ALBSI <- as.numeric(A$ALBSI)
median(A$ALBSI, na.rm=TRUE)
A$ALB_med[A$ALBSI <  45] <- "A) <  45 (median)"
A$ALB_med[A$ALBSI >= 45] <- "B) >= 45 (median)"
A$ALB_med[is.na(A$ALBSI)]  <- "C) missing"

A$BASOSI <- as.numeric(A$BASOSI)
median(A$BASOSI, na.rm=TRUE)
A$BASO_med[A$BASOSI <  0.02] <- "A) <  0.02 (median)"
A$BASO_med[A$BASOSI >= 0.02] <- "B) >= 0.02 (median)"
A$BASO_med[is.na(A$BASOSI)]  <- "C) missing"

A$BICARSI <- as.numeric(A$BICARSI)
median(A$BICARSI, na.rm=TRUE)
A$BICAR_med[A$BICARSI <  24] <- "A) <  24 (median)"
A$BICAR_med[A$BICARSI >= 24] <- "B) >= 24 (median)"
A$BICAR_med[is.na(A$BICARSI)]  <- "C) missing"

A$BILISI <- as.numeric(A$BILISI)
median(A$BILISI, na.rm=TRUE)
A$BILI_med[A$BILISI <  7] <- "A) <  7 (median)"
A$BILI_med[A$BILISI >= 7] <- "B) >= 7 (median)"
A$BILI_med[is.na(A$BILISI)]  <- "C) missing"

A$BUNSI <- as.numeric(A$BUNSI)
median(A$BUNSI, na.rm=TRUE)
A$BUN_med[A$BUNSI <  4.64] <- "A) <  4.64 (median)"
A$BUN_med[A$BUNSI >= 4.64] <- "B) >= 4.64 (median)"
A$BUN_med[is.na(A$BUNSI)]  <- "C) missing"

A$CASI <- as.numeric(A$CASI)
median(A$CASI, na.rm=TRUE)
A$CA_med[A$CASI <  2.38] <- "A) <  2.38 (median)"
A$CA_med[A$CASI >= 2.38] <- "B) >= 2.38 (median)"
A$CA_med[is.na(A$CASI)]  <- "C) missing"

A$CHOL_HDL <- as.numeric(A$CHOL_HDL)
median(A$CHOL_HDL, na.rm=TRUE)
A$CHOL_HDL_med[A$CHOL_HDL <  4] <- "A) <  4 (median)"
A$CHOL_HDL_med[A$CHOL_HDL >= 4] <- "B) >= 4 (median)"
A$CHOL_HDL_med[is.na(A$CHOL_HDL)]  <- "C) missing"

A$CHOLSI <- as.numeric(A$CHOLSI)
median(A$CHOLSI, na.rm=TRUE)
A$CHOL_med[A$CHOLSI <  4.95] <- "A) <  4.95 (median)"
A$CHOL_med[A$CHOLSI >= 4.95] <- "B) >= 4.95 (median)"
A$CHOL_med[is.na(A$CHOLSI)]  <- "C) missing"

A$CREATSI <- as.numeric(A$CREATSI)
median(A$CREATSI, na.rm=TRUE)
A$CREAT_med[A$CREATSI <  77] <- "A) <  77 (median)"
A$CREAT_med[A$CREATSI >= 77] <- "B) >= 77 (median)"
A$CREAT_med[is.na(A$CREATSI)]  <- "C) missing"

A$EOSLESI <- as.numeric(A$EOSLESI)
median(A$EOSLESI, na.rm=TRUE)
A$EOSLE_med[A$EOSLESI <  2] <- "A) <  2 (median)"
A$EOSLE_med[A$EOSLESI >= 2] <- "B) >= 2 (median)"
A$EOSLE_med[is.na(A$EOSLESI)]  <- "C) missing"

A$EOSSI <- as.numeric(A$EOSSI)
median(A$EOSSI, na.rm=TRUE)
A$EOS_med[A$EOSSI <  0.16] <- "A) <  0.16 (median)"
A$EOS_med[A$EOSSI >= 0.16] <- "B) >= 0.16 (median)"
A$EOS_med[is.na(A$EOSSI)]  <- "C) missing"

A$GGTSI <- as.numeric(A$GGTSI)
median(A$GGTSI, na.rm=TRUE)
A$GGT_med[A$GGTSI <  25] <- "A) <  25 (median)"
A$GGT_med[A$GGTSI >= 25] <- "B) >= 25 (median)"
A$GGT_med[is.na(A$GGTSI)]  <- "C) missing"

A$GLUCPSI <- as.numeric(A$GLUCPSI)
median(A$GLUCPSI, na.rm=TRUE)
A$GLUCP_med[A$GLUCPSI <  5.2] <- "A) <  5.2 (median)"
A$GLUCP_med[A$GLUCPSI >= 5.2] <- "B) >= 5.2 (median)"
A$GLUCP_med[is.na(A$GLUCPSI)]  <- "C) missing"

A$HCT <- as.numeric(A$HCT)
median(A$HCT, na.rm=TRUE)
A$HCT_med[A$HCT <  0.43] <- "A) <  0.43 (median)"
A$HCT_med[A$HCT >= 0.43] <- "B) >= 0.43 (median)"
A$HCT_med[is.na(A$HCT)]  <- "C) missing"

A$HDLSI <- as.numeric(A$HDLSI)
median(A$HDLSI, na.rm=TRUE)
A$HDL_med[A$HDLSI <  1.22] <- "A) <  1.22 (median)"
A$HDL_med[A$HDLSI >= 1.22] <- "B) >= 1.22 (median)"
A$HDL_med[is.na(A$HDLSI)]  <- "C) missing"

A$HDT <- as.numeric(A$HDT)
median(A$HDT, na.rm=TRUE)
A$HDT_med[A$HDT <  1.1] <- "A) <  1.1 (median)"
A$HDT_med[A$HDT >= 1.1] <- "B) >= 1.1 (median)"
A$HDT_med[is.na(A$HDT)]  <- "C) missing"

A$HGBSI <- as.numeric(A$HGBSI)
median(A$HGBSI, na.rm=TRUE)
A$HGB_med[A$HGBSI <  146] <- "A) <  146 (median)"
A$HGB_med[A$HGBSI >= 146] <- "B) >= 146 (median)"
A$HGB_med[is.na(A$HGBSI)]  <- "C) missing"

A$KSI <- as.numeric(A$KSI)
median(A$KSI, na.rm=TRUE)
A$K_med[A$KSI <  4.3] <- "A) <  4.3 (median)"
A$K_med[A$KSI >= 4.3] <- "B) >= 4.3 (median)"
A$K_med[is.na(A$KSI)]  <- "C) missing"

A$LDLSI <- as.numeric(A$LDLSI)
median(A$LDLSI, na.rm=TRUE)
A$LDL_med[A$LDLSI <  3.11] <- "A) <  3.11 (median)"
A$LDL_med[A$LDLSI >= 3.11] <- "B) >= 3.11 (median)"
A$LDL_med[is.na(A$LDLSI)]  <- "C) missing"

A$LPASI <- as.numeric(A$LPASI)
median(A$LPASI, na.rm=TRUE)
A$LPA_med[A$LPASI <  0.21] <- "A) <  0.21 (median)"
A$LPA_med[A$LPASI >= 0.21] <- "B) >= 0.21 (median)"
A$LPA_med[is.na(A$LPASI)]  <- "C) missing"

A$LYMLESI <- as.numeric(A$LYMLESI)
median(A$LYMLESI, na.rm=TRUE)
A$LYMLE_med[A$LYMLESI <  26] <- "A) <  26 (median)"
A$LYMLE_med[A$LYMLESI >= 26] <- "B) >= 26 (median)"
A$LYMLE_med[is.na(A$LYMLESI)]  <- "C) missing"

A$LYMSI <- as.numeric(A$LYMSI)
median(A$LYMSI, na.rm=TRUE)
A$LYM_med[A$LYMSI <  1.76] <- "A) <  1.76 (median)"
A$LYM_med[A$LYMSI >= 1.76] <- "B) >= 1.76 (median)"
A$LYM_med[is.na(A$LYMSI)]  <- "C) missing"

A$MONOLSI <- as.numeric(A$MONOLSI)
median(A$MONOLSI, na.rm=TRUE)
A$MONOL_med[A$MONOLSI <  6] <- "A) <  6 (median)"
A$MONOL_med[A$MONOLSI >= 6] <- "B) >= 6 (median)"
A$MONOL_med[is.na(A$MONOLSI)]  <- "C) missing"

A$TRIGFSI <- as.numeric(A$TRIGFSI)
median(A$TRIGFSI, na.rm=TRUE)
A$TRIGF_med[A$TRIGFSI <  1.41] <- "A) <  1.41 (median)"
A$TRIGF_med[A$TRIGFSI >= 1.41] <- "B) >= 1.41 (median)"
A$TRIGF_med[is.na(A$TRIGFSI)]  <- "C) missing"

A$URATESI <- as.numeric(A$URATESI)
median(A$URATESI, na.rm=TRUE)
A$URATE_med[A$URATESI <  357] <- "A) <  357 (median)"
A$URATE_med[A$URATESI >= 357] <- "B) >= 357 (median)"
A$URATE_med[is.na(A$URATESI)]  <- "C) missing"

A$WBCSI <- as.numeric(A$WBCSI)
median(A$WBCSI, na.rm=TRUE)
A$WBC_med[A$WBCSI <  7.1] <- "A) <  7.1 (median)"
A$WBC_med[A$WBCSI >= 7.1] <- "B) >= 7.1 (median)"
A$WBC_med[is.na(A$WBCSI)]  <- "C) missing"

A$BASE <- as.numeric(A$BASE)
median(A$BASE, na.rm=TRUE)
A$Base_SBP[A$BASE <  132] <- "A) < 132 mmHg"
A$Base_SBP[A$BASE >= 132] <- "B) 132-145 mmHg"
A$Base_SBP[A$BASE >= 145] <- "C) >= 145 mmHg"
A$Base_SBP[is.na(A$BASE)]  <- "D) missing"


#names(A)[names(A) == "AGEGR1"]   <- ""


factors=c("STUDYID", "SEX", "RACE", "ETHNIC", "Height_gr", "Weight_gr", "BMI_gr", "Age_gr", 
          "Base_SBP", "CHD10R1", "ALB_med", "BASO_med", "BICAR_med", "BILI_med", "BUN_med", "CA_med",
          "CHOL_HDL_med", "CHOL_med", "CREAT_med", "EOSLE_med", "EOS_med", "GGT_med", "GLUCP_med", "HCT_med",
          "HDL_med", "HDT_med", "HGB_med", "K_med", "LDL_med", "LPA_med", "LYMLE_med", "LYM_med",
          "MONOL_med", "TRIGF_med", "URATE_med", "WBC_med")

### analysis function "auwe" to be filled in with the statistical evaluation
auwe<- function(D){
  
  Mean.Change.SBP.SoC <- round(mean(D$CHG[D$TRT01PN == 0], na.rm=TRUE),2)
  Mean.Change.SBP.Int <- round(mean(D$CHG[D$TRT01PN == 1], na.rm=TRUE),2)
  Diff.Change.SBP     <- Mean.Change.SBP.SoC - Mean.Change.SBP.Int
  
  Mean.PctChange.SBP.SoC <- round(mean(D$PCHG[D$TRT01PN == 0], na.rm=TRUE),2)
  Mean.PctChange.SBP.Int <- round(mean(D$PCHG[D$TRT01PN == 1], na.rm=TRUE),2)
  Diff.PctChange.SBP     <-  Mean.PctChange.SBP.Int - Mean.PctChange.SBP.SoC
  
  N.SoC  <- sum(D$TRT01PN==0)
	N.Int  <- sum(D$TRT01PN==1)

	Responder.120.SoC    <- sum(D$AVALCAT1N[D$TRT01PN == 0] == 1, na.rm=TRUE)
	Responder.120.Int    <- sum(D$AVALCAT1N[D$TRT01PN == 1] == 1, na.rm=TRUE)

  Prop.Responder.120.SoC <- round(Responder.120.SoC/sum(!is.na(D$AVALCAT1N[D$TRT01PN == 0]), na.rm=TRUE)*100,2)
  Prop.Responder.120.Int <- round(Responder.120.Int/sum(!is.na(D$AVALCAT1N[D$TRT01PN == 1]), na.rm=TRUE)*100,2)

  Diff.Responder.120     <- Prop.Responder.120.Int - Prop.Responder.120.SoC
  OR.Responder.120       <- limit(round((Responder.120.Int * (N.SoC-Responder.120.SoC))/(Responder.120.SoC * (N.Int-Responder.120.Int)),3), high=100)
  RelRisk.Responder.120  <- limit(round((Responder.120.Int * N.SoC)/(Responder.120.SoC * N.Int),3), high=100)
  
  Responder.132.SoC    <- sum(D$AVALCAT2N[D$TRT01PN == 0] == 0, na.rm=TRUE)
  Responder.132.Int    <- sum(D$AVALCAT2N[D$TRT01PN == 1] == 0, na.rm=TRUE)
  
  Prop.Responder.132.SoC <- round(Responder.132.SoC/sum(!is.na(D$AVALCAT2N[D$TRT01PN == 0]), na.rm=TRUE)*100,2)
  Prop.Responder.132.Int <- round(Responder.132.Int/sum(!is.na(D$AVALCAT2N[D$TRT01PN == 1]), na.rm=TRUE)*100,2)
  
  Diff.Responder.132     <- Prop.Responder.132.Int - Prop.Responder.132.SoC
  OR.Responder.132       <- limit(round((Responder.132.Int * (N.SoC-Responder.132.SoC))/(Responder.132.SoC * (N.Int-Responder.132.Int)),3))
  RelRisk.Responder.132  <- limit(round((Responder.132.Int * N.SoC)/(Responder.132.SoC * N.Int),3))
  
  
	return(data.frame(Diff.Change.SBP, Mean.Change.SBP.SoC, Mean.Change.SBP.Int,
	                  Diff.PctChange.SBP, Mean.PctChange.SBP.SoC, Mean.PctChange.SBP.Int, 
	                  Diff.Responder.120, OR.Responder.120, RelRisk.Responder.120, Prop.Responder.120.SoC, Prop.Responder.120.Int,
	                  Diff.Responder.132, OR.Responder.132, RelRisk.Responder.132, Prop.Responder.132.SoC, Prop.Responder.132.Int,
	                  N.SoC, N.Int
	                  ))

}

#limit function / truncation of large or small estimates
limit <- function(x, low=0.05, high=20){
  if (!is.na(x)) y=min(high, max(low,x)) else y=NA
  return (y)
}

results <- subscreencalc(data=A,
                    eval_function=auwe,
                    endpoints=c("CHG", "PCHG", "AVALCAT1N", "AVALCAT2N"),
                    treat="TRT01PN",
                    subjectid="USUBJD",
			              factors=factors,
                    min_comb=1,
                    max_comb=2,
			              nkernel=16,
                    par_functions = "limit",
			              factorial = TRUE,
			              use_complement =FALSE, 
                    verbose=T)


#setwd("O:/1_Global_Biostatistics/Biostatistics Innovation Center/BIC Project - Subgroup Analyses/Screening/16244")
#save(results, file = "o:/1_Global_Biostatistics/Biostatistics Innovation Center/BIC Project - Subgroup Analyses/Screening/_archive/WW/sgs3.RData")
# rm("results")
# load("o:/1_Global_Biostatistics/Biostatistics Innovation Center/BIC Project - Subgroup Analyses/Screening/_archive/WW/sgs3.RData")

subscreenshow(results, port=14444)

