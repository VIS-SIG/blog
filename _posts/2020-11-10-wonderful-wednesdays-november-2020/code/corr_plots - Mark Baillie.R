#########################################
##  Warning this code requires a re-factor
## and put repeated steps in to a function
#########################################

library(corrplot)
library(tidyverse)

## read in data
final_in <- read_csv("mediation_data.csv")

#plot on one page
par(mfrow = c(2, 3))
par(cex = 0.75)

##-----------------------------------------------------
## Overall correlations
title <- "How do all outcomes relate overall?"

corrs <- final_in %>%
  dplyr::select("itch", "BSA", "redness", "DLQI") %>%
  filter(complete.cases(.)) %>%
  dplyr::mutate_all(as.numeric)

M <- cor(corrs)
col <-
  colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(
  M,
  method = "ellipse",
  col = col(200), tl.cex = 1/par("cex"),
  type = "upper",
  order = "hclust",
  number.cex = .7,
  title = title,
  addCoef.col = "black",
  # Add coefficient of correlation
  tl.col = "black",
  tl.srt = 90,
  # Text label color and rotation
  # hide correlation coefficient on the principal diagonal
  diag = FALSE,
  mar = c(0, 0, 3, 0)
)



##-----------------------------------------------------
### - By Rx arm
title <- "How do all outcomes relate within Rx?"

corrs <- final_in %>%
  filter(TRT == "Rx") %>%
  dplyr::select("itch", "BSA", "redness", "DLQI") %>%
  dplyr::mutate_all(as.numeric)
M <- cor(corrs)
col <-
  colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

corrplot(
  M,
  method = "ellipse",
  col = col(200), tl.cex = 1/par("cex"),
  type = "upper",
  order = "hclust",
  number.cex = .7,
  title = title,
  addCoef.col = "black",
  # Add coefficient of correlation
  tl.col = "black",
  tl.srt = 90,
  diag = FALSE,
  mar = c(0, 0, 3, 0)
)


##-----------------------------------------------------
## By Placebo
corrs <- final_in %>%
  filter(TRT == "placebo") %>%
  dplyr::select("itch", "BSA", "redness", "DLQI") %>%
  dplyr::mutate_all(as.numeric)
M <- cor(corrs)
col <-
  colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

title <- "How do all outcomes relate within Placebo?"
corrplot(
  M,
  method = "ellipse",
  col = col(200), tl.cex = 1/par("cex"),
  type = "upper",
  order = "hclust",
  number.cex = .7,
  title = title,
  addCoef.col = "black",
  # Add coefficient of correlation
  tl.col = "black",
  tl.srt = 90,
  # Text label color and rotation
  # hide correlation coefficient on the principal diagonal
  diag = FALSE,
  mar = c(0, 0, 3, 0)
)


##-----------------------------------------------------
## Overall and complete cases

corrs <- final_in %>%
  filter(itch_LOCF == FALSE &
           BSA_LOCF == FALSE & redness_LOCF == FALSE & DLQI_LOCF == FALSE) %>%
  filter(complete.cases(.)) %>%
  dplyr::select("itch", "BSA", "redness", "DLQI") %>%
  dplyr::mutate_all(as.numeric)

M <- cor(corrs)

col <-
  colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))



title <-
  "How do all outcomes relate overall\n(excluding patients with imputed data)?"
corrplot(
  M,
  method = "ellipse",
  col = col(200), tl.cex = 1/par("cex"),
  type = "upper",
  order = "hclust",
  number.cex = .7,
  title = title,
  addCoef.col = "black",
  # Add coefficient of correlation
  tl.col = "black",
  tl.srt = 90,
  # Text label color and rotation
  # hide correlation coefficient on the principal diagonal
  diag = FALSE,
  mar = c(0, 0, 3, 0)
)


##-----------------------------------------------------
### By Rx and complete cases
corrs <- final_in %>%
  filter(TRT == "Rx") %>%
  filter(itch_LOCF == FALSE &
           BSA_LOCF == FALSE & redness_LOCF == FALSE & DLQI_LOCF == FALSE) %>%
  filter(complete.cases(.)) %>%
  dplyr::select("itch", "BSA", "redness", "DLQI") %>%
  dplyr::mutate_all(as.numeric)
M <- cor(corrs)
col <-
  colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

title <- "How do all outcomes relate within Rx?\n(Complete cases)"
corrplot(
  M,
  method = "ellipse",
  col = col(200), tl.cex = 1/par("cex"),
  type = "upper",
  order = "hclust",
  number.cex = .7,
  title = title,
  addCoef.col = "black",
  # Add coefficient of correlation
  tl.col = "black",
  tl.srt = 90,
  # Text label color and rotation
  # hide correlation coefficient on the principal diagonal
  diag = FALSE,
  mar = c(0, 0, 3, 0)
)


##-----------------------------------------------------
### Placebo and complete cases

corrs <- final_in %>%
  filter(TRT == "placebo") %>%
  filter(itch_LOCF == FALSE &
           BSA_LOCF == FALSE & redness_LOCF == FALSE & DLQI_LOCF == FALSE) %>%
  filter(complete.cases(.)) %>%
  dplyr::select("itch", "BSA", "redness", "DLQI") %>%
  dplyr::mutate_all(as.numeric)
M <- cor(corrs)
col <-
  colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

title <-
  "How do all outcomes relate within placebo?\n(Complete cases)"
corrplot(
  M,
  method = "ellipse",
  col = col(200), tl.cex = 1/par("cex"),
  type = "upper",
  order = "hclust",
  number.cex = .7,
  title = title,
  addCoef.col = "black",
  # Add coefficient of correlation
  tl.col = "black",
  tl.srt = 90,
  # Text label color and rotation
  # hide correlation coefficient on the principal diagonal
  diag = FALSE,
  mar = c(0, 0, 3, 0)
)
