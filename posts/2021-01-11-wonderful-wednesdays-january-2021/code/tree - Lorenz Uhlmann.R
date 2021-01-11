# Prediction data:
##################

# Classification tree:
# ====================

setwd("/home/lorenz/Wonderful_Wednesday_Webinars/2021-01/")
dat <- read.csv("Reexcision.csv")
for (i in 1:ncol(dat)) {
  if (length(unique(dat[, i])) < 5) {
    dat[, i] <- factor(dat[, i])
  }
}

dat$RE <- factor(dat$RE, levels = c(0, 1), labels = c("no", "yes"))

require(rpart)
require(rpart.plot)
dat_tree <- rpart(RE~., dat, model = TRUE)
png("tree.png", height = 4, width = 6, res = 150, units = "in")
prp(dat_tree, type = 4, extra = 6, cex = 0.5, varlen = 0,
    main = 'Classification tree for reexcission:
    Accomp. in situ, Tumor size, and histology seem to be the most important predictors.
    Each box shows the predicted value and the relative frequency of the category RE=yes.')
dev.off()