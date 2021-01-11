# Nomogram:
# =========

setwd("/home/lorenz/Wonderful_Wednesday_Webinars/2021-01/")
dat <- read.csv("Reexcision.csv")
for (i in 1:ncol(dat)) {
  if (length(unique(dat[, i])) < 5) {
    dat[, i] <- factor(dat[, i])
  }
}

dat$RE <- factor(dat$RE, levels = c(0, 1), labels = c("no", "yes"))

require(rms)

# attach(dat)
# ddist <- datadist(age, tumor.size, hist, mult.foc, acc.in.situ, lymph.inv, estr.rec, prog.rec)
ddist <- datadist(dat)
options(datadist='ddist')
# detach(dat)
fit <- lrm(RE ~ ., data = dat)

png("nomogram.png", height = 8, width = 10, res = 150, units = "in")
par(xpd = T)
plot(nomogram(fit, fun = function(x) 1/(1 + exp(-x)),
              funlabel = "Probability for reexcission",
              fun.at = c(.001, 0.01, 0.05, seq(.1, .9, by = .1), .95, .99, .999)),
     label.every = 1)
text(0.4, 1.07, "Nomogram based on RE data:
     Read out the points for each predictor and sum them up.
     Then, the probability for re-excission can be read out.")
dev.off()