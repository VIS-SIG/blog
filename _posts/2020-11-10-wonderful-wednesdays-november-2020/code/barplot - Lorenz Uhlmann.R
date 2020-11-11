# Baplots to show the reduction in treatment effect on DLQI.
# ==========================================================

# Read in the data set:
dat <- read.csv("mediation_data.csv")


fit <- lm(DLQI ~ TRT, data = dat)
t.val.pure <- coef(summary(fit))[2, 3]

t.val.vec <- numeric(3)
j <- 1
for (i in c(2, 4, 6)) {
  fit <- lm(dat$DLQI ~ dat[, i] + dat$TRT)
  t.val.vec[j] <- coef(summary(fit))[3, 3]
  j <- j + 1
}

# Redo without imputed data:
t.val.vec.re <- numeric(3)
j <- 1
for (i in c(2, 4, 6)) {
  dat.re <- dat[which(dat[, i+1] == F), ]
  fit <- lm(dat.re$DLQI ~ dat.re[, i] + dat.re$TRT)
  t.val.vec.re[j] <- coef(summary(fit))[3, 3]
  j <- j + 1
}

# Combine both vectors:
t.vals <- c(t.val.vec.re[3], t.val.vec[3],
            t.val.vec.re[2], t.val.vec[2],
            t.val.vec.re[1], t.val.vec[1])

# Calculate difference:
t.vals - t.val.pure

# Calculate the difference:
t.val.diff <- t.vals - t.val.pure
t.val.mat <- matrix(t.val.diff, ncol = 3)
fit <- lm(DLQI ~ TRT, dat)
t.val.mat.pr <- t.val.mat/abs(coef(summary(fit))[2, 3]) * 100

png("barplot.png", width = 7, height = 5, res = 300, units = "in")
par(xpd = T, cex.main = 0.9)
barplot(t.val.diff, horiz = T, col = c("darkcyan", "blue"), xlim = c(0, 5),
        space = rep(c(0.25, 0), 3),
        main = "Adjusting for itch leads to the greatest reduction
        in the standardized treatment effect on DLQI.
        Removing the LOCF imputed data diminishes the differences.",
        xlab = "Reduction in the standardized treatment effect")
y.coord <- c(1.25, 3.5, 5.75)
text(-0.25, y.coord[3], "itch")
text(-0.25, y.coord[2], "BSA")
text(-0.35, y.coord[1], "redness")
for (i in 1:nrow(t.val.mat)) {
  for (j in 1:ncol(t.val.mat)) {
    t.val <- paste0(format(round(t.val.mat[i, j], 1), nsmall = 1), " (",
                    format(round(t.val.mat.pr[i, j], 1), nsmall = 1), "%)")
    text(t.val.mat[i, j] + .45, y.coord[j] + i - 1.5, t.val,
         col = c("darkcyan", "blue")[i])
  }
}
segments(abs(t.val.pure), 0, abs(t.val.pure), 6.75, lty = 2, col = "darkblue")
text(4.6, -0.5, paste("Unadjusted \n effect:", format(round(abs(t.val.pure), 1), nsmall = 1)),
     cex = 0.8, col = "darkblue")
legend(x = 3.25, y = 2.25, legend = c("LOCF", "Observed"), fill = c("blue", "darkcyan"), bty = "n")
dev.off()