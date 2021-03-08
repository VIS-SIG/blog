# Heatmap:
##########

# Load required packages:
require(naniar)
require(ggplot2)

# Load data set:
dat <- read.csv("missing_data.csv")

# Focus only on continuous pain variables:
dat <- dat[, c(1:11, 25)]

# Reshape data into long format and do some data preparation steps:
dat.long <- reshape(dat, varying = 1:11, direction = "long", sep = ".")
dat.long <- dat.long[, -4]
dat.long$time <- factor(dat.long$time)


dat.long.pbo <- dat.long[which(dat.long$trt == "pbo"), ]
dat.long.act <- dat.long[which(dat.long$trt == "act"), ]

names(dat.long.act)[3] <- "active"

dat.long.pbo <- dat.long.pbo[, -1]
dat.long.act <- dat.long.act[, -1]

dat.long.combined <- dat.long.act
dat.long.combined$placebo <- dat.long.pbo$pain

# Create heatmap:
gg_miss_fct(x = dat.long.combined, fct = time) +
  ggtitle("Percentage of missing values increases over time
and is generally higher in the active group") +
  xlab("Visit") + ylab("Treatment") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        text = element_text(size = 18))
ggsave("heatmap.png", height = 5, width = 10)