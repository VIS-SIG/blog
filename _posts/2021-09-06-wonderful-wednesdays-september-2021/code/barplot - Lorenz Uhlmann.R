# Barplot by treatment and time:
# Frequency of values per patient within the range of 72 and 140 mg/dL:

# Read in data:
dat <- read.csv("simulated_data.csv", sep = ",", dec = ".")

# Create function to calculate the percentage per patient:
num.f <- function(x) length(which(x >= 72 & x <= 140)) / length(x)

# Calculate mean values:
res <- aggregate(dat$Simulated_CGMValue, list(dat$SUBJID, dat$TREATMENT, dat$VISITNUM), num.f)
res <- aggregate(res$x, list(res$Group.2, res$Group.3), mean)

# Prepare the results to be plotted:
names(res) <- c("TREATMENT", "VISITNUM", "Value")
res$VISITNUM <- factor(res$VISITNUM, levels = c(3, 17, 21), labels = c("baseline", "26 weeks", "52 weeks"))
res$TREATMENT <- factor(res$TREATMENT, ordered = T,
                        levels = c("SOC", "Rx low", "Rx medium", "Rx high"))
res$Value <- res$Value*100
res$val.t <- format(round(res$Value, 1), nsmall = 1)

# Create the plot:
library(ggplot2)
library(RColorBrewer)
ggplot(res, aes(y=Value, x=VISITNUM, fill=TREATMENT)) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Time under good glycemic control (%)") + xlab ("Study visit number") +
  theme_minimal() + #scale_fill_brewer(palette="Set1")
  scale_fill_manual(breaks = c("SOC", "Rx low", "Rx medium", "Rx high"),
                    values = c("lightgrey", brewer.pal(n = 3, name = "Greens"))) +
  geom_text(aes(label=val.t), vjust = 1.5, size = 4, position = position_dodge(.9)) +
  ggtitle("The relative amount of time (averaged over all patients) under good glycemic control
(in the range of 72 and 140 mg/dL) was the highest in the medium and high dose.")
ggsave("barplot.png")