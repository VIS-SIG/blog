### Load packages
library(here)
library(tidyverse)

### Load data
data <- data.frame(read_delim(here("Satisfaction_wW2005.csv"), delim=';'))
data$smoker <- ifelse(data$smoker == "Y", "Smoker", ifelse(data$smoker == "N", "Non-smoker", NA))

### Only keep non-missing data
data_nonmiss <- na.omit(data[, c("satisfaction", "smoker")])

### Derive number of subjects within subgroups
n <- data_nonmiss %>% group_by(smoker) %>% summarize(n = n())
  
### Create label variable including number of subjects
data_n <- merge(data_nonmiss, n, by = "smoker")
data_n$group <- paste0(data_n$smoker, " (n = ", data_n$n, ")")
  
### Plot
ggplot(data = data_n, aes(x = satisfaction, colour = group)) +
  stat_ecdf(size = 1, alpha = 0.5) +
  theme_minimal(base_size = 15) +
  scale_x_reverse(name = "Satisfaction with life", breaks = seq(0, 10, 1), minor_break = NULL) +
  scale_y_continuous(name = "Cumulative proportion of subjects", breaks = seq(0, 1, 0.25), minor_break = NULL) +
  scale_color_manual(name = NULL, values = c("#E7B800", "#2E9FDF")) +
  theme(legend.position = c(0.15, 0.85)) +
  geom_segment(aes(x = 4.5, y = 0.5, xend = 2.5, yend = 0.5), colour = "black", size = 1, arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 3.5, y = 0.53, label = "Less satisfied", size = 5, colour = "black") +
  ggtitle("Non-smokers more satisfied with life than smokers") +
  NULL

ggsave(file = here("cdf_smoker.png"), width = 35, height = 20, units = "cm")
  

