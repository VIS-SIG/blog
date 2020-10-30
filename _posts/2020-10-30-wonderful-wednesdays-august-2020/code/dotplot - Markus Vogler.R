### Load packages
library(here)
library(tidyverse)
library(ggtext)

### Load data
data <- data.frame(read_delim(here("2020-07-08-COPD-PSI-data.csv"), delim=','))
data$ANTHONISEN <- fct_rev(factor(data$ANTHONISEN)) # reverse factor for plotting

### Get percentage for number of exacerbations by treatment group
freq <- data %>% group_by(RAND_TRT, ANTHONISEN, .drop = F) %>% summarise(n = n()) %>% mutate(freq = 100 * n / sum(n))
freq$freq_label <- format(round(freq$freq, 1), nsmall = 1)

### Add N's to treatment group labels
N <- data %>% group_by(RAND_TRT) %>% summarize(N = n())

### Create label variable including number of subjects
freq_n <- merge(freq, N, by = "RAND_TRT")
freq_n$RAND_TRT_L <- paste0(freq_n$RAND_TRT, " (N=", freq_n$N, ")")

### Colors
fill <- c("#E7B800", "#2E9FDF")

### Plot
ggplot(freq_n, aes(x = ANTHONISEN, y = freq, color = RAND_TRT)) +
  # Set minimal theme and flip x/y-axes
  theme_minimal(base_size = 20) +
  coord_flip() +
  # Plot points
  geom_point(position = position_dodge(width = 0), shape = 124, size = 10) +
  # Plot annotations (different positions by treatment group)
  geom_text(data = freq[freq$RAND_TRT == "ICS/LABA",], aes(label=freq_label, x=ANTHONISEN, y=freq), hjust=0.6, vjust=-1.5, size = 5) +
  geom_text(data = freq[freq$RAND_TRT == "LABA",], aes(label=freq_label, x=ANTHONISEN, y=freq), hjust=0.6, vjust=2.5, size = 5) +
  # Color treatment group labels in title
  labs(title = paste("Larger proportion exacerbation-free with <span style='color:", fill[1], "'>", unique(freq_n$RAND_TRT_L)[1], 
                     "</span> compared to <span style='color:", fill[2], "'>", unique(freq_n$RAND_TRT_L)[2], "</span>"),
       subtitle = "% of total within treatment groups by number of exacerbations") +
  theme(plot.title = element_markdown()) +
  # Set colors and remove legend (in comments: code to color treatment group labels in legend)
  #scale_color_manual(labels = paste("<span style='color:", fill, "'>", unique(freq$RAND_TRT), "</span>"), 
  #                   values = fill, name = NULL) +   
  #theme(legend.text = element_markdown(size = 12), legend.position = c(0.1, 0.89)) +
  #guides(colour = guide_legend(override.aes=list(size=0))) +
  scale_color_manual(values = fill) +
  theme(legend.position = "none") +
  # Remove clutter
  scale_y_continuous(expand = c(0,1)) +
  theme(axis.title.x = element_blank(), axis.text.x=element_blank(), 
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  scale_x_discrete(name = NULL) +
  NULL

ggsave(file = here("dotplot.png"), width = 35, height = 20, units = "cm")
  

