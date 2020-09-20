####################################################################
# Program name: hgb_lasagna_f.R
# Purpose: To produce lasagna plot individual Hgb values at each
#         visit (for Wonderful Wednesdays July 2020)
# Written by: Steve Mallett
# Date: 12-Jun-2020
####################################################################

library(haven)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# flag variables for Hgb changes since previous visit were added prior to import (using SAS)

hgb1 <- read_sas("hgb_sim2.sas7bdat") %>%
  filter(TRT01PN == 1 & AVISITN != 10) %>%
  mutate(id = as.numeric(str_extract(USUBJID, "[^.]+$")))

plot01 <- ggplot() +
  geom_raster(data = hgb1, aes(x=AVISITN, y=id, fill=factor(flag)),  interpolate = TRUE, hjust = 0, vjust = 0) +
  scale_x_continuous(" ",
                     breaks=c(20, 30, 40, 50, 60, 70),
                     labels=c("4", "8", "12", "16", "20", "24"),
                     limits=c(10, 70)) +
  scale_y_continuous(" ") +
  scale_fill_manual(
    values=c("#0066CC", "#99CCFF", "#E0E0E0", "#FF9999", "#CC0000"), 
    name="Hgb \nExcursion\ng/dL",
    breaks=c("-2", "-1", "0", "1", "2"),
    labels=c("<-2", "<-1", "None", ">1", ">2")
    ) +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 15),
        axis.ticks.x = element_blank(),
        axis.text.x =  element_text(size = 15),  
        axis.text.y =  element_blank(),        
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 25),
        panel.border = element_rect(colour = "black", fill=NA, size=0.25),
        panel.grid = element_blank(),
        plot.margin=unit(c(0,0,0,0),"cm")) +
  ggtitle(label = "Treatment: E")

hgb2 <- read_sas("hgb_sim2.sas7bdat") %>%
  filter(TRT01PN == 2 & AVISITN != 10) %>%
  mutate(id = as.numeric(str_extract(USUBJID, "[^.]+$")))

plot02 <- ggplot() +
  geom_raster(data = hgb2, aes(x=AVISITN, y=id, fill=factor(flag)),  interpolate = TRUE, hjust = 0, vjust = 0) +
  scale_x_continuous("Week",
                     breaks=c(20, 30, 40, 50, 60, 70),
                     labels=c("4", "8", "12", "16", "20", "24"),
                     limits=c(10, 70)) +
  scale_y_continuous(" ") +
  scale_fill_manual(
    values=c("#0066CC", "#99CCFF", "#E0E0E0", "#FF9999", "#CC0000"), 
    name="Hgb Change\nSince Last Visit\ng/dL",
    breaks=c("-2", "-1", "0", "1", "2"),
    labels=c("<-2", "<-1", "None", ">1", ">2")
  ) +
  theme_minimal() +
  theme(legend.position="bottom",
    text = element_text(size = 15),
    axis.ticks.x = element_blank(),
    axis.text.x =  element_text(size = 15),  
    axis.text.y =  element_blank(),        
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size = 20),
    plot.title = element_text(hjust = 0.5, size = 25),
    panel.border = element_rect(colour = "black", fill=NA, size=0.25),
    panel.grid = element_blank(),
    legend.title=element_text(size=15),
    legend.text=element_text(size=15),
    plot.margin=unit(c(0,0,0,0),"cm")) +
  ggtitle(label = "Treatment: C")

p <- plot_grid(plot01, plot02, align = "v", nrow = 2, rel_heights = c(1, 1.19))

title <- ggdraw() + draw_label("Lasagna Plot of Individual Haemoglobin\n Changes Since Previous Visit\n", size = 25)

p2 <- plot_grid(title, p, ncol=1, rel_heights = c(1, 10))  

ggsave("hgb_lasagna_plot.png", p2, width=12, height=12, dpi=300)


              