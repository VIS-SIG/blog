####################################################################
# Program name: hgb_quantiles_f.R
# Purpose: To produce plot summarising spread of Hgb values at each
#         visit (for Wonderful Wednesdays July 2020)
# Written by: Steve Mallett
# Date: 12-Jun-2020
####################################################################

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# Get Hgb data (Group E)

hgb1 <- read_sas("hgb_data.sas7bdat") %>%
  filter(TRT01PN == 1)

quant1 <- hgb1 %>% group_by(AVISITN) %>%
  do(quant = quantile(.$AVAL, probs = seq(0.2,0.8,.05)), probs = seq(0.2,0.8,.05)) %>%
  unnest(cols=c(quant, probs)) %>%
  mutate(delta = 2*round(abs(.5-probs)*100)) %>% 
  group_by(AVISITN, delta) %>%
  summarize(quantmin = min(quant), quantmax= max(quant))

# Derive median Hgb

hgb_med1 <- hgb1 %>% 
  group_by(AVISITN) %>% 
  summarise(hgb.median = median(AVAL)) 

# Produce plot for group E

plot01 <- ggplot() +
  geom_ribbon(data = quant1, aes(x = AVISITN, ymin = quantmin, ymax = quantmax,
                                    group = reorder(delta, -delta), fill = as.numeric(delta)),
              alpha = .5) +
  geom_line(data = hgb_med1, aes(x = AVISITN, y = hgb.median, color = "dark blue"), size = 2 ) +  
  geom_segment(aes(x=10, xend=70, y=10, yend=10), linetype = 2, color = "blue") +
  geom_segment(aes(x=10, xend=70, y=11.5, yend=11.5), linetype = 2, color = "blue") +
  scale_x_continuous(" ",
                     breaks=c(10, 20, 30, 40, 50, 60, 70),
                     limits=c(10, 70)) +
  scale_y_continuous("Hgb (g/dL)",
                     breaks=c(8, 9, 10, 11, 12),
                     limits=c(8, 12),
                     ) +
   scale_fill_gradient(low = "#000080",
                       high = "#87CEFA",) +
  scale_color_identity(name = " ",
                       guide=legend,
                       labels = " ") + 
  theme_minimal() +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y.left =  element_text(color = 'blue'),        
        plot.title = element_text(hjust = 0.5, size = 25),
        text = element_text(size = 15),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin=unit(c(1,0,0,0),"cm")) +
  ggtitle(label = "Treatment Group: E")

# Get Hgb data (control)

hgb2 <- read_sas("hgb_data.sas7bdat") %>%
  filter(TRT01PN == 2)

quant2 <- hgb2 %>% group_by(AVISITN) %>%
  do(quant = quantile(.$AVAL, probs = seq(0.2,0.8,.05)), probs = seq(0.2,0.8,.05)) %>%
  unnest(cols=c(quant, probs)) %>%
  mutate(delta = 2*round(abs(.5-probs)*100)) %>% 
  group_by(AVISITN, delta) %>%
  summarize(quantmin = min(quant), quantmax= max(quant))

# Derive median Hgb

hgb_med2 <- hgb2 %>% 
  group_by(AVISITN) %>% 
  summarise(hgb.median = median(AVAL)) 

plot02 <- ggplot() +
  geom_ribbon(data = quant2, aes(x = AVISITN, ymin = quantmin, ymax = quantmax,
                                    group = reorder(delta, -delta), fill = as.numeric(delta)),
              alpha = .5) +
  geom_line(data = hgb_med2, aes(x = AVISITN, y = hgb.median, color = "dark blue"), size = 2) + 
  geom_segment(aes(x=10, xend=70, y=10, yend=10), linetype = 2, color = "blue") +
  geom_segment(aes(x=10, xend=70, y=11.5, yend=11.5), linetype = 2, color = "blue") +
  scale_x_continuous("Week",
                     breaks=c(10, 20, 30, 40, 50, 60, 70),
                     labels=c("0", "4", "8", "12", "16", "20", "24"),
                     limits=c(10, 70)) +
  scale_y_continuous("Hgb (g/dL)",
                     breaks=c(8, 9, 10, 11, 12),
                     limits=c(8, 12),
                     ) +
  scale_fill_gradient(
                      low = "#000080",
                      high = "#87CEFA") +
  scale_color_identity(name = "Median",
                       guide=legend,
                       labels = " ") + 
  labs(fill = "Hgb (% patients in band)") +
  theme_minimal() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5, size = 25),
        text = element_text(size = 15),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        axis.text.y.left =  element_text(color = 'blue'),
        legend.text=element_text(size=14),
        legend.title=element_text(size=20),
        legend.key.size = unit(1.2, "cm"),        
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  ggtitle(label = "Treatment Group: C")

p <- grid.arrange(arrangeGrob(plot01, ncol=1, nrow=1),
                  arrangeGrob(plot02, ncol=1, nrow=1),
                  heights = c(1,1.32))

title <- ggdraw() + draw_label("Treatment E Stabilises Haemoglobin Within Target Range\n With Reduced Variability", size = 25)

p2 <- plot_grid(title, p, ncol=1, rel_heights = c(1, 10))  

ggsave("hgb_plot_quantiles.png", p2, width=12, height=12, dpi=300)

