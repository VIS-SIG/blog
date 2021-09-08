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
library(grid)
library(gridExtra)

cgm0 <- read_sas("/shared/175/arenv/arwork/gsk1278863/mid209676/present_2020_01/code/CGM/cgm_data2.sas7bdat") %>%
  filter(trtcd == 0 & VISITNUM == 21)

quant <- cgm0 %>% group_by(CGMTIME) %>%
  do(quant = quantile(.$aval, probs = seq(0.2,0.8,.05)), probs = seq(0.2,0.8,.05)) %>%
  unnest(cols=c(quant, probs)) %>%
  mutate(delta = 2*round(abs(.5-probs)*100)) %>% 
  group_by(CGMTIME, delta) %>%
  summarize(quantmin = min(quant), quantmax= max(quant)) %>%
  mutate(hour = (CGMTIME/3600)) %>%
  filter(delta != 0)

# Produce plot for Placebo

plot0 <- ggplot() +
  geom_ribbon(data = quant, aes(x = hour, ymin = quantmin, ymax = quantmax,
                                    group = reorder(delta, -delta), fill = as.numeric(delta)),
              alpha = .5) +
  geom_segment(aes(x=1, xend=24, y=180, yend=180), linetype = 2, color = "blue") +
 scale_x_continuous(" ",
                    breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
                    limits=c(1, 24)) +
 scale_y_continuous("Glucose (mg/dL)",
                   breaks=c(100, 150, 200),
                   limits=c(100, 200),
                   ) +
  scale_fill_continuous(guide = guide_legend(direction = "horizontal", 
                                             title.position = "none")) + 
  scale_color_identity(name = " ",
                       guide=legend,
                       labels = " ") + 
  theme_minimal() +
  theme(legend.position="none",
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 15, color = 'black'),
        axis.title.y = element_text(size = 25),

        axis.ticks.x=element_blank(),
        axis.text.y.left =  element_text(color = 'black'),        
        plot.title = element_text(hjust = 0.5, size = 25),
        text = element_text(size = 15),

        axis.title = element_text(size = 25),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin=unit(c(1,0,0,0),"cm")) +
  ggtitle(label = "Placebo")

#####################################
cgm1 <- read_sas("/shared/175/arenv/arwork/gsk1278863/mid209676/present_2020_01/code/CGM/cgm_data2.sas7bdat") %>%
  filter(trtcd==1 & VISITNUM == 21)

quant <- cgm1 %>% group_by(CGMTIME) %>%
  do(quant = quantile(.$aval, probs = seq(0.2,0.8,.05)), probs = seq(0.2,0.8,.05)) %>%
  unnest(cols=c(quant, probs)) %>%
  mutate(delta = 2*round(abs(.5-probs)*100)) %>% 
  group_by(CGMTIME, delta) %>%
  summarize(quantmin = min(quant), quantmax= max(quant)) %>%
  mutate(hour = (CGMTIME/3600)) %>%
  filter(delta != 0)

plot1 <- ggplot() +
  geom_ribbon(data = quant, aes(x = hour, ymin = quantmin, ymax = quantmax,
                                 group = reorder(delta, -delta), fill = as.numeric(delta)),
              alpha = .5) +
  geom_segment(aes(x=1, xend=24, y=180, yend=180), linetype = 2, color = "blue") +
  scale_x_continuous(" ",
                     breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
                     limits=c(1, 24)) +
  scale_y_continuous("Glucose (mg/dL)",
                     breaks=c(100, 150, 200),
                     limits=c(100, 200),
  ) +
   scale_fill_continuous(guide = guide_legend(direction = "horizontal", 
                                             title.position = "none")) + 
  scale_color_identity(name = " ",
                       guide=legend,
                       labels = " ") + 
  theme_minimal() +
  theme(legend.position="none",
        axis.title.y = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y.left =  element_blank(),        
        plot.title = element_text(hjust = 0.5, size = 25),
        text = element_text(size = 15),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin=unit(c(1,0,0,0),"cm")) +
  ggtitle(label = "Low Dose")

##################################################################################################
cgm2 <- read_sas("/shared/175/arenv/arwork/gsk1278863/mid209676/present_2020_01/code/CGM/cgm_data2.sas7bdat") %>%
  filter(trtcd==2 & VISITNUM == 21)

quant <- cgm2 %>% group_by(CGMTIME) %>%
  do(quant = quantile(.$aval, probs = seq(0.2,0.8,.05)), probs = seq(0.2,0.8,.05)) %>%
  unnest(cols=c(quant, probs)) %>%
  mutate(delta = 2*round(abs(.5-probs)*100)) %>% 
  group_by(CGMTIME, delta) %>%
  summarize(quantmin = min(quant), quantmax= max(quant)) %>%
  mutate(hour = (CGMTIME/3600)) %>%
  filter(delta != 0)

plot2 <- ggplot() +
  geom_ribbon(data = quant, aes(x = hour, ymin = quantmin, ymax = quantmax,
                                 group = reorder(delta, -delta), fill = as.numeric(delta)),
              alpha = .5) +
  geom_segment(aes(x=1, xend=24, y=180, yend=180), linetype = 2, color = "blue") +
  scale_x_continuous("Time",
                     breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
                     limits=c(1, 24)) +
  scale_y_continuous("Glucose (mg/dL)",
                     breaks=c(100, 150, 200),
                     limits=c(100, 200)) +
  scale_fill_continuous(guide = guide_legend(title = "% patients in band",
                                             direction = "horizontal",
                                             title.position = "left",
                                             element_text(size=15))) +
   theme_minimal() +
  theme(legend.position=c(0.5, 0.65),
        plot.title = element_text(hjust = 0.5, size = 25),
        text = element_text(size = 15),
        axis.text = element_text(size = 15, color = 'black'),
        axis.title.y = element_text(size = 25),
        axis.title.x = element_text(size = 15),        
        legend.key.size = unit(1, "cm"),        
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  # guides(colour = guide_legend(override.aes = list(size=3))) +
  ggtitle(label = "Medium Dose")

############################################################################################################
cgm3 <- read_sas("/shared/175/arenv/arwork/gsk1278863/mid209676/present_2020_01/code/CGM/cgm_data2.sas7bdat") %>%
  filter(trtcd==3 & VISITNUM == 21)

quant <- cgm3 %>% group_by(CGMTIME) %>%
  do(quant = quantile(.$aval, probs = seq(0.2,0.8,.05)), probs = seq(0.2,0.8,.05)) %>%
  unnest(cols=c(quant, probs)) %>%
  mutate(delta = 2*round(abs(.5-probs)*100)) %>% 
  group_by(CGMTIME, delta) %>%
  summarize(quantmin = min(quant), quantmax= max(quant)) %>%
  mutate(hour = (CGMTIME/3600)) %>%
  filter(delta != 0)

plot3 <- ggplot() +
  geom_ribbon(data = quant, aes(x = hour, ymin = quantmin, ymax = quantmax,
                                group = reorder(delta, -delta), fill = as.numeric(delta)),
              alpha = .5) +
  geom_segment(aes(x=1, xend=24, y=180, yend=180), linetype = 2, color = "blue") +
  scale_x_continuous("Time",
                     breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24),
                     limits=c(1, 24)) +
  scale_y_continuous(" ",
                     breaks=c(100, 150, 200),
                     limits=c(100, 200),
  ) +
  scale_fill_continuous(guide = guide_legend(direction = "horizontal", 
                                             title.position = "none",
                                             element_text(size=6))) + 

  theme_minimal() +
  theme(legend.position="none",
        plot.title = element_text(hjust = 0.5, size = 25),
        text = element_text(size = 15),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 15, color = 'black'),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 15),  
        legend.text=element_text(size=6),
        legend.title=element_text(size=15),
        legend.key.size = unit(1, "cm"),        
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  ggtitle(label = "High Dose")

#####################################################################################################

p <- grid.arrange(arrangeGrob(plot0, ncol=1, nrow=1),
                  arrangeGrob(plot1, ncol=1, nrow=1),
                  arrangeGrob(plot2, ncol=1, nrow=1),
                  arrangeGrob(plot3, ncol=1, nrow=1),
                  heights = c(1,1))

title <- ggdraw() + draw_label("Increasing Dose Leads To Improved Glucose Stability at Week 52", size = 25)

p2 <- plot_grid(title, p, ncol=1, rel_heights = c(1, 10))  

ggsave("/shared/175/arenv/arwork/gsk1278863/mid209676/present_2020_01/code/CGM/cgm_plot_quantiles.png", p2, width=18, height=12, dpi=300)

