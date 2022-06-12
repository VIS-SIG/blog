library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(readxl)
library(cowplot)

hgb1 <- read_excel("/.../growth.xlsx") %>%
  filter(MEASURE == "STATURE" & TRT == "Rx" & TIME != 0)

quant1 <- hgb1 %>% group_by(TIME) %>%
  do(quant = quantile(.$CFB, probs = seq(0.2,0.8,.05)), probs = seq(0.2,0.8,.05)) %>%
  unnest(cols=c(quant, probs)) %>%
  mutate(delta = 2*round(abs(.5-probs)*100)) %>% 
  group_by(TIME, delta) %>%
  summarize(quantmin = min(quant), quantmax= max(quant)) %>%
  filter(delta > 0)

# Derive median Hgb

hgb_med1 <- hgb1 %>% 
  group_by(TIME) %>% 
  summarise(hgb.median = median(CFB)) 

# Produce plot for group E

plot01 <- ggplot() +
  geom_ribbon(data = quant1, aes(x = TIME, ymin = quantmin, ymax = quantmax,
                                    group = reorder(delta, -delta), fill = factor(delta)),
              alpha = .5) +
  geom_line(data = hgb_med1, aes(x = TIME, y = hgb.median, color = "#00441b"), size = 2 ) +  
  scale_x_continuous("Time (days)",
                     breaks=c(16, 28, 40, 52, 64, 76),
                     limits=c(16, 76)) +
  scale_y_continuous(" ",
                     breaks=c(-1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5),
                     limits=c(-1.5, 1.5),
                     ) +
  scale_fill_manual("% Patients in band", values=c("#e5f5e0","#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c")) +
  scale_color_identity(name = " ",
                       guide=legend,
                       labels = " ") + 
  theme_minimal() +
  theme(legend.position="none",
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y.left =  element_text(color = 'black'),        
        plot.title = element_text(hjust = 0.5, size = 25),
        text = element_text(size = 15),
        axis.text = element_text(size = 20),
        axis.title = element_text(size = 25),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.margin=unit(c(1,0,0,0),"cm")) +
  ggtitle(label = "Treatment")

hgb2 <- read_excel("/.../growth.xlsx") %>%
  filter(MEASURE == "STATURE" & TRT == "Placebo"  & TIME != 0)


quant2 <- hgb2 %>% group_by(TIME) %>%
  do(quant = quantile(.$CFB, probs = seq(0.2,0.8,.05)), probs = seq(0.2,0.8,.05)) %>%
  unnest(cols=c(quant, probs)) %>%
  mutate(delta = 2*round(abs(.5-probs)*100)) %>% 
  group_by(TIME, delta) %>%
  summarize(quantmin = min(quant), quantmax= max(quant)) %>%
  filter(delta > 0)

hgb_med2 <- hgb2 %>% 
  group_by(TIME) %>% 
  summarise(hgb.median = median(CFB)) 

plot02 <- ggplot() +
  geom_ribbon(data = quant2, aes(x = TIME, ymin = quantmin, ymax = quantmax,
                                    group = reorder(delta, -delta), fill = factor(delta)),
              alpha = .5) +
  geom_line(data = hgb_med2, aes(x = TIME, y = hgb.median, color = "#00441b"), size = 2) + 
  geom_segment(x=17, y=1, xend=75, yend=1, color="black") +
  scale_x_continuous("Time (days)",
                     breaks=c(16, 28, 40, 52, 64, 76),
                     limits=c(16, 76)) +
  scale_y_continuous(" ",
                     breaks=c(-1.5, -1.0, -0.5, 0, 0.5, 1.0, 1.5),
                     limits=c(-1.5, 1.5),
  ) +
  scale_fill_manual("% Patients in band", values=c("#e5f5e0","#c7e9c0", "#a1d99b", "#74c476", "#41ab5d", "#238b45", "#006d2c")) +
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
        axis.text.y.left =  element_text(color = 'black'),
        legend.text=element_text(size=14),
        legend.title=element_text(size=20),
        legend.key.size = unit(1.2, "cm"),        
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  ggtitle(label = "Control")

p <- grid.arrange(arrangeGrob(plot01, ncol=1, nrow=1),
                  arrangeGrob(plot02, ncol=1, nrow=1),
                  heights = c(1,1.32))

title <- ggdraw() + draw_label("Change from Baseline in Stature (cm) Over Time\nNo Evidence of a Treatment Effect", size = 24)

p2 <- plot_grid(title, p, ncol=1, rel_heights = c(1, 10))  

gsave("/.../growth.png", p2, width=12, height=12, dpi=300)

