####################################################################
# Program name: COPD_exacs_STEPP.R
# Purpose: To produce a STEPP plot for COPD exacerbations 
#          examining age and FEV1
#         (for Wonderful Wednesdays Aug 2020)
# Written by: Steve Mallett
# Date: 31-Jul-2020
####################################################################

library(haven)
library(ggplot2)
library(grid)
library(gridExtra)

copd_age <- read_sas("/.../plot_ww2020_08.sas7bdat") %>%
  filter(var == "age_n")
  
plot01 <- ggplot() +
  geom_ribbon(data = copd_age, aes(x = median, ymin = LowerExp, ymax = UpperExp), fill = "#b2e2e2", alpha = .5) +
  geom_line(data = copd_age, aes(x = median, y = ExpEstimate), color = "#006d2c", size = 0.5 ) +  
  geom_segment(aes(x=53, xend=72, y=0.82, yend=0.82), linetype = 2, color = "black") +
  scale_x_continuous("Age (years)",
                     # breaks=c(55, 60, 65, 70),
                     # labels=c(55, 60, 65, 70),
                     limits=c(53, 72)) +
  scale_y_log10(" ",
                breaks=c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6),
                limits=c(0.4, 1.6)) +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 15),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x =  element_text(size = 20),  
        axis.text.y =  element_text(size = 20),        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 25),
        panel.border = element_rect(colour = "black", fill=NA, size=0.25),
        # panel.grid = element_blank(),
        plot.margin=unit(c(0,0,0,0),"cm")) +
  ggtitle(label = " ")

copd_fev <- read_sas("/shared/175/arenv/arwork/gsk1278863/mid209676/present_2020_01/code/COPD/plot_ww2020_08.sas7bdat") %>%
  filter(var == "fev1_rv")

plot02 <- ggplot() +
  geom_ribbon(data = copd_fev, aes(x = median, ymin = LowerExp, ymax = UpperExp), fill = "#bdd7e7", alpha = .5) +
  geom_line(data = copd_fev, aes(x = median, y = ExpEstimate), color = "#08519c", size = 0.5 ) +  
  # geom_hline(yintercept = 0.82, linetype = 2, color = "black", size = 0.5) +
  geom_segment(aes(x=34, xend=60, y=0.82, yend=0.82), linetype = 2, color = "black") +
  scale_x_continuous("FEV1 (% predicted)",
                     breaks=c(34, 40, 45, 50, 55, 60),
                     labels=c(35, 40, 45, 50, 55, 60),
                     limits=c(34, 60)) +
  scale_y_log10(" ",
                breaks=c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6),
                limits=c(0.4, 1.6)) +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 15),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x =  element_text(size = 20),  
        axis.text.y =  element_text(size = 20),        
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 25),
        panel.border = element_rect(colour = "black", fill=NA, size=0.25),
        # panel.grid = element_blank(),
        plot.margin=unit(c(0,0,0,0),"cm")) +
  ggtitle(label = " ")

g <- grid.arrange(plot01, plot02, ncol =1, nrow = 2,
                  left = textGrob("Rate Ratio (ICS+LABA / LABA)", rot = 90, gp = gpar(fontsize=20)),
                  top = textGrob("ICS+LABA Reduces COPD Exacerbation Rate by 18 Percent Compared\n to LABA, Consistently Across Age and Lung Function Values", gp = gpar(fontsize=25)),
                  bottom = textGrob("\nAnalysis performed using a negative binomial regression model adjusting for number of previous \nexacerbations and region. Sliding window STEPP plots, using overlapping subgroups (N=350).", gp = gpar(fontsize=18)))

ggsave("/.../plot_ww2020_08.png", g, width=12, height=12, dpi=300)
