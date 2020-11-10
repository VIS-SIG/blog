library(tidyverse)
library(dplyr)
library(tidyr)
library(mediation)
library(grid)
library(gridExtra)
library(haven)
library(ggplot2)
library(cowplot)

data <- read_csv("/shared/175/arenv/arwork/gsk1278863/mid209676/present_2020_01/code/mediation/TRT.csv")

# Summarise data
sum <- data %>%
  group_by(TRT) %>%
  summarise(avg=mean(DLQI))
sort <- c(1,2)
sum <- cbind(sum, sort)

# Plot Mean DLQI
plot01 <- ggplot(data=sum) +
  geom_text(aes(x=-5, y=sort, label=TRT, color=TRT), size=10) +
  geom_segment(aes(x=0, xend=avg, y=sort, yend=sort, color=TRT)) +
  geom_vline(aes(xintercept=0)) + 
  geom_point(aes(x=avg, y=sort, color=TRT), alpha=0.7, size=10) +
  scale_x_continuous("Mean DLQI at Week 24",
                     labels=c(" ", "0", "10", "20", "30"),
                     breaks=c(-10, 0, 10, 20, 30),
                     limits=c(-10, 30)) +
  scale_y_continuous(" ",
                     labels=c(" ", " ", " "),
                     breaks=c(1,2,3),
                     limits=c(0, 3)) +
  labs(title="Rx reduces DLQI by approx. 5 units. The treatment effect is \n
              reduced by approx. 4 units when Itch is added to the model. \n
              Therefore the treatment effect of Rx is mediated primarily by itch.\n") +
  theme_minimal() +
  theme(legend.position="none",
        text = element_text(size = 20),
        axis.ticks.x = element_blank(),

        axis.text.y =  element_blank(),        
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 25),
        panel.border = element_rect(colour = "black", fill=NA, size=0.25),
        panel.grid = element_blank())

# Mediation analysis (itch)
model.I <- lm(itch ~ TRT, data)
model.Yi <- lm(DLQI ~ TRT + itch, data)
med.itch <- mediate(model.I, model.Yi, treat='TRT', mediator='itch',
                   boot=TRUE, sims=500)
itch <- (med.itch$d0)*(-1)

# Mediation analysis (BSA)
model.B <- lm(BSA ~ TRT, data)
model.Yb <- lm(DLQI ~ TRT + BSA, data)
med.BSA <- mediate(model.B, model.Yb, treat='TRT', mediator='BSA',
                    boot=TRUE, sims=500)
BSA <- (med.BSA$d0)

# Mediation analysis (redness)
model.R <- lm(redness ~ TRT, data)
model.Yr <- lm(DLQI ~ TRT + redness, data)
med.redness <- mediate(model.R, model.Yr, treat='TRT', mediator='redness',
                   boot=TRUE, sims=500)
redness <- (med.redness$d0)

# Combine ACME values
sort <- c(1, 2, 3)
V1 <- " "
A <- data.frame(cbind(itch, BSA, redness))
A2 <- gather(A, var, val)
A3 <- cbind(A2, sort, V1)

# Overall treatment effect
var = "Effect"
sort = 1
coeff <- data.frame(cbind(summary(model.0 <- lm(DLQI ~ TRT, data))$coefficients[2,1]*(-1), var)) %>%
  mutate(val=as.numeric(V1)) 
coeff2 <- cbind (coeff, sort)

# Plot treatment effect
plot02 <- ggplot() +
  geom_vline(aes(xintercept=0)) +  
  geom_text(data=coeff2, aes(x=-0.75, y=sort), label="Rx Effect", color="blue", size=10) +
  geom_segment(data=coeff2, aes(x=0, xend=val, y=sort, yend=sort), color="blue") +
  geom_point(data=coeff2, aes(x=val, y=sort), color="Blue", alpha=0.7, size=10) +
  scale_x_continuous("Treatment Effect (placebo - Rx)",
                     labels=c(" ", "0", "1", "2", "3", "4", "5", "6"),
                     breaks=c(-1, 0, 1, 2, 3, 4, 5, 6),
                     limits=c(-1, 6)) +
  scale_y_continuous(" ",
                     labels=c(" "),
                     breaks=c(1),
                     limits=c(1)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.ticks.x = element_blank(),
        axis.text.y =  element_blank(),        
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 25),
        panel.border = element_rect(colour = "black", fill=NA, size=0.25),
        panel.grid = element_blank(),
        plot.caption=element_text(hjust = 0)) 

# Plot mediation effect
plot03 <- ggplot() +
  geom_text(data=A3, aes(x=-0.75, y=sort, label=var), color="black", size=10) +
  geom_segment(data=A3, aes(x=0, xend=val, y=sort, yend=sort)) +
  geom_point(data=A3, aes(x=val, y=sort), color="black", alpha=0.7, size=10, shape=1) +
  scale_x_continuous("Reduction in Treatment Effect (Mediation Effect)",
                     labels=c(" ", "0", "1", "2", "3", "4", "5", "6"),
                     breaks=c(-1, 0, 1, 2, 3, 4, 5, 6),
                     limits=c(-1, 6)) +
  scale_y_continuous(" ",
                     labels=c(" ", " ", " "),
                     breaks=c(1, 2, 3),
                     limits=c(0, 4)) +
  theme_minimal() +
  theme(text = element_text(size = 20),
        axis.ticks.x = element_blank(),
        axis.text.y =  element_blank(),        
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, size = 25),
        panel.border = element_rect(colour = "black", fill=NA, size=0.25),
        panel.grid = element_blank(),
        plot.caption=element_text(hjust = 0)) +
  ggtitle(label = " ") 

p <- plot_grid(plot01, plot02, plot03, align = "v", nrow = 3, rel_heights = c(1.5, 0.6, 1.2))

ggsave("/shared/175/arenv/arwork/gsk1278863/mid209676/present_2020_01/code/mediation/DLQI_mediation_mallett.png", p, width=12, height=12, dpi=300)

