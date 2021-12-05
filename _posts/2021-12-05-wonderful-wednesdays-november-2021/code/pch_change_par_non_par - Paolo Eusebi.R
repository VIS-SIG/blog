# Data and packages ----
library(readr)
library(ggplot2)
library(dplyr)
library(cowplot)
library(ggpubr)

urlfile <- "https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2021/2021-10-13/WWW_example_minimal_clinical_improvement.csv"

df <- read_csv(url(urlfile)) %>%
  mutate(change=`total score follow up`-`total score baseline`,
         change_pct=(`total score follow up`-`total score baseline`)*100/`total score baseline`) %>%
  rename(CGI_I=`CGI-I`)

# Bootstrapping ECDFs ----
set.seed(1234)
nboot <- 1000
boot <- NULL
for (i in 1:nboot){
  d <- df[sample(seq_len(nrow(df)), nrow(df), replace=T), ]
  d$sim <- i
  boot <- rbind(boot, d)
}

res1 <- boot %>%
  group_by(CGI_I, sim) %>%
  summarise(change_pct_median=mean(change_pct)) %>%
  ungroup() %>%
  group_by(CGI_I) %>%
  summarise(boot_median = median(change_pct_median),
            boot_lo95ci = quantile(change_pct_median, 0.025),
            boot_hi95ci = quantile(change_pct_median, 0.975)) 
res1

res2 <- boot %>%
  group_by(CGI_I) %>%
  summarise(boot_lo95pi = quantile(change_pct, 0.025),
            boot_hi95pi = quantile(change_pct, 0.975)) 
res2

res3 <- df %>%
  group_by(CGI_I) %>%
  summarise(n = n(),
            mean = mean(change_pct),
            sd = sd(change_pct),
            lo95pi = mean - 1.96*sd,
            hi95pi = mean + 1.96*sd,
            lo95ci = mean - 1.96*(sd/sqrt(n)),
            hi95ci = mean + 1.96*sd/sqrt(n)) 
res3

chg <- res1 %>%
  left_join(res2) %>%
  left_join(res3)
chg

no_chg <- res1 %>%
  left_join(res3) %>%
  filter(CGI_I==4) %>%
  select(lo95ci, hi95ci, lo95pi, hi95pi) %>%
  right_join(tibble(id=1:7), by = character())
no_chg

# Final plot ----
## CDFs
p1 <- ggplot() +
  stat_ecdf(aes(x=change_pct, group=as.factor(sim)), data = boot, colour = alpha("deepskyblue4", 0.2)) +
  stat_ecdf(aes(x=change_pct), data = df, size = 1.25, color = "orange2") +
  scale_x_continuous(limits = c(-80, 80)) +
  facet_grid(rows = vars(CGI_I))+
  labs(title = "Cumulative Distribution Functions of Score % Changes",
       subtitle = "Empirical CDF (Orange) with 1000 Bootstrap Samples (Blue)",
       y = "Cumulative Distribution Function",
       x = "Change %") +
  theme_minimal() +
  theme(strip.text = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12)) 
p1


labels_cgii <- as_labeller(c(`1` = "1\nVery Much\nImproved", 
                             `2` = "2\nMuch\nImproved",
                             `3` = "3\nMinimally\nImproved", 
                             `4` = "4\nNo Change",
                             `5` = "5\nMinimally\nWorse",
                             `6` = "6\nMuch\nWorse", 
                             `7` = "7\nVery Much\nWorse"))

## Forest plot
p2 <- ggplot(data=chg) +
  
  geom_rect(data=no_chg, aes(ymin=0, ymax=1,
                             xmin=lo95ci, xmax=hi95ci),
            fill="orange2", alpha=0.025) +
  
  geom_point(aes(x = boot_median, y=0.6),
             size = 4, color = "deepskyblue4") +
  geom_errorbarh(aes(xmin = boot_lo95ci, xmax = boot_hi95ci, y = 0.6), 
                 height = 0.1, size=1.25, color = "deepskyblue4") +
  geom_errorbarh(aes(xmin = boot_lo95pi, xmax = boot_hi95pi, y = 0.6), 
                 height = 0.0, size=0.75, color = "deepskyblue4", linetype="dotted") +
  
  geom_point(aes(x = mean, y = 0.4), 
             size = 4, color = "orange2") +
  geom_errorbarh(aes(xmin = lo95ci, xmax = hi95ci, y = 0.4), 
                 height = 0.1, size=1.25, color = "orange2") +
  geom_errorbarh(aes(xmin = lo95pi, xmax = hi95pi, y = 0.4), 
                 height = 0.0, size=0.75, color = "orange2", linetype="dotted") +

  facet_grid(rows = vars(CGI_I), labeller = labels_cgii)+
  ylim(0, 1) +
  theme_minimal() +
  labs(title = "Estimates, 95% Confidence Intervals and 95% Prediction Intervals",
       subtitle = "Parametric (Orange) and with 1000 Bootstrap Samples (Blue)",
       x = "Change %", 
       y = "") +
  theme(strip.text.y = element_text(size = 14, angle = 0),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12)) +
  coord_cartesian(xlim = c(-50, 50), ylim=c(0,1), expand=F, clip = "on")
p2


## Legend
dummy1 <- chg[chg$CGI_I==4,]
dummy2 <- chg[chg$CGI_I==5,]

tl<-0.015 # tip.length
l1 <- ggplot(data=dummy2) +
  labs(title = "Legend") +
  geom_rect(data=dummy1, aes(ymin=0, ymax=1, xmin=lo95ci, xmax=hi95ci),
            fill="orange2", alpha = 0.15) +  
  geom_bracket(xmin=dummy1$lo95ci, xmax=dummy1$hi95ci,
               y.position = 0, 
               label = "95% CI - No Change", tip.length = c(-tl, -tl)) + 
  geom_point(aes(x = boot_median, y=0.75), 
             size = 4, color = "deepskyblue4") +
  geom_errorbarh(aes(xmin = boot_lo95ci, xmax = boot_hi95ci, y = 0.75),
                 height = 0.1, size=1.25, color = "deepskyblue4") +
  geom_errorbarh(aes(xmin = boot_lo95pi, xmax = boot_hi95pi, y = 0.75), 
                 height = 0.0, size=0.75, color = "deepskyblue4", linetype="dotted") +
  geom_bracket(xmin=dummy2$lo95ci, xmax=dummy2$hi95ci, y.position = 0.35,
               label = "95% Confidence Interval", tip.length = c(tl, tl)) +
  geom_bracket(xmin=dummy2$mean, xmax=dummy2$mean, y.position = 0.3,
               label = "Estimate", tip.length = c(tl, tl)) +
  geom_bracket(xmin=dummy2$lo95pi, xmax=dummy2$hi95pi, y.position = 0.15, vjust = 4,
               label = "95% Prediction Interval", tip.length = c(-tl, -tl)) +
  geom_point(aes(x = mean, y = 0.25), 
             size = 4, color = "orange2") +
  geom_errorbarh(aes(xmin = lo95ci, xmax = hi95ci, y = 0.25), 
                 height = 0.1, size=1.25, color = "orange2") +
  geom_errorbarh(aes(xmin = lo95pi, xmax = hi95pi, y = 0.25), 
                 height = 0.0, size=0.75, color = "orange2", linetype="dotted") +
  geom_bracket(xmin=dummy2$boot_lo95ci, xmax=dummy2$boot_hi95ci,
               y.position = 0.85,
               label = "95% Confidence Interval (Bootstrap)", tip.length = c(tl, tl)) +
  geom_bracket(xmin=dummy2$boot_median, xmax=dummy2$boot_median,
               y.position = 0.8,
               label = "Estimate (Bootstrap)", tip.length = c(tl, tl)) +
  geom_bracket(xmin=dummy2$boot_lo95pi, xmax=dummy2$boot_hi95pi, 
               y.position = 0.65, vjust = 4,
               label = "95% Prediction Interval (Bootstrap)", tip.length = c(-tl, -tl)) + 
  coord_cartesian(xlim = c(-30, 50), ylim=c(0,1)) +
  theme_void() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        plot.subtitle = element_text(size = 12))
l1

pnull <- ggplot() +
  coord_cartesian(xlim = c(-30, 50), ylim=c(0,1)) +
  theme_void()
  
legend <- plot_grid(l1, pnull, ncol=1,
                    rel_heights = c(0.5, 0.5))

## Title
title <- ggdraw() + 
  draw_label(
    "Exploring uncertainty of minimally clinically relevant changes",
    fontface = 'bold', size = 36,
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
title

plot_row <- plot_grid(p1, p2, pnull , legend, nrow=1, rel_widths = c(3, 3, 0.25, 1.25))
g2 <- plot_grid(title, plot_row, ncol = 1, rel_heights = c(0.1, 1))
g2
f<-1.7
ggsave(file="plot_par_nonpar.pdf", g2, width = 16*f, height = 9*f)
