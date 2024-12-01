library(dplyr)
library(ggplot2)
library(broom)
library(survival)
library(patchwork)

# load data
ADTTE <- read_csv('2024-08-12-psi-vissig-adtte.csv') %>%
  mutate(EVNTDESCN=case_when(
    EVNTDESC=="Death" ~ 1,
    EVNTDESC=="PD" ~ 2,
    EVNTDESC=="Lost to follow-up" ~ 3,
    EVNTDESC=="No next-line therapy initiated" ~ 4,
    EVNTDESC=="Ongoing on first next-line therapy" ~ 5,
    EVNTDESC=="Second next-line therapy initiated" ~ 6))

# plot KM curve by treatment 
d <- survfit(Surv(AVAL, CNSR == 0) ~ TRT01P  , data = ADTTE )  %>%
  tidy(fit) %>%
  rename(TRT01P=strata) %>%
  mutate(TRT01P=gsub('TRT01P=', '', TRT01P))

a <- ggplot(data=d, aes(x=time, y=estimate, group = TRT01P, colour = TRT01P)) +
  # do not inherit legend from ggplot
  geom_line(show.legend = T) +
  geom_point(pch=ifelse(d$n.censor>0,"|","")) +
  theme_bw() +
  theme(legend.position="top",
        legend.text = element_text(size=12)) +
  guides(colour = guide_legend(title = "")) +
  labs(y="", x="Time (days)") +
  scale_color_brewer(palette="Set1")+
  annotate("text", x=-250, 0.35, label = "Progression Free Survival", hjust = 0, vjust = 1, angle=90) +
  coord_cartesian(xlim = c(0, 2000),  clip = 'off')
a

b <- ggplot(data = ADTTE, aes(x=AVAL, y=reorder(EVNTDESC, -EVNTDESCN), colour=TRT01P)) + 
  geom_point(pch=ifelse(ADTTE$CNSR==1, "|", "x"), size=3, alpha=0.5) +
  facet_grid(cols = vars(TRT01P)) +
  theme_bw() +
  theme(legend.position="none") +
  labs(y="", x="Time (days)") +
  scale_color_brewer(palette="Set1")
b

a/b +
  plot_layout(heights = c(2, 1))

ggsave("figures/survplot_v1_2024-09-07.png", width = 16, height = 9)
