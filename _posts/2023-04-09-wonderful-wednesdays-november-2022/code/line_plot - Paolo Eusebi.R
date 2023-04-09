library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

d0 <- read.csv2("ww eortc qlq-c30 missing.csv", sep=",") %>%
  as_tibble() 
d0
d1 <- df %>%
  pivot_longer(cols=starts_with("WEEK"), names_to = "AVISIT", values_to = "AVAL") %>%
  mutate(AVAL=as.numeric(AVAL)) %>%
  select(USUBJID, ARM, LASTVIS, AGE:AVAL)
d1  

d2 <- d1 %>%
  group_by(ARM, LASTVIS, AVISIT) %>%
  summarize(AVAL = mean(AVAL, na.rm=TRUE)) %>%
  mutate(LASTVISC=as.factor(paste("Week", sprintf("%02.f", LASTVIS))),
         AVISITN = as.numeric(gsub("WEEK","",AVISIT))) %>%
  mutate(LASTVISC=fct_reorder(LASTVISC, LASTVIS))


cc <- scales::seq_gradient_pal("yellow", "blue", "Lab")(seq(0,1,length.out=14))
show_col(cc)

breaks <- sort(names(table(df_2$LASTVISC)))
labels <- breaks

ggplot(data=d2, aes(x=AVISITN, y=AVAL, group=LASTVISC, color=LASTVISC)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = round(seq(0, 100, 8.333333333),2),
                     limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(0, 48, 3), labels = paste("Wk", seq(0, 48, 3))) +

  scale_color_manual(values = cc, labels=labels, breaks=breaks) +
  facet_grid(cols=vars(ARM)) +
  labs(title = "Dropout is associated with worsening in quality of life",
     y = "EORTC QLQ-C30 QL [0-100]",
     x = "Week on treatment") +
  theme(plot.background = element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        legend.background = element_rect(fill="black"),
        legend.box.background = element_rect(fill="black"),
        legend.key = element_blank(),
        legend.text = element_text(colour="grey"),
        panel.grid = element_line(colour="grey5"),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        plot.title=element_text(colour = "grey", size = 14, face = "bold"),
        strip.text = element_text(colour = "grey50", size = 10),
        axis.text = element_text(angle = 90))

ggsave(filename = "line_plot.png", device = "png", width = 12, height = 6)
