library(tidyverse)
library(ggtext)

inc <- read_csv('vaccine_data.csv') 

measles <- inc %>%
  filter(disease == "MEASLES") %>%
  mutate(inc_cat = cut(incidence, c(0, 200, 400, 600, 800, 1000, 1200, Inf))) %>%
  mutate(inc_fac = factor(inc_cat)) 

ggplot(data=measles) +
  geom_tile(aes(x=year, y=state, fill=inc_fac), color="gray") +
  scale_x_continuous("Year", breaks=seq(1930, 2010, by=10), limits = c(1928, 2000)) +
  scale_y_discrete("State", limits=rev) +
  geom_vline(xintercept = 1963, size = 1, color = "#d7191c") +
  scale_fill_brewer("Incidence\nPer\n100,0000\nPopulation", 
                    labels=c("0-<200", "200-<400", "400-<600", "600-<800", "800-<1000", "1000-<1200", ">=1200"),
                    na.value = 'white',
                    na.translate = F) +
  labs (title ="Incidence of measles in US states declines after <b style='color:#d7191c;'>vaccine approval</b>")+
  theme_minimal() +
  theme(legend.title.align = 0.5,
        axis.text.x = element_text(size = 12, color = "#525252"),
        axis.text.y = element_text(size = 8, color = "#525252"),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_markdown())



  