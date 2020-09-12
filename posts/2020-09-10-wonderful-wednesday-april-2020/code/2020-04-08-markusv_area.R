### Purpose: create TTE area plot

### Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(zoo)

### Load data
setwd('C:/Users/XXXXXXX')
ADTTE <- read_delim('2020-04-08-psi-vissig-adtte.csv', delim=';')

### AVAL is given in days; transform into weeks
ADTTE$AVAL_wk = ADTTE$AVAL / 7

### Derive number of subjects per treatment group
ADTTE_ntrt <- ADTTE %>% group_by(TRT01PN, TRT01P) %>% mutate(N=n())

### Derive cumulative number (percentage) of subjects with event / censoring
ADTTE_cum <- ADTTE_ntrt %>% group_by(TRT01PN, TRT01P, N, EVNTDESC, AVAL_wk) %>% 
  summarise(n = n()) %>% mutate(n = cumsum(n), pct = n / N)

### Create grid to ensure all groups have a value at each timepoint (necessary for geom_area())
grid <- with(ADTTE_cum, expand.grid(TRT01PN = unique(TRT01PN), EVNTDESC = unique(EVNTDESC), 
                                    AVAL_wk = unique(AVAL_wk)))
ADTTE_grid <- merge(grid, ADTTE_cum, by = c("TRT01PN", "EVNTDESC", "AVAL_wk"), all = T)

### Fill in NA values with last non-NA value and remove leading NA rows
ADTTE_locf <- ADTTE_grid %>% group_by(TRT01PN, EVNTDESC) %>% do(na.locf(.))

### Control order (show events at bottom and censoring above)
ADTTE_locf$EVNTDESC <- factor(ADTTE_locf$EVNTDESC, levels = c("Lost to follow-up", "No next-line therapy initiated",
                                                              "Second next-line therapy initiated",
                                                              "Ongoing on first next-line therapy", "PD", "Death"))

### Add N to treatment label and order by TRT01PN
ADTTE_locf$TRT01P_label <- with(ADTTE_locf, paste0(TRT01P, " (N=", N, ")"))
ADTTE_locf$TRT01P_label <- reorder(ADTTE_locf$TRT01P_label, ADTTE_locf$TRT01PN)

### Stacked area chart
ggplot(data = ADTTE_locf, aes(x = AVAL_wk, y = pct, fill = EVNTDESC)) +
  geom_area(alpha = 0.6) +
  facet_wrap(TRT01P_label ~ .) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 160, by = 20), name = "Week") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2), 
                     name = "Cumulative proportion with event / censoring", expand = c(0, 0)) +
  geom_vline(xintercept = 52, alpha = 0.3) +
  theme_minimal(base_size = 18) +
  scale_fill_grey(start = 0.8, end = 0.2) +
  theme(legend.position = c(0.15, 0.85), legend.title = element_blank(),  # remove legend title and set legend position
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),  # remove vertical gridlines
        panel.grid.minor.y = element_blank()) +  # remove horizontal minor gridlines
  ggtitle("Combination of tablemab + vismab leads to less deaths during first 52 weeks of study treatment")

ggsave(filename = "2020-04-08-markusv_area.png", width = 19.3, height = 9, units = "in")
