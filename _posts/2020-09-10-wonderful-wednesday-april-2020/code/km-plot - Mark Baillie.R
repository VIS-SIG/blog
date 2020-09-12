library(tidyverse)    
library(broom)
library(survival)
library(here)

# load data
ADTTE <- read_csv(here("data", '2020-04-08-psi-vissig-adtte.csv'))


# Set up meta data for the report
#-------------------------------------------------
title <- "Evidence of improved progression-free survival for combo over monotherapy"
subtitle <- "Kaplan-Meier estimates over time including 95% uncertainty interval"
source <- "*The number of patients at risk (and events) are displayed the time point reference.
Data source: https://github.com/VIS-SIG/Wonderful-Wednesdays/tree/master/data/2020/2020-04-08"
y_axis <- "Progression free survival"
x_axis <- "Time [days]*"


############################################
## Survival model for KM and risk set
############################################

fit <- survfit(Surv(AVAL, CNSR == 0) ~ TRT01P, data = ADTTE, conf.type = 'log-log' ) 
sumfit <- summary(fit, times = c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000)) 


# tidy KM curve by treatment for plotting
#-------------------------------------------
km <-
  survfit(Surv(AVAL, CNSR == 0) ~ TRT01P,
          data = ADTTE,
          conf.type = 'log-log')  %>%
  broom::tidy(fit) %>%
  dplyr::mutate(group = stringr::str_remove(strata, "TRT01P=")) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c(
      "tablemab + vismab 52 weeks",
      "tablemab x 12 week -> vismab 34 weeks",
      "vismab x 52 weeks",
      "tablemab x 52 weeks"
    )
  ))

# tidy KM curve by treatment for small multiples
# this is a second data set to pass in for plotting
# ghost lines of each treatment 
#-------------------------------------------
km_sm <- km %>%
  mutate(group2 = group)


# Calculate risk set for annotations
#------------------------------------------
risk_data <-
  tibble(
    time =  sumfit$time,
    group = sumfit$strata,
    n.risk =   sumfit$n.risk,
    n.events = sumfit$n.event
  ) %>%
  dplyr::mutate(
    group = stringr::str_remove(group, "TRT01P="),
    label = paste0(n.risk, " (", n.events, ")"),
    y_pos = 0.01
  )


############################################
## Create the plot
############################################

plot <-
  km %>% ggplot(aes(x = time, y = estimate, group = group)) +
  ## draw the ghost lines of each treatment by facet. Force the group facet to null
  geom_step(
    data = transform(td2, group = NULL),
    aes(time, estimate, group = group2),
    size = 0.75,
    color = "#000000",
    alpha = 0.15
  ) +
  
  ## draw km lines
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, fill = "red") +
  geom_step(color = "red") +
  
  ## draw risk set
  geom_text(data = risk_data, mapping = aes(x = time, y = y_pos, label = label, group = group, fill = NULL), size = 2.5) +
  
  ## asthetics of the plot
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000)) +
  scale_y_continuous(breaks = c(0, 0.25, 0.50, 0.75, 1), limits = c(0, 1)) +

  ## annotations
  labs(title = title,
       subtitle = subtitle,
       caption = source) +
  xlab(x_axis) +
  ylab(y_axis) +
  
  # set up basic theme 
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  
  # Set the entire chart region to a light gray color
  theme(panel.background = element_rect(fill = color_background, color = color_background)) +
  theme(plot.background = element_rect(fill = color_background, color = color_background)) +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.35)) +
  facet_wrap(~ group, scales = 'free', ncol = 2) 

# Save plot to file
#-----------------------------------------
ggsave(
  file = here("figs", "ww-km-plot.png"),
  width = 35, height = 20, units = "cm")


