### Purpose: create TTE KM plots

### Load packages
library(readr)
library(survival)
library(survminer)
library(stringr)

### Load data
setwd('C:/Users/XXXXXXXXXXXXX')
ADTTE <- read_delim('2020-04-08-psi-vissig-adtte.csv', delim=';')

### AVAL is given in days; transform into weeks
ADTTE$AVAL_wk = ADTTE$AVAL / 7

### Create survival curves
surv_curve <- survfit(Surv(AVAL_wk, CNSR == 0) ~ TRT01P, data = ADTTE)

### Adjust treatment group labels in surv_curve
names(surv_curve$strata) <- str_remove(names(surv_curve$strata), "TRT01P=") 

### Theme for plots
theme_table <- theme_minimal(base_size = 18) %+replace% 
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),  # remove vertical gridlines
        panel.grid.major.y = element_blank(),  # remove horizontal gridlines
        axis.title.y = element_blank(), axis.title.x = element_blank(),  # remove axis titles
        axis.text.x = element_blank(), axis.ticks.x = element_blank())  # remove x-axis text and ticks

theme_gg <- theme_minimal(base_size = 18) %+replace% 
  theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),  # remove minor gridlines
        legend.title = element_blank())  # remove legend title

### Plot 
plots <- list()
# Proportion event-free
plots[[1]] <- ggsurvplot(fit = surv_curve,  # survfit object
                         data = ADTTE,  # data used to fit survival curves. 
                         break.y.by = 0.2,  # y-axis breaks
                         risk.table = T,  # show risk table.
                         conf.int = T,  # show confidence intervals
                         xlim = c(0, 160),  # present narrower X axis
                         break.time.by = 20,  # x-axis breaks
                         ggtheme = theme_gg,  # use theme defined above
                         risk.table.y.text = F, # show bars instead of names in text annotations in legend of risk table
                         xlab = "Week",
                         ylab = "Proportion event-free",
                         legend = c(0.7, 0.15),  # legend position
                         tables.theme = theme_table)  # use them defined above for risk table

# Cumulative proportion with event
plots[[2]] <- ggsurvplot(fit = surv_curve,
                         data = ADTTE,
                         fun = "event",  # show event probability instead of survival probability
                         ylim = c(0, 0.6),  # do not include full scale (0-1) to allow to better discriminate between treatments
                         break.y.by = 0.1,
                         cumevents = T,  # show cumulative number of events
                         conf.int = F,
                         xlim = c(0, 160),
                         break.time.by = 20,
                         ggtheme = theme_gg,
                         cumevents.y.text = FALSE,
                         xlab = "Week",
                         ylab = "Cumulative proportion with event",
                         legend = "none",
                         tables.theme = theme_table)

# Save plot
ggsave(filename = "2020-04-08-markusv_km.png",
       plot = arrange_ggsurvplots(plots, print = T, ncol = 2, nrow = 1),
       width = 19.3, height = 9, units = "in")
