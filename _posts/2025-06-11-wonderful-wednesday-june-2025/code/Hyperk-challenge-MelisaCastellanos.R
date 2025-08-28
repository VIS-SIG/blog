#############################################################################

#       Wonderful Wednesday Challenge - May 14 - Melisa Castellanos

#############################################################################

# https://github.com/VIS-SIG/Wonderful-Wednesdays/blob/master/data/2025/2025-05-14/fig2data.csv

library(readr)
library(dbplyr)
library(tidyverse)
library(ggplot2)

data <- read_csv(
  "C:/Users/castelme/OneDrive - Boehringer Ingelheim/Documents/R/Hyperkalemia challenge/fig2data.csv")
data

data_wide_summary <- data %>%

  select(visit, treat, stat, serumK) %>%
  pivot_wider(
    id_cols = c(visit, treat),    # Columns that uniquely identify each row in the new wide format
    names_from = stat,            # The column whose values ("llc", "ulc", "mean") will become new column names
    values_from = serumK) 

data_wide_summary

# plot

ggplot(data_wide_summary, aes(x = factor(visit, level = c("Baseline", "1st week","2nd week","4th week","6th week","8th week")), 
   y = mean, 
   group = treat, 
   color = treat)) +
   geom_ribbon(aes(ymin=llc, ymax=ulc, fill = treat), alpha = 0.3, linetype = 0) +
   geom_line(size = 1.5) +
   geom_hline(yintercept = 5, linetype = "dashed", color = "gray", size = 1) +
   labs(
   title = "Serum potassium changes. Comparison of two treatments for hyperkalemia.",
   subtitle = "Data from Elsayed et al. (2025)",
   caption = "Plotted by Melisa Castellanos for ~Wonderful Wednesday Challenge~ by PSI.
              SCZ = sodium zirconium cyclosilicate. SPS = sodium polystyrene sulfonate.
              LLC = lower limit of confidence. ULC = Upper limit of confidence. 
              In this new visualization, the treatment arms are shown within the same plot making comparison easier.",
   x = "Visit",
   y = "Serum K (mEq/L)",
   fill = "LLC - ULC",
   color = "Treatment Arm Mean"
   ) +
  theme_minimal() +
  scale_color_manual(values = c("SZC group" = "darkblue", # Line of treatment 1
                                "SPS group" = "darkgreen")) + # line treatment 2
  scale_fill_manual(values = c("SZC group" = "lightblue", # A lighter shade of blue for SZC group's band
                               "SPS group" = "lightgreen"))  # A lighter shade of red for SPS group's band
  

                   


