library(tidyverse)
library(stringr)
library(lubridate)
library(gridExtra)
library(readr)
library(ggpubr)
library(grid)
library(plotly)
library(ggdark)

df <- read_csv("C:/Users/AW0204TU/Desktop/TidyTuesday/disease_data.csv")

# Records of the year for which each vaccine was introduced
vac_dat <- data.frame(disease = c("MEASLES", "POLIO", "RUBELLA"), 
                      Z = c(1963, 1955, 1969))

df %>%
  na.omit() %>%
  ggplot() +
  geom_tile(aes(x = year, y = state, fill = incidence)) +
  scale_fill_gradient2(trans = "log10", high = "red", low = "blue", 
                       mid = "white", midpoint = 0, na.value = "white") +
  scale_x_continuous(expand = c(0.02, 0.02)) +
  geom_vline(data = vac_dat, aes(xintercept = Z), linewidth = 1.5, color = "darkred") +
  facet_wrap(~disease, scales = "free") +
  dark_theme_minimal() +
  theme(axis.text.y =  element_text(size = 5), 
        plot.title = element_text(hjust = 0.5, size = 16)) +
  labs(y = "State", fill = "Log \nIncidence", x = "Year", 
       title = "Visulaizing Effect of Vaccines",
       caption = "Dark Red Line Indicates Introduction of Vaccine")

