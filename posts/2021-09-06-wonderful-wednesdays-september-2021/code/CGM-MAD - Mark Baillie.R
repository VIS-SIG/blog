#-----------------------------------------------------------------------------
# Plot a model averaged patient profile
# Warning. The proposed model does not take in to account patients and shouldn't
# be believed, but is used for illustrative purposes. 
#-----------------------------------------------------------------------------

library(tidyverse)
library(readr)


simulated_data <- read_csv("simulated_data.csv")

simulated_data %>% glimpse()

simulated_data %>%
  mutate(TREATMENT = factor(TREATMENT, levels=c("SOC", "Rx low", "Rx medium", "Rx high")),
         VISIT = case_when(
           VISITNUM == 3 ~ "Baseline",
           VISITNUM == 17 ~ "Week 26",
           VISITNUM == 21 ~ "Week 52")
  ) %>%
  ggplot(aes(x = CGMTIME, y = Original_CGMValue, colour = TREATMENT, group = TREATMENT)) +
  geom_smooth(se = TRUE) +
  scale_y_continuous(trans = "log") +
  facet_grid(VISIT ~ TREATMENT ) +
  theme_light(base_size = 12) +
  #theme_dark() +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Glucose mg/dL") +
  labs(title = "Medium dose Rx dose appears to stabilise glucose over 24 period compared to SoC",
       subtitle = "Model averaged patient profiles by treatment and visit",
       caption = "A generalised additive model was used to provide an averaged patient profile within treatment and visit. \n
       Additional development of a flexible model of 24-hour patient CGM is required.") +
  scale_color_brewer(palette="Dark2")

ggsave("MAD-CGM.png", width = 12, height = 8)
