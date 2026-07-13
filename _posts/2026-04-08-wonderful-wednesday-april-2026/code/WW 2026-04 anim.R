library(tidyverse)
library(gganimate)
library(gifski)
library(transformr)

# Load data
data <- read_csv("simulated_MBC2_data.csv")

# Summarize by arm and time
summary_data <- data %>%
  group_by(arm, time) %>%
  summarise(
    mean_stress = mean(stress, na.rm = TRUE),
    se_stress = sd(stress, na.rm = TRUE) / sqrt(n()),
    mean_sleep = mean(sleep, na.rm = TRUE),
    se_sleep = sd(sleep, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# Pivot to long format for faceting and animation
summary_long <- summary_data %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "outcome",
    values_to = "mean"
  ) %>%
  mutate(
    outcome = recode(outcome,
                     mean_stress = "Stress",
                     mean_sleep = "Sleep")
  )

# Base plot
p_anim <- ggplot(summary_long, aes(x = time, y = mean, colour = arm, group = arm)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - ifelse(outcome=="Stress", se_stress, se_sleep),
                    ymax = mean + ifelse(outcome=="Stress", se_stress, se_sleep)),
                width = 0.1) +
  facet_wrap(~ outcome, scales = "free_y") +
  scale_color_manual(values = c("Sleep/Stress" = "#1b9e77",
                                "Diet/Activity" = "#d95f02")) +
  labs(
    title = "Study Outcomes Over Time",
    x = "Time",
    y = "Mean value",
    colour = "Study Arm"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  ) +
  # Animation
  transition_reveal(time) +
  ease_aes('linear')

# Animate
animate(p_anim, nframes = 100, fps = 10, width = 800, height = 600, renderer = gifski_renderer("animated_plot.gif"))
