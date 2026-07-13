# ---- Load libraries ----
library(tidyverse)

# ---- Load data ----
data <- read_csv("simulated_MBC2_data.csv")

# ---- Summarize data by arm and time ----
summary_data <- data %>%
  group_by(arm, time) %>%
  summarise(
    mean_stress = mean(stress, na.rm = TRUE),
    se_stress   = sd(stress, na.rm = TRUE) / sqrt(n()),
    mean_sleep  = mean(sleep, na.rm = TRUE),
    se_sleep    = sd(sleep, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# ---- Reshape for faceted plotting ----
summary_long <- summary_data %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "outcome",
    values_to = "mean"
  ) %>%
  mutate(
    outcome = recode(outcome,
                     mean_stress = "Stress",
                     mean_sleep  = "Sleep")
  )

y_limits <- summary_long %>%
  group_by(outcome) %>%
  summarise(max_y = max(mean + 1.5*se_stress, na.rm = TRUE), .groups = "drop")

# Simplest approach: use the overall maximum to keep same scale
overall_max <- max(y_limits$max_y)
# ---- Faceted plot for Stress and Sleep ----
p <- ggplot(summary_long, aes(x = time, y = mean, group = arm, colour = arm)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(
    title  = "Study Outcomes Over Time by Arm",
    x      = "Time (months)",
    y      = "Mean Value",
    colour = "Study Arm"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(limits = c(0, overall_max), expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 20) +
  theme(
    panel.background   = element_rect(fill = "white", color = NA),  # faint gray panel background
    strip.background   = element_rect(fill = "white", color = NA),  # faint gray facet label background
    axis.title         = element_text(size = 22),
    axis.text          = element_text(size = 20),
    legend.title       = element_text(size = 20),
    legend.text        = element_text(size = 18),
    panel.grid.major   = element_line(color = "gray85"),
    panel.grid.minor   = element_line(color = "gray90")
  )

ggsave(p, filename = "WW2020604a.png", width = 16, height = 16)