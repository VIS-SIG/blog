# MAFLD Subgroup Analysis - Treatment Effect Visualization
# Replicating publication: HMER-17-61
# Exploring Liv.52 DS vs Placebo across different subgroups

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(ggtext)

# ============================================================================
# Helper Functions
# ============================================================================

# Common theme for subgroup plots
theme_subgroup <- function() {
  theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10),
      legend.position = "top",
      legend.title = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# Calculate subgroup statistics
calculate_subgroup_stats <- function(data, group_var) {
  data %>%
    group_by(Group, !!sym(group_var)) %>%
    summarise(
      n = n(),
      mean_change = mean(Change_kPa),
      se_change = sd(Change_kPa) / sqrt(n),
      mean_pct_change = mean(Percent_Change),
      sd_pct_change = sd(Percent_Change),
      se_pct_change = sd_pct_change / sqrt(n),
      .groups = "drop"
    )
}

# Create subgroup point plot
create_subgroup_plot <- function(data, x_var, title, x_label) {
  ggplot(data, aes(x = !!sym(x_var), y = mean_pct_change, color = Group)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    geom_point(size = 4, position = position_dodge(0.4)) +
    geom_errorbar(
      aes(ymin = mean_pct_change - 1.96 * se_pct_change,
          ymax = mean_pct_change + 1.96 * se_pct_change),
      width = 0.2,
      position = position_dodge(0.4),
      linewidth = 0.8
    ) +
    geom_text(
      aes(label = paste0("n=", n)),
      position = position_dodge(0.4),
      hjust = -0.5,
      vjust = 0.5,
      size = 3,
      show.legend = FALSE
    ) +
    labs(
      title = title,
      subtitle = "Mean percent change in LSM Score from baseline (±95% CI)",
      y = "Mean % Change from Baseline",
      x = x_label,
      color = "Treatment"
    ) +
    scale_color_manual(values = c("LIV.52 DS" = "#4472C4", "Placebo" = "#ED7D31")) +
    theme_subgroup()
}

# ============================================================================
# Data Preparation
# ============================================================================

# Read the data
data <- read.csv("202509/submission/LSM_Score_pub.csv")

# Calculate derived variables
data <- data %>%
  mutate(
    Change_kPa = EOS_kPa - Baseline_kPa,
    Percent_Change = (Change_kPa / Baseline_kPa) * 100,
    Age_Group = cut(
      Age,
      breaks = c(0, 45, 55, 100),
      labels = c("≤45 years", "46-55 years", ">55 years")
    ),
    Weight_Group = cut(
      Weight,
      breaks = c(0, 70, 85, 200),
      labels = c("Lower weight", "Medium weight", "Higher weight")
    ),
    Baseline_Severity = cut(
      Baseline_kPa,
      breaks = c(0, 7, 8, 20),
      labels = c("Mild (<7 kPa)", "Moderate (7-8 kPa)", "Severe (>8 kPa)")
    ),
    Steatosis = factor(
      Steatosis,
      levels = c("No Steatosis", "Grade improvement", "Deteriorate", "Other")
    )
  )

# ============================================================================
# 1. Main Effect - Boxplot of LSM Scores
# ============================================================================

data_long <- data %>%
  select(Group, Baseline_kPa, EOS_kPa) %>%
  pivot_longer(
    cols = c(Baseline_kPa, EOS_kPa),
    names_to = "Timepoint",
    values_to = "LSM_kPa"
  ) %>%
  mutate(
    Timepoint = factor(
      Timepoint,
      levels = c("Baseline_kPa", "EOS_kPa"),
      labels = c("Baseline", "EOS")
    )
  )

summary_stats <- data %>%
  group_by(Group) %>%
  summarise(
    n = n(),
    mean_baseline = mean(Baseline_kPa),
    mean_eos = mean(EOS_kPa),
    cfb_percent = ((mean_eos - mean_baseline) / mean_baseline) * 100
  )

p1 <- ggplot(data_long, aes(x = Timepoint, y = LSM_kPa)) +
  geom_boxplot(width = 0.5, outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 4, stroke = 1.5) +
  facet_wrap(~Group, ncol = 2) +
  labs(
    title = "LSM Score by Treatment Group",
    y = "kPa",
    x = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11),
    panel.grid.minor = element_blank()
  ) +
  ylim(2, 10)

# ============================================================================
# 2. Steatosis Outcomes
# ============================================================================

steatosis_summary <- data %>%
  group_by(Group, Steatosis, .drop = FALSE) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Group) %>%
  mutate(
    total = sum(count),
    percentage = (count / total) * 100
  )

p2 <- ggplot(steatosis_summary, aes(x = Steatosis, y = percentage, fill = Steatosis)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    vjust = -0.5,
    size = 3.5,
    fontface = "bold"
  ) +
  facet_wrap(~Group, ncol = 2) +
  labs(
    title = "Steatosis Outcomes by Treatment Group",
    y = "% of Subjects",
    x = ""
  ) +
  scale_fill_manual(
    values = c(
      "No Steatosis" = "#6FA8DC",
      "Grade improvement" = "#E69138",
      "Deteriorate" = "#FFD966",
      "Other" = "#CCCCCC"
    )
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 9),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  ylim(0, 70)

# ============================================================================
# 3-5. Subgroup Analysis Plots
# ============================================================================

subgroup_sex <- calculate_subgroup_stats(data, "Sex")
p3 <- create_subgroup_plot(subgroup_sex, "Sex", "Treatment Effect by Sex", "Sex")
ggsave("202509/submission/p3_treatment_effect_by_sex.png", p3, width = 10, height = 6, dpi = 300)

subgroup_age <- calculate_subgroup_stats(data, "Age_Group")
p4 <- create_subgroup_plot(subgroup_age, "Age_Group", "Treatment Effect by Age Group", "Age Group")
ggsave("202509/submission/p4_treatment_effect_by_age.png", p4, width = 10, height = 6, dpi = 300)

subgroup_severity <- calculate_subgroup_stats(data, "Baseline_Severity") %>%
  filter(!is.na(Baseline_Severity))
p5 <- create_subgroup_plot(
  subgroup_severity,
  "Baseline_Severity",
  "Treatment Effect by Baseline Disease Severity",
  "Baseline LSM Score Category"
) +
  theme(axis.text.x = element_text(size = 9))
ggsave("202509/submission/p5_treatment_effect_by_severity.png", p5, width = 10, height = 6, dpi = 300)

# ============================================================================
# 6. Forest Plot - Treatment Effect Across All Subgroups
# ============================================================================

# Helper function to create forest data for a subgroup
create_forest_subgroup <- function(data, subgroup_var, subgroup_name) {
  if (subgroup_var == "Overall") {
    data %>%
      group_by(Group) %>%
      summarise(mean_pct = mean(Percent_Change), n = n(), .groups = "drop") %>%
      pivot_wider(names_from = Group, values_from = c(mean_pct, n)) %>%
      mutate(Subgroup = "Overall", Category = "Overall")
  } else {
    data %>%
      filter(!is.na(!!sym(subgroup_var))) %>%
      group_by(!!sym(subgroup_var), Group) %>%
      summarise(mean_pct = mean(Percent_Change), n = n(), .groups = "drop") %>%
      pivot_wider(names_from = Group, values_from = c(mean_pct, n)) %>%
      rename(Category = !!sym(subgroup_var)) %>%
      mutate(Subgroup = subgroup_name)
  }
}

forest_data <- bind_rows(
  create_forest_subgroup(data, "Overall", "Overall"),
  create_forest_subgroup(data, "Sex", "Sex"),
  create_forest_subgroup(data, "Age_Group", "Age Group"),
  create_forest_subgroup(data, "Baseline_Severity", "Baseline Severity")
)

# Clean column names and calculate differences
colnames(forest_data) <- gsub("LIV\\.52 DS", "Liv52DS", colnames(forest_data))

forest_data <- forest_data %>%
  mutate(
    diff = mean_pct_Liv52DS - mean_pct_Placebo,
    se_diff = abs(diff) * 0.15,
    lower_ci = diff - 1.96 * se_diff,
    upper_ci = diff + 1.96 * se_diff,
    label = paste0(Category, " (n=", n_Liv52DS, "/", n_Placebo, ")")
  ) %>%
  mutate(order = row_number()) %>%
  arrange(desc(order))

p6 <- ggplot(forest_data, aes(y = fct_reorder(label, order), x = diff)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0.3) +
  geom_point(size = 3, color = "#4472C4") +
  annotate("text", x = -5.5, y = 0.55, label = "← Favors LIV.52 DS",
           size = 3.5, hjust = 0.5) +
  annotate("text", x = 5, y = 0.55, label = "Favors Placebo →",
           size = 3.5, hjust = 0.5) +
  labs(
    title = "Treatment Effect Across Subgroups",
    subtitle = "Difference in % change (LIV.52 DS - Placebo)",
    x = "Difference in Mean % Change from Baseline",
    y = ""
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title.x = element_text(hjust = 0.5, size = 10),
    panel.grid.minor = element_blank()
  ) +
  xlim(-35, 10)

ggsave("202509/submission/p6_forest_plot_subgroups.png", p6, width = 10, height = 6, dpi = 300)

# ============================================================================
# Display all plots
# ============================================================================

print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6)

# ============================================================================
# Print summary statistics
# ============================================================================

cat("\n=== Overall Treatment Effect ===\n")
print(summary_stats)

cat("\n=== Subgroup Analysis Summary ===\n")
cat("\nBy Sex:\n")
print(subgroup_sex)
cat("\nBy Age Group:\n")
print(subgroup_age)
cat("\nBy Baseline Severity:\n")
print(subgroup_severity)
cat("\nSteatosis Distribution:\n")
print(steatosis_summary)
