library(ggplot2)
library(tidyverse)
library(haven)
library(ggh4x)

data  <- read_sas("lsm_all.sas7bdat") %>%
  arrange(method, treat, avisitn) %>%
  mutate(
    facet_group = case_when(
      method == 1 ~ "Treatment \n Policy Estimand",
      TRUE ~ "Hypothetical Estimand"
    ),
    group_label = case_when(
      method == "1" ~ "No Imputation",
      method == "2" ~ "Multiple Imputation: \nMissing at Random (MAR)",
      method == "3" ~ "Multiple Imputation: \nMissing Not at Random (CIR)",
      method == "4" ~ "Multiple Imputation: \nMissing Not at Random (J2R)"
    )
  )

my_colors <- c("1" = "#1f77b4", "2" = "#ff7f0e")   
my_labels <- c("1" = "Active", "2" = "Control")
my_panels <- c("1" = " ", "2" = "Missing at Random", "3" = "Copy Increment\n from Reference", "4" = "Jump to Reference", "5" = "Treatment Policy \n Estimand", "6" = "Hypothetical \n Estimand")
my_black <- "#252525"

data$method <- factor(data$method, levels = c(1,2,3,4))
data$facet_group <- factor(data$facet_group, levels = c("Treatment \n Policy Estimand", "Hypothetical Estimand"))
data$group_label <- factor(data$group_label, levels = c("No Imputation", "Multiple Imputation: \nMissing at Random (MAR)",  "Multiple Imputation: \nMissing Not at Random (CIR)", "Multiple Imputation: \nMissing Not at Random (J2R)"))

custom_labels <- c(
  "1" = "No Imputation",
  "2" = "Multiple Imputation: \nMissing at Random (MAR) ",
  "3" = "Multiple Imputation: \nCopy Increment ",
  "4" = "Multiple Imputation: \nJump to Reference "
)

plot <-  ggplot(data) +
  geom_line(aes(x = avisitn, y = Estimate, color=factor(treat)), linetype = 1, size=1, alpha=0.8) +
  # facet_grid(~ method, labeller = as_labeller(my_panels)) +
  facet_nested(~ facet_group + group_label) + 
  scale_x_continuous("Time (weeks)", limits=c(1, 12), breaks = 0:12) +
  scale_y_continuous("Change From Baseline", limits=c(-10, 80), breaks=seq(-10, 80, by = 10)) +
  scale_color_manual(" ", values = my_colors, labels = my_labels) +
  theme(panel.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour = "#f0f0f0",
                                      linewidth = 0.5,
                                      linetype = 1),
        panel.border=element_rect(fill = NA,
                                  colour = my_black,
                                      linewidth = 1,
                                      linetype = 1),
        strip.background = element_rect(fill = NA,
                                        colour = my_black,
                                        linewidth = 1,
                                        linetype = 1),
        strip.text = element_text(
          colour = my_black,
          size = 12),
        axis.line.y=element_line(colour = my_black,
                               linewidth = 0.5,
                               linetype = 1),
        axis.text.x=element_text(
          colour = my_black,
          size = 11),
        axis.text.y=element_text(
          colour = my_black,
          size = 12),
        axis.title=element_text(
          colour = my_black,
          size =12),
        legend.text=element_text(
          colour = my_black,
          size = 12))
ggsave(plot, filename = "lsm_panel.png", width = 12, height = 6)