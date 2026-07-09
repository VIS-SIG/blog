###############################################################################
# Load Required Packages
###############################################################################

library(readxl)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(patchwork)
library(htmlwidgets)


###############################################################################
# Load Data
###############################################################################

pk <- read_excel("Simulated_PK_Data.xlsx") %>%
  mutate(subject = factor(subject))


###############################################################################
# Add Pre-Dose Observations and Calculate Cmax Tmax and AUC
###############################################################################

predose <- pk %>%
  distinct(subject) %>%
  mutate(
    time_hr = 0,
    concentration_ng_ml = 0
  )

pk <- bind_rows(pk, predose) %>%
  arrange(subject, time_hr)

pk_summary <- pk %>%
  group_by(subject) %>%
  summarise(
    Cmax = max(concentration_ng_ml),
    Tmax = time_hr[which.max(concentration_ng_ml)],
    AUC = sum(
      diff(time_hr) *
        (
          head(concentration_ng_ml, -1) +
            tail(concentration_ng_ml, -1)
        ) / 2
    ),
    .groups = "drop"
  )


###############################################################################
# Spaghetti Plots of Concentrations Over Time
###############################################################################

p1 <- 
  ggplot(
    data = pk, 
    aes(
      x = time_hr,
      y = concentration_ng_ml,
      group = subject,
      order = time_hr,
      )
    ) + 
  geom_line_interactive(
    aes(
      data_id = subject,
      tooltip = paste0("Subject: ", subject)
      ),
    colour = "lightgrey",
    linewidth = 0.8,
    alpha = 0.8
  ) + 
  geom_point_interactive(
    aes(
      data_id = subject,
      tooltip = paste0(
        "<b>", subject, "</b>",
        "<br>Time: ", time_hr, " hours",
        "<br>Concentration: ",
        round(concentration_ng_ml, 1),
        " ng/mL"
      )
    ),
    colour = "grey",
    size = 2,
    alpha = 0.8
  ) +
  stat_summary(
    aes(group = 1),
    fun = median,
    geom = "line",
    colour = "black",
    linewidth = 1.2
  ) +
  scale_x_continuous(
    breaks = c(0, 0.5, 1, 2, 4, 6, 8, 12, 24),
    labels = c("0", "0.5", "1", "2", "4", "6", "8", "12", "24")
  ) + 
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20)
  ) + 
  labs(
    title = "Individual PK Profiles",
    subtitle = "Concentration Over Time",
    x = "Time post-dose (hours)",
    y = "Concentration (ng/mL)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


###############################################################################
# Scatter Plot of AUC vs Cmax
###############################################################################  

p2 <-
  ggplot(
    data = pk_summary,
    aes(
      x = AUC,
      y = Cmax
    )
  ) +
  geom_point_interactive(
    aes(
      data_id = subject,
      tooltip = paste0(
        "<b>", subject, "</b>",
        "<br>Cmax: ", round(Cmax, 1), " ng/mL",
        "<br>AUC: ", round(AUC, 1), " ng·hours/mL",
        "<br>Tmax: ", Tmax, " hours"
      )
    ),
    colour = "grey",
    fill = "grey",
    shape = 16,
    alpha = 0.8,
    size = 2
  ) +
  labs(
    title = "Exposure Relationship",
    subtitle = "Max Concentration (Cmax) versus Area Under the Curve (AUC)",
    x = "AUC (ng·hours/mL)",
    y = "Cmax (ng/mL)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title.position = "plot"
  )


###############################################################################
# Combine Plots Using patchwork Package
###############################################################################

combined_plot <- p1 + p2 +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(
    caption = paste(
      "Individual profiles shown in grey.",
      "\nBlack line represents the median profile.",
      "\nInteractive selection highlights the corresponding",
      "subject across both panels.",
      "\nNote: Concentration at 0 hours was assumed to be",
      "0 ng/mL for visualisation and AUC calculations."
    )
  ) &
  theme(
    plot.caption.position = "plot",
    plot.caption = element_text(
      hjust = 0,
      size = 8,
    )
  )



###############################################################################
# Make Plots Interactive Using ggiraph Package
###############################################################################

interactive_plot <- girafe(
  ggobj = combined_plot,
  width_svg = 16,
  height_svg = 6,
  options = list(
    # Hover Styling
    opts_hover(
      css = paste(
        "stroke:#09B5CD;",
        "opacity:1;",
        "stroke-width:3;"
      )
    ),
    # Fade Everything Else
    opts_hover_inv(
      css = "opacity:0.15;"
    ),
    # Allow Click Selection
    opts_selection(
      type = "single",
      only_shiny = FALSE,
      css = paste(
        "stroke:#09B5CD;",
        "opacity:1;",
        "stroke-width:3;"
      )
    ),
    # Fade Everything Else
    opts_selection_inv(
      css = "opacity:0.15;"
    )
  )
)


###############################################################################
# Display Plot
###############################################################################

interactive_plot


###############################################################################
# HTML export
###############################################################################

interactive_plot$width <- "100%"
interactive_plot$height <- "950px"

saveWidget(
  interactive_plot,
  "interactive_pk_visualisation.html",
  selfcontained = TRUE
  )
