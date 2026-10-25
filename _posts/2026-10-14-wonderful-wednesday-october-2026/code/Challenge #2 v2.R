# Load Required Packages
library(ggplot2)
library(patchwork)
library(scales)
library(cowplot)

# Risk Values
background_risk <- 5
total_risk <- 15
covid_risk <- 400

risk_data <- data.frame(
  label = c(
    paste("Background prevalence:", background_risk),
    paste("After vaccination:", total_risk),
    paste("In COVID-infected patients:", covid_risk)
  ),
  risk = c(background_risk, total_risk, covid_risk),
  colour = c("grey40", "#d62828", "#1f77b4"),
  
  # Label positions for top panel
  label_x = c(50, 50, covid_risk),
  label_y = c(1.15, 1.1, 1.1)
)

# Labels for lower panel
lower_labels <- data.frame(
  risk = c(background_risk, total_risk, covid_risk),
  label_x = c(5000, 25000, 50000),
  y = c(1.1, 1.2, 1.3),
  label = c(paste(background_risk),
            paste(total_risk),
            paste(covid_risk)),
  colour = c("grey40", "#d62828", "#1f77b4")
)

# Top Panel
p_main <- ggplot(risk_data) +
  
  annotate(
    "segment",
    x = 0,
    xend = 1.25*covid_risk,
    y = 1,
    yend = 1,
    linewidth = 8,
    colour = "grey90"
  ) +
  
  geom_point(
    aes(
      x = risk,
      y = 1,
      fill = colour
    ),
    shape = 21,
    size = 8,
    colour = "black",
  ) +
  
  scale_fill_identity() +
  
  geom_text(
    aes(
      x = label_x,
      y = label_y,
      label = label,
      colour = colour
    ),
    fontface = "bold",
    size = 5
  ) +
  
  scale_colour_identity() +
  
  scale_x_continuous(
    limits = c(0, 500),
    breaks = seq(0, 500, 100)
  ) +
  
  coord_cartesian(
    ylim = c(0.95, 1.2),
  ) +
  
  labs(
    caption = paste("Cases per million person-years = expected blood clots",
                    "among 1,000,000 people over one year.")
  ) +
  
  theme_minimal(base_size = 16) +
  
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_blank(),
    
    panel.border = element_rect(
      colour = "red",
      fill = NA,
      linewidth = 2
    ),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.caption = element_text(
      hjust = 0,
      face = "italic",
      size = 12,
      margin = margin(t = 15)
    ),
    
    plot.margin = margin(
      t = 40,
      r = 20,
      b = 20,
      l = 40
    )
  )

# Bottom Panel
p_inset <- ggplot(risk_data) +
  
  annotate(
    "segment",
    x = 0,
    xend = 1000000,
    y = 1,
    yend = 1,
    linewidth = 8,
    colour = "grey90"
  ) +
  
  geom_point(
    aes(
      x = risk,
      y = 1,
      fill = colour
    ),
    shape = 21,
    size = 8,
    colour = "black"
  ) +
  
  # Leader lines from labels to points
  geom_segment(
    data = lower_labels,
    aes(
      x = risk,
      xend = label_x,
      y = 1,
      yend = y - 0.02
    ),
    colour = "grey50",
    linewidth = 0.5
  ) +
  
  # Labels
  geom_text(
    data = lower_labels,
    aes(
      x = label_x,
      y = y,
      label = label,
      colour = colour
    ),
    fontface = "bold",
    size = 5
  ) +
  
  scale_fill_identity() +
  scale_colour_identity() +
  
  scale_x_continuous(
    limits = c(-10000, 1000000),
    breaks = c(0, 500000, 1000000),
    labels = comma
  ) +
  
  coord_cartesian(
    ylim = c(0.95, 1.3),
  ) +
  
  labs(
    title = paste("Blood clots per million person-years"),
  ) +
  
  theme_minimal(base_size = 16) +
  
  annotate(
    "rect",
    xmin = -10000,
    xmax = 10000,
    ymin = 0.965,
    ymax = 1.035,
    colour = "red",
    fill = NA,
    linewidth = 1.5
  ) +
  
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 18,
      margin = margin(b = 15)
    ),
    
    plot.margin = margin(
      t = 10,
      r = 20,
      b = 20,
      l = 20
    )
  )

# Combined Plot
combined_plot <-
(p_inset / p_main) +
  plot_layout(
    heights = c(1, 3)
  ) +
  plot_annotation(
    title = paste("In absolute terms, blood clots are rare overall.\n",
                  "In relative terms, the vaccination increases the risk",
                  "of blood clots, but by much less than",
                  "a COVID infection does."),
    theme = theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 24,
        face = "bold"
      )
    )
  )

# Drawing connecting lines for zoom effect
# NOTE - VALUES MANUALLY TOGGLED TO LINE UP NICELY IN ZOOMED WINDOW
# WILL NOT ALWAYS LINE UP AS DESIRED
ggdraw(combined_plot) +
  
  draw_line(
    x = c(0.085, 0.035),
    y = c(0.705, 0.577),
    colour = "red",
    linetype = "dashed",
    linewidth = 1.2
  ) +
  
  draw_line(
    x = c(0.085, 0.98),
    y = c(0.705, 0.578),
    colour = "red",
    linetype = "dashed",
    linewidth = 1.2
  )






