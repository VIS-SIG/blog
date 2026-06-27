library(ggplot2)
library(ggtext)
library(scales)

# ggplot2 default colours
default_cols <- hue_pal()(2)

# Fix y-axis range (preserved)
y_limits <- c(0, 1)
y_range  <- diff(y_limits)

# Position lollipops at ~40% and ~60% of y-range
y_short <- y_limits[1] + 0.60 * y_range
y_long  <- y_limits[1] + 0.40 * y_range

df <- data.frame(
  group = factor(c("Failing", "Sustaining"),
                 levels = c("Failing", "Sustaining")),
  y     = c(y_short, y_long),
  time  = c(2, 11)
)

# Arrow position
arrow_y <- mean(df$y)

p <- ggplot(df, aes(x = time, y = y, colour = group)) +
  
  # Lollipop stems
  geom_segment(
    aes(x = 0, xend = time, yend = y),
    linewidth = 2
  ) +
  
  # Lollipop heads
  geom_point(size = 10) +
  
  # Vertical reference line at time = 0
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 2
  ) +
  
  # Arrow between lollipops
  annotate(
    "segment",
    x = 2, xend = 11,
    y = arrow_y, yend = arrow_y,
    linewidth = 2,
    arrow = arrow(length = unit(0.35, "cm"))
  ) +
  
  # Arrow label
  annotate(
    "label",
    x = 6.5,
    y = arrow_y,
    label = "+9 years",
    size = 10,
    fontface = "bold",
    fill = "white",
    linewidth = 0
  ) +
  
  # Numeric labels at lollipop ends
  annotate(
    "text",
    x = 2,
    y = y_short + 0.05,
    label = "2",
    colour = default_cols[1],
    size = 10,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = 11,
    y = y_long - 0.05,
    label = "11",
    colour = default_cols[2],
    size = 10,
    fontface = "bold"
  ) +
  
  # Scales
  scale_y_continuous(breaks = NULL, limits = y_limits) +
  scale_colour_manual(values = default_cols) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.05))) +
  
  # Labels
  labs(
    x = "Time To Diabetic Treatment Intensification (Years)",
    y = NULL,
    title = paste0(
      "Patients <span style='color:", default_cols[2], "'>",
      "sustaining composite treatment targets",
      "</span> delayed diabetic treatment intensification <br> by an average of ",
      "9 years compared with ",
      "<span style='color:", default_cols[1], "'>",
      "those failing to do so",
      "</span>"
    )
  ) +
  
  # Theme
  theme_minimal(base_size = 32) +
  theme(
    axis.line.x  = element_line(colour = "black", linewidth = 2),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid   = element_blank(),
    plot.title   = element_markdown(size = 32, lineheight = 1.4),
    legend.position = "none"
  )

print(p)