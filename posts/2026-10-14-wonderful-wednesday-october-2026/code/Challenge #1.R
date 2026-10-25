# Load Required Packages
library(ggplot2)
library(ggtext)
library(grid)

# Risk Values
background_risk <- 5
total_risk <- 15
additional_risk <- total_risk - background_risk

#Create Plot
ggplot() +
  
  # Background prevalence bar
  geom_rect(
    aes(
      xmin = 0.6,
      xmax = 1.4,
      ymin = 0,
      ymax = background_risk
    ),
    fill = "light grey",
    colour = "black"
  ) +
  
  # Grey section of vaccination bar
  geom_rect(
    aes(
      xmin = 1.6,
      xmax = 2.4,
      ymin = 0,
      ymax = background_risk
    ),
    fill = "light grey",
    colour = "black"
  ) +
  
  # Additional risk section of vaccination bar
  geom_rect(
    aes(
      xmin = 1.6,
      xmax = 2.4,
      ymin = background_risk,
      ymax = total_risk
    ),
    fill = "red",
    colour = "black"
  ) +
  
  # Dashed line showing background prevalence level
  geom_segment(
    aes(x = 0.4, xend = 2.6,
        y = background_risk, yend = background_risk),
    linetype = "dashed",
    colour = "grey40",
    linewidth = 0.8
  ) +
  
  # Value labels inside bars
  annotate(
    "text",
    x = 1,
    y = background_risk - 1,
    label = background_risk,
    fontface = "bold",
    size = 7
  ) +
  
  annotate(
    "text",
    x = 2,
    y = total_risk - 1,
    label = total_risk,
    fontface = "bold",
    size = 7
  ) +
  
  # Annotation showing additional risk
  annotate(
    "segment",
    x = 1.45, xend = 1.45,
    y = background_risk, yend = total_risk,
    linewidth = 1
  ) +
  
  annotate(
    "segment",
    x = 1.45, xend = 1.55,
    y = background_risk, yend = background_risk,
    linewidth = 1
  ) +
  
  annotate(
    "segment",
    x = 1.45, xend = 1.55,
    y = total_risk, yend = total_risk,
    linewidth = 1
  ) +
  
  annotate(
    "text",
    x = 1,
    y = background_risk + 0.5*additional_risk,
    label = paste0("+", additional_risk, 
                   " cases per million\nperson-years"),
    hjust = 0,
    fontface = "bold",
    size = 7
  ) +
  
  scale_x_continuous(
    breaks = c(1, 2),
    labels = c(
      "Background\nprevalence",
      "Risk after\nvaccination"
    )
  ) +
  
  labs(
    title = paste0(
      "The vaccine results in a ",
      "<span style='color:", "red", ";'><b>200% increase</b></span>",
      " in the risk of a blood clot"
    ),
    subtitle = "Blood clot cases per million person-years"
  ) +
  
  theme_minimal(base_size = 16) +
  
  theme(
    plot.title = element_markdown(
      size = 27,
      face = "plain",
      hjust = 0.5,
      margin = margin(b = 10)
    ),
    
    plot.subtitle = element_text(
      size = 21,
      hjust = 0.5,
      margin = margin(b = 20)
    ),
    
    # Remove all y-axis elements
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Keep prominent x-axis
    axis.title.x = element_blank(),
    axis.line.x = element_line(
      colour = "black",
      linewidth = 1.2
    ),
    axis.ticks.x = element_line(
      colour = "black"
    ),
    
    axis.text.x = element_text(
      size = 21,
      face = "bold"
    ),
    
    panel.grid.major.x = element_blank(),
    
    axis.line.y = element_blank(),
    
    plot.margin = margin(
      t = 20,
      r = 20,
      b = 20,
      l = 40
    )
  )
