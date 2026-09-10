#=================================================
# Load Required Packages
#=================================================
library(readxl)
library(dplyr)
library(ggplot2)
library(grid)


#=================================================
# Import Data
#=================================================
dat <- read_excel("Protocol Amendment Dataset.xlsx")

#=================================================
# Plot
#=================================================
ggplot(
  dat,
  aes(
    x = Month,
    y = New_Subjects_Enrolled,
    fill = Amendment_Status
  )
) +
  geom_col() +
  geom_text(
    aes(label = New_Subjects_Enrolled,
        colour = Amendment_Status),
    vjust = -0.25,
    size = 5
  ) +
  annotate(
    "segment",
    x = 9.5,
    xend = 9.5,
    y = 0,
    yend = max(dat$New_Subjects_Enrolled) + 3,
    colour = "#D55E00",
    linewidth = 1.2,
    linetype = 2
  ) +
  annotate(
    "text",
    x = 9.5,
    y = max(dat$New_Subjects_Enrolled) + 5,
    label = "Protocol amendment introduced",
    colour = "#D55E00",
    size = 6
  ) +
  annotate(
    "text",
    x = 5,
    y = max(dat$New_Subjects_Enrolled) + 5,
    label = "Pre-Amendment",
    colour = "grey55",
    size = 6
  ) +
  annotate(
    "text",
    x = 17,
    y = max(dat$New_Subjects_Enrolled) + 5,
    label = "Post-Amendment",
    colour = "#4EA5D9",
    size = 6
  ) +
  scale_fill_manual(
    values = c(
      "Before Amendment" = "grey55",
      "After Amendment" = "#4EA5D9"
    )
  ) +
  scale_colour_manual(
    values = c(
      "Before Amendment" = "grey55",
      "After Amendment" = "#4EA5D9"
    )
  ) +
  scale_x_continuous(
    breaks = 1:24,
    expand = expansion(mult = c(0, 0))
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Monthly Enrolment",
    subtitle = paste("Enrolment increased immediately following the amendment",
                     "and continued to rise throughout the remainder",
                     "of the study")
  ) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 18),
    axis.text.x = element_text(size = 12),
    axis.line.x = element_line(colour = "black", size = 2),
    legend.position = "none",
    plot.title = element_text(
      face = "bold",
      size = 30,
      hjust = 0.5,
      margin = margin(
        b = 15
      )
    ),
    plot.subtitle = element_text(
      size = 20,
      hjust = 0.5,
      margin = margin(
        b = 35
      )
    ),
    plot.margin = margin(
      t = 25,
      r = 25,
      b = 20,
      l = 25
    )
  ) 
