# Load Required Packages
library(ggplot2)
library(ggtext)

# Data Setup
background_cases <- 5
vaccine_cases <- 10
covid_infections_vaccinated <- 0.3*(1-0.7)*1000000
covid_infections_unvaccinated <- 0.3*1000000
covid_cases_vaccinated <- 0.3*(1-0.7)*400
covid_cases_unvaccinated <- 0.3*400

plot_data <- data.frame(
  group = rep(c("Unvaccinated", "Vaccinated"), each = 3),
  component = c(
    "Background", "Vaccine", "COVID",
    "Background", "Vaccine", "COVID"
  ),
  value = c(
    background_cases, 0, covid_cases_unvaccinated,
    background_cases, vaccine_cases, covid_cases_vaccinated
  )
)

plot_data$component <- factor(
  plot_data$component,
  levels = c("COVID", "Vaccine", "Background")
)

totals <- aggregate(
  value ~ group,
  data = plot_data,
  sum
)

# Annotations
annot_data <- data.frame(
  x = c(
    0.45,  # unvaccinated background
    0.45,  # unvaccinated covid
    
    2.55,  # vaccinated background
    2.55,  # vaccinated vaccine
    2.55   # vaccinated covid
  ),
  
  y = c(
    background_cases/2,
    background_cases + covid_cases_unvaccinated/2,
    
    background_cases/2,
    background_cases + vaccine_cases/2,
    background_cases + vaccine_cases + covid_cases_vaccinated/2
  ),
  
  label = c(
    paste(background_cases,
          "cases due to background risk"),
    
    paste(covid_cases_unvaccinated,
          "cases among",
          format(covid_infections_unvaccinated,
                 big.mark = ",", scientific = FALSE),
          "individuals contracting COVID"),
    
    paste(background_cases,
          "cases due to background risk"),
    
    paste(vaccine_cases,
          "cases due to increased risk post-vaccination"),
    
    paste(covid_cases_vaccinated,
          "cases among",
          format(covid_infections_vaccinated,
                 big.mark = ",", scientific = FALSE),
          "individuals contracting COVID")
  ),
  
  colour = c(
    "grey40",
    "#2c7bb6",
    
    "grey40",
    "#e52521",
    "#2c7bb6"
  )
)

# Plot
ggplot(
  plot_data,
  aes(
    x = group,
    y = value,
    fill = component
  )
) +
  
  geom_col(
    width = 0.7,
    colour = "black",
    linewidth = 0.8
  ) +
  
  scale_fill_manual(
    values = c(
      "Background" = "grey70",
      "Vaccine"    = "#e52521",
      "COVID"      = "#2c7bb6"
    )
  ) +
  
geom_text(
  data = totals,
  aes(
    x = group,
    y = value - 6,
    label = value
  ),
  inherit.aes = FALSE,
  colour = "white",
  fontface = "bold",
  size = 9
) +

  geom_text(
    data = subset(annot_data, x < 1),
    aes(
      x = x,
      y = y,
      label = label,
      colour = colour
    ),
    inherit.aes = FALSE,
    hjust = 1,
    size = 4.5
  ) +
  
  geom_text(
    data = subset(annot_data, x > 1),
    aes(
      x = x,
      y = y,
      label = label,
      colour = colour
    ),
    inherit.aes = FALSE,
    hjust = 0,
    size = 4.5
  ) +
  
  scale_colour_identity() +
  
  labs(
    title = paste("There is a higher risk of blood clots in unvaccinated",
                  "than in vaccinated patients"),
    subtitle = "Blood clots per million person-years",
    x = NULL,
    y = NULL
  ) +
  
  coord_cartesian(
    xlim = c(-0.2, 3.2),
    ylim = c(0, max(totals$value)+10),
    clip = "off"
  ) +
  
  theme_minimal(base_size = 16) +
  
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 24
    ),
    
    plot.subtitle = element_text(
      hjust = 0.5,
      size = 18
    ),
    
    axis.text.x = element_text(
      size = 18,
      face = "bold"
    ),
    
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    
    legend.position = "none",
    
    plot.margin = margin(
      t = 20,
      r = 220,
      b = 20,
      l = 220
    )
  )