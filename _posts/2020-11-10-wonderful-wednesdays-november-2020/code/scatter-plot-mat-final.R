library(tidyverse)
library(ggforce)

## Save plot
page_width <- 350
page_height <- 250
d_dpi <- 400

## read in data
final_in <- read_csv("mediation_data.csv")


## Plot overall
ggplot(final_in, aes(x = .panel_x, y = .panel_y, colour = TRT, fill = TRT)) +
  geom_autopoint(alpha = 0.6) +
  geom_autodensity(alpha = 0.2) +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3)) +
  facet_matrix(vars(DLQI, itch, redness, BSA), layer.diag = 2, layer.upper = 3, 
               grid.y.diag = FALSE) +
  labs(title = "Itch has a strong positive association with DLQI.",
       subtitle = "There is evidence that the DLQI treatment effect is mediated through itch.",
       caption = "\n Cubic splines are presented in the top-right layer regressing y on x.\nScatter-plots of the same relationship are displayed in the bottom-left later.\nThe marginal distribution by treatment are displayed on the diagonal.") +  
  theme_light(base_size = 14) +
  theme(legend.position = "bottom")

## save plot
ggsave(file = paste0("scatter-matrix-final.png"), 
       width = page_width, height = page_height, 
       units = "mm", dpi = d_dpi)

