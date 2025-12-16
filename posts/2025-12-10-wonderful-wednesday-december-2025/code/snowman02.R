library(tidyverse)
library(ggforce)
library(gganimate)

# --- Snowman setup ---
r_bottom <- 1.8
r_middle <- 1.6
r_head <- 1.1
overlap <- 0.2

y_bottom <- 0
y_middle <- y_bottom + r_bottom - overlap
y_head <- y_middle + r_middle + r_head - overlap

snowman_parts <- tibble(
  x = 0, y = c(y_middle, y_head),
  r = c( r_middle, r_head)
)

# Head features: eyes and slightly lowered nose
head_x <- 0; head_y <- y_head; head_r <- r_head
features_head <- tibble(
  x = c(head_x - 0.2*head_r, head_x + 0.2*head_r, head_x),  # eyes and nose centered
  y = c(head_y + 0.18*head_r, head_y + 0.18*head_r, head_y - 0.1*head_r),  # nose slightly lower
  size = c(3, 3, 4),  # nose larger
  col = c("black","black","orange"),
  shape = c(1,1,24)  # eyes, eyes, nose
)

# Buttons
buttons <- tibble(
  x = c(0, 0, 0),
  y = c(y_middle + 0.25*r_middle, y_middle, y_middle - 0.25*r_middle),
  size = c(r_middle, r_middle, r_middle)  # increased size
)

p <- ggplot() +
  # Background
  # annotate("rect", xmin = -5, xmax = 5, ymin = -5, ymax = 10, fill = "#2b3b4f") +
  
  # Snowman body
  geom_circle(data = snowman_parts, aes(x0=x, y0=y, r=r),
              # fill="white",
              color="gray30", size=0.4) +
  # Features (eyes and slightly lowered nose)
  geom_point(data=features_head, aes(x=x, y=y),
             color=features_head$col,
             fill=features_head$col,
             size=features_head$size,
             shape=features_head$shape) +
  
  # Buttons
  geom_point(data=buttons, aes(x=x, y=y), color="black", size=buttons$size) +
  
  # Snowflakes
  # geom_point(data=snowflakes_anim, aes(x=x, y=y), color="white", size=1.5, alpha=0.8) +
  
  # coord_fixed(xlim=c(-5,5), ylim=c(0,10)) +
  scale_x_continuous(limits=c(-2, 2)) +
  theme_classic(base_size = 24) +
  theme(legend.position="none",
        axis.line = element_line(color = "black", linewidth = 0.4),
        panel.grid.major = element_line(color = "darkgray", linewidth = 0.3),
        panel.grid.minor = element_blank(),   # remove minor gridlines
        panel.background = element_rect(fill = "grey85", color = NA)
  )

ggsave(p, filename = "sm02.png", width = 16, height = 16)
