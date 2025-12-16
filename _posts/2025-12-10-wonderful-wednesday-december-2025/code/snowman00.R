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

# --- Snowflakes ---
n_flakes <- 200
n_frames <- 150
fall_speed <- 0.08

snowflakes <- tibble(
  flake = 1:n_flakes,
  x = runif(n_flakes, -5, 5),
  y_start = runif(n_flakes, 0, 10)
)

# Compute snowflake positions for every frame
y_min <- 0
y_max <- 10
range_y <- y_max - y_min

snowflakes_anim <- snowflakes %>%
  crossing(frame = 1:n_frames) %>%
  mutate(y = y_start - fall_speed * frame,
         y = y_min + (y - y_min) %% range_y)  # smooth wrapping

p <- ggplot() +
 
  # Features (eyes and slightly lowered nose)
  geom_point(data=features_head, aes(x=x, y=y),
             color=features_head$col,
             fill=features_head$col,
             size=features_head$size,
             shape=features_head$shape) +
  
  # Buttons
  geom_point(data=buttons, aes(x=x, y=y), color="black", size=buttons$size) +
  

  coord_fixed(xlim=c(-5,5), ylim=c(0,10)) +
  theme(legend.position="none",
        axis.line = element_line(color = "black", linewidth = 0.4),
        panel.grid.major = element_line(color = "#969696", linewidth = 0.3),
        panel.grid.minor = element_blank(),   # remove minor gridlines
        panel.background = element_rect(fill = "#f0f0f0", color = NA)
  )

ggsave(p, filename = "temp00.png", width = 16, height = 16)


