library(tidyverse)
library(ggforce)
library(gganimate)

# --- Snowman setup ---

r_bottom <- 1.8
r_middle <- 1.6
r_head <- 1.1
overlap <- 0.2

eye_size <- 6
button_size <- 6
nose_size <- 6
hat_width1 <- 1.0
hat_width2 <- 0.8
hat_height <- 0.7

y_bottom <- 0
y_middle <- y_bottom + r_bottom - overlap
y_head <- y_middle + r_middle + r_head - overlap

snowman_parts <- tibble(
  x = 0, y = c(y_middle, y_head),
  r = c( r_middle, r_head)
)

# Head features: eyes and slightly lowered nose
features_head <- tibble(
  x = c(head_x - 0.2*head_r, head_x + 0.2*head_r, head_x),  # eyes and nose centered
  y = c(head_y + 0.19*head_r, head_y + 0.19*head_r, head_y - 0.1*head_r),  # nose slightly lower
  size = c(eye_size, eye_size, nose_size),  # nose larger
  col = c("black","black","orange"),
  shape = c(1,1,24)  # eyes, eyes, nose
)

# Buttons
buttons <- tibble(
  x = c(0, 0, 0),
  y = c(y_middle + 0.25*r_middle, y_middle, y_middle - 0.25*r_middle),
  size = c(button_size, button_size, button_size)   
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

snowflakes_anim <- snowflakes %>%
  crossing(frame = 1:n_frames) %>%
  mutate(y = y_start - fall_speed * frame,
         y = y_min + (y - y_min) %% range_y)  # smooth wrapping

p <- ggplot() +
 
  # Snowman body
  geom_circle(data = snowman_parts, aes(x0=x, y0=y, r=r),
              fill="white",
              color="gray30", size=0.4) +
  # 
  # Arms
  # geom_segment(data = arms, aes(x=x, y=y, xend=xend, yend=yend),
  #              size=1.2, color="saddlebrown", lineend="round") +
  
  # Hat
  geom_rect(aes(xmin=-hat_width2, xmax=hat_width2,
                ymin=head_y+head_r, ymax=head_y+head_r+hat_height),
            fill="black") +
  geom_rect(aes(xmin=-hat_width1, xmax=hat_width1,
                ymin=head_y+head_r-hat_brim_height/2, ymax=head_y+head_r+hat_brim_height/2),
            fill="black") +
  geom_rect(aes(xmin=-hat_width2, xmax=hat_width2,
                ymin=head_y+head_r+hat_brim_height/2, ymax=head_y+head_r+hat_brim_height/2+hat_height/10),
            fill="red") +
  
  geom_point(data=features_head, aes(x=x, y=y),
             color=features_head$col,
             fill=features_head$col,
             size=features_head$size,
             shape=features_head$shape) +
  
  # Buttons
  geom_point(data=buttons, aes(x=x, y=y), color="black", size=eye_size) +
  
  # Snowflakes
  geom_point(data=snowflakes_anim, aes(x=x, y=y), color="white", size=1.5, alpha=0.8) +
  # 
  scale_x_continuous(limits=c(-5, 5)) +
  scale_y_continuous(limits=c(0, 10)) +
  
  theme_void() +
  theme(legend.position="none",
         panel.background = element_rect(fill = "#d9d9d9", color = NA)
  ) +
  
  # Animation
  transition_manual(frame)

p <- p +
  annotate(
    "text",
    x = 0, y = 8.5,
    label = "MERRY CHRISTMAS!",
    size = 15,
    fontface = "bold",
    family = "sans",
    colour = "black"
  )


anim_out <- animate(p, nframes=n_frames, fps=30, width=700, height=700, renderer = gifski_renderer(loop = TRUE))

anim_save("temp06.gif", anim_out)


