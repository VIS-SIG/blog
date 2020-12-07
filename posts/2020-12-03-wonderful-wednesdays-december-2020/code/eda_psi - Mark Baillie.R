library(readr)
library(tidyverse)
library(Hmisc)
library(hrbrthemes)
library(ggtext)
library(rlang)


# Functions originally from Cedric Scherer  
# https://cedricscherer.netlify.app/2019/08/05/a-ggplot2-tutorial-for-beautiful-plotting-in-r/
element_textbox_highlight <- function(..., hi.labels = NULL, hi.fill = NULL,
                                      hi.col = NULL, hi.box.col = NULL, hi.family = NULL) {
  structure(
    c(element_textbox(...),
      list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, hi.box.col = hi.box.col, hi.family = hi.family)
    ),
    class = c("element_textbox_highlight", "element_textbox", "element_text", "element")
  )
}

element_grob.element_textbox_highlight <- function(element, label = "", ...) {
  if (label %in% element$hi.labels) {
    element$fill <- element$hi.fill %||% element$fill
    element$colour <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
    element$family <- element$hi.family %||% element$family
  }
  NextMethod()
}


## Read data and manipulate
data <- read_csv("BIG_DATA_PSI_WW_DEC2020.csv") %>%
  mutate(
    TRT01PC = if_else(TRT01P == "INT", "Intensive treatment", "Standard of care"),
    STUDYIDC = paste0("Study ", STUDYID)
  )

# Check color palettes 
# RColorBrewer::display.brewer.all()



#-------------------------------------------------------
# Small multiples of BASE by Study



plot1a <-
  ggplot(data, aes(x = BASE, y = TRT01PC, fill = TRT01PC)) +
  geom_density_ridges(scale = 4,
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7) + 
  scale_y_discrete(expand = c(0, 0)) +   
  scale_x_continuous(expand = c(0, 0)) + 
  scale_fill_brewer(palette = "Dark2") +
  coord_cartesian(clip = "off") + 
  theme_ipsum_rc(base_size = 16) +
  labs(x = "DBP [mmHg] at 1-year") +
  facet_wrap(~STUDYIDC) +
  labs(
    x = "Mean systolic blood pressure at baseline [mmHg]",
    fill = "Treatment",
    title = "Comparison of mean systolic blood pressure measured at 1-year by study",
    subtitle = "There is some evidence of a bi-modal distribution in studys 1 and 3",
    caption = "Data: BIG_DATA_PSI_WW_DEC2020.csv"
  ) +
  theme(legend.position = "bottom")


ggsave("eda-plot1a.png", plot1a, height = 8, width = 12, dpi = 250)




#-------------------------------------------------------
# Small multiples of AVAL by Study



plot1b <-
  ggplot(data, aes(x = AVAL, y = TRT01PC, fill = TRT01PC)) +
  geom_density_ridges(scale = 4,
                      jittered_points = TRUE,
                      position = position_points_jitter(width = 0.05, height = 0),
                      point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7) + 
  scale_y_discrete(expand = c(0, 0)) +   
  scale_x_continuous(expand = c(0, 0)) + 
  scale_fill_brewer(palette = "Dark2") +
  coord_cartesian(clip = "off") + 
  theme_ipsum_rc(base_size = 16) +
  labs(x = "DBP [mmHg] at 1-year") +
  facet_wrap(~STUDYIDC) +
  labs(
    x = "Mean systolic blood pressure at 1-year [mmHg]",
    fill = "Treatment",
    title = "Comparison of mean systolic blood pressure measured at 1-year by study",
    subtitle = "There is some evidence of a bi-modal distribution in studys 1 and 3",
    caption = "Data: BIG_DATA_PSI_WW_DEC2020.csv"
  ) +
  theme(legend.position = "bottom")


ggsave("eda-plot1b.png", plot1a, height = 8, width = 12, dpi = 250)




#-------------------------------------------------------
# Small multiples of AVAL vs BASE by Study

plot2a <-
  data %>% ggplot(aes(
    x = BASE,
    y = AVAL,
    group = TRT01PC,
    color = TRT01PC
  )) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "grey",
    size = 1,
    alpha = 0.5
  ) +
  geom_smooth(
    method = "lm",
    formula = y ~ splines::bs(x, 3),
    se = TRUE,
    alpha = 0.55
  ) +
  geom_point(alpha = 0.45, size = 0.6) +
  facet_wrap( ~ STUDYIDC, ncol = 3) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(
    x = "SBP [mmHg] at randomisation",
    y = "SBP [mmHg] at 1-year",
    color = "Treatment",
    title = "Comparison of pre-post mean systolic blood pressure (SBP) measured at baseline and 1-year",
    subtitle = "Study 1 and 3 may have data quality issues - further investigation required",
    caption = "The by-treatment relationship also disaplyed using a cubic splines.\ny = x reference line also displayed.\nData: BIG_DATA_PSI_WW_DEC2020.csv"
  )  +
  theme_ipsum_rc(base_size = 16) +
  theme(
    strip.text = element_textbox_highlight(
      size = 12,
      face = "bold",
      fill = "white",
      box.color = "white",
      color = "gray40",
      halign = .5,
      linetype = 1,
      r = unit(0, "pt"),
      width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0),
      margin = margin(0, 1, 3, 1),
      hi.labels = c("Study 1", "Study 3"),
      hi.family = "Bangers",
      hi.fill = "firebrick",
      hi.box.col = "firebrick",
      hi.col = "white"
    ),
    legend.position = "bottom"
  )


ggsave("eda-plot2a.png", plot2a, height = 8, width = 12, dpi = 250)




#---------------------------------------
# Plot study1 and 3 only 

plot2b <-
  data %>%
  filter(STUDYID == c(1, 3)) %>% 
  ggplot(aes(
    x = BASE,
    y = AVAL,
    group = TRT01PC,
    color = TRT01PC
  )) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "grey",
    size = 1,
    alpha = 0.5
  ) +
  geom_point(alpha = 0.7, size = 1) +
  facet_grid(TRT01PC  ~ STUDYIDC) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  labs(
    x = "Mean systolic blood pressure [mmHg] at randomisation",
    y = "Mean systolic blood pressure [mmHg] at 1-year",
    color = "Treatment",
    title = "The intensive treatment arm for study 1 and 3 displayed patterns of interest",
    subtitle = "It is always important to plot data many ways",
    caption = "http://robertgrantstats.co.uk/drawmydata.html \nData: BIG_DATA_PSI_WW_DEC2020.csv"
  )  +
  theme_ipsum_rc(base_size = 16) +
  theme(
    strip.text = element_textbox_highlight(
      size = 12,
      face = "bold",
      fill = "white",
      box.color = "white",
      color = "gray40",
      halign = .5,
      linetype = 1,
      r = unit(0, "pt"),
      width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0),
      margin = margin(0, 1, 3, 1),
      hi.labels = c("Study 1", "Study 3"),
      hi.family = "Bangers",
      hi.fill = "firebrick",
      hi.box.col = "firebrick",
      hi.col = "white"
    ),
    legend.position = "bottom"
  )


ggsave("eda-plot2b.png", plot2b, height = 8, width = 12, dpi = 250)






