library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)
library(emojifont)
library(ggtext)

adsl <- read_xpt("adsl.xpt")

adsl2 <- adsl %>%
  mutate(agegp2 = cut(AGE, quantile(AGE, c(0:6/6)))) %>%
  group_by(agegp2) %>%
  arrange(AGE) %>%
  select(agegp2, BMIBL, SEX, AGE) %>%
  mutate(id = as.numeric(substr(agegp2,2,3))) %>%
  drop_na(id)

# Create a vector containing unique values for age group identifier
id <- adsl2 %>%
  group_by(id) %>%
  slice(1) %>%
  select(id) %>%
  unlist() %>%
  as.vector()

for (i in id) {
  
  ADSLloop <- adsl2 %>%
    filter(id == i)
  
  plot <- ggplot(ADSLloop, aes(x = BMIBL, colour = SEX)) +
    geom_density(show.legend = FALSE) +
    geom_text(aes(x=25, y=0.19, label="Age Group:"), size=16, color="#7570b3") +
    geom_text(aes(x=28, y=0.19, label=agegp2), size=16, color="#7570b3") +
    scale_x_continuous("BMI", limits=c(15, 40)) +
    scale_y_continuous("Frequency", limits=c(0, 0.2)) +
    scale_color_discrete(type=c("#1b9e77", "#d95f02")) +
    theme(plot.title = element_markdown(colour = "#636363",
                                        size = 50),
          panel.background=element_rect(fill="white"),
          panel.grid.major=element_line(colour = "#f0f0f0",
                                        linewidth = 0.5,
                                        linetype = 1),
          axis.line=element_line(colour = "#bdbdbd",
                                 linewidth = 0.5,
                                 linetype = 1),
          axis.text=element_text(
            colour = "#636363",
            size = 28),
          axis.title=element_text(
            colour = "#636363",
            size = 32)) +
    labs(title="<b>Distribution of BMI by Sex (<span style='color:#1b9e77'>Female</span> and <span style='color:#d95f02'>Male</span>) and  <span style='color:#7570b3'>Age Group</span></b>") 

  ggsave(
    filename = paste0("frames/plot", sprintf("%03d", i) , ".png"),
    plot = plot,
    device = "png",
    height = 1500,
    width = 2757,
    units = "px"
  )
}

# Additional steps to create animated gif file:
# 1. Install ImageMagick
# 2. Open a command line
# 3. cd to the folder containing the *.png files 
# 4. Enter the following command:
#   magick convert -delay 150 -loop 0 *.png -scale 4800x2700 WW_Dec23_SM01.gif