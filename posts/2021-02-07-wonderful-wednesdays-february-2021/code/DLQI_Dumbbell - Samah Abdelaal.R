# Load data
dql <- read.csv("ww2020_dlqi.csv")
attach(dql)
View(dql)

# Load library
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggcharts)
library(ggalt)

# Seperate treatment arms
new_A <- 
  dql %>% 
  filter(TRT=="A")
new_B <- 
  dql %>% 
  filter(TRT=="B")

## Treatment A : Placebo

dtA <- new_A %>%
  # Remove missing data
  filter(!is.na(new_A)) %>% 
  # Select relevant variables
  select(
    DLQI101, DLQI102, DLQI103, DLQI104, DLQI105,
    DLQI106, DLQI107, DLQI108, DLQI109, DLQI110,
    VISIT
    ) %>%
  # Summarize mean score for each question grouped by visit
  # while also renaming variables to indicate the meaning of each score
  group_by(VISIT) %>%
  summarise(
    Symptoms = mean(DLQI101, na.rm = T),
    Embarrassment = mean(DLQI102, na.rm = T),
   `Shopping and home care` = mean(DLQI103, na.rm = T),
    Clothes = mean(DLQI104, na.rm = T),
   `Social and leisure` = mean(DLQI105, na.rm = T),
    Sport = mean(DLQI106, na.rm = T),
    `Work and study` = mean(DLQI107, na.rm = T),
    `Close relationships` = mean(DLQI108, na.rm = T),
    Sex = mean(DLQI109, na.rm = T),
    Treatment = mean(DLQI110, na.rm = T)
   )

# Tidying data
dtA <- 
  dtA %>% 
  pivot_longer(
    !VISIT,
    names_to = "Domain",
    values_to = "Mean_Score"
    )

# Seperating the visit variable into baseline and week 16
dtA <-
  dtA %>% 
  pivot_wider(
    names_from = VISIT,
    values_from = Mean_Score
    )

# Ensuring the domain levels are ordered the same
dtA <-
  dtA %>% 
  mutate(
    Domain = factor(Domain,
                    levels = c("Symptoms", "Embarrassment",
                               "Shopping and home care",
                               "Clothes", "Social and leisure",
                               "Sport", "Work and study",
                               "Close relationships",
                               "Sex", "Treatment"))
    )

# Constructing a dumbbell plot using ggalt package with a ggchart theme

(a <- 
  ggplot()+
  geom_dumbbell(
    data = dtA,
    aes(
      y = Domain,
      x = Baseline,
      xend = `Week 16`
      ),
    size = 1.5,
    color = "lightgray",
    size_x = 3,
    colour_x = "violetred4",
    size_xend = 3,
    colour_xend = "maroon1"
    )
  + theme_ggcharts(grid = "Y")
  + labs(
    title = "Placebo"
    )
  + theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 9)
    ))

## Treatment B : Active Treatment

dtB <- new_B %>%
  # Remove missing data
  filter(!is.na(new_B)) %>% 
  # Select relevant variables
  select(
    DLQI101, DLQI102, DLQI103, DLQI104, DLQI105,
    DLQI106, DLQI107, DLQI108, DLQI109, DLQI110,
    VISIT
    ) %>%
  # Summarize mean score for each question grouped by visit
  # while also renaming variables to indicate the meaning of each score
  group_by(VISIT) %>%
  summarise(
    Symptoms = mean(DLQI101, na.rm = T),
    Embarrassment = mean(DLQI102, na.rm = T),
    `Shopping and home care` = mean(DLQI103, na.rm = T),
    Clothes = mean(DLQI104, na.rm = T),
    `Social and leisure` = mean(DLQI105, na.rm = T),
    Sport = mean(DLQI106, na.rm = T),
    `Work and study` = mean(DLQI107, na.rm = T),
    `Close relationships` = mean(DLQI108, na.rm = T),
    Sex = mean(DLQI109, na.rm = T),
    Treatment = mean(DLQI110, na.rm = T)
    )

# Tidying data
dtB <- 
  dtB %>% 
  pivot_longer(
    !VISIT,
    names_to = "Domain",
    values_to = "Mean_Score"
    )

# Seperating the visit variable into baseline and week 16
dtB <-
  dtB %>% 
  pivot_wider(
    names_from = VISIT,
    values_from = Mean_Score
    )

# Ensuring the domain levels are ordered the same
dtB <-
  dtB %>% 
  mutate(
    Domain = factor(Domain,
                    levels = c("Symptoms", "Embarrassment",
                               "Shopping and home care",
                               "Clothes", "Social and leisure",
                               "Sport", "Work and study",
                               "Close relationships",
                               "Sex", "Treatment"))
    )

# Constructing a dumbbell plot using ggalt package with a ggchart theme

(b <- 
  ggplot()+
  geom_dumbbell(
    data = dtB,
    aes(
      y = Domain,
      x = Baseline,
      xend = `Week 16`
      ),
    size = 1.5,
    color = "lightgray",
    size_x = 3,
    colour_x = "violetred4",
    size_xend = 3,
    colour_xend = "maroon1")
  + theme_ggcharts(grid = "Y") 
  + labs(
    title = "Active Treatment"
    ) + theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 9)
        ))

## Mean total score for both treatment arms

totaldt <- 
  dql %>%
  # Remove missing data
  filter(!is.na(dql)) %>%
  # select and summarizing relevant variables
  select(DLQI_SCORE, TRT, VISIT) %>%
  group_by(TRT, VISIT) %>%
  summarise(
    qtotal = mean(DLQI_SCORE, na.rm = T)
    )

# Tidying data
totaldt <- 
  totaldt %>% 
  pivot_wider(
    names_from = VISIT,
    values_from = qtotal
    )

totaldt$TRT[totaldt$TRT=="A"] <- "Placebo"
totaldt$TRT[totaldt$TRT=="B"] <- "Active Treatment"

# Constructing a dumbbell plot using ggcharts

(c <- 
  dumbbell_chart(
    data = totaldt,
    x = TRT,
    y1 = Baseline,
    y2 = `Week 16`,
    line_color = "lightgray",
    line_size = 3,
    point_color = c("violetred4", "maroon1"),
    point_size = 7
  ) + labs(
    x = NULL,
    y = NULL,
    title = "Dermatological Life Quality Index DLQI",
    subtitle = "Change in mean scores from Baseline to Week 16  (Top chart is total score)",
    caption = "Samah Abdelaal"
    ) + theme(
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(size = 14,
                              face = "bold"),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 11,
                                face = "italic"),
    legend.position = "bottom"
    ))

# Compine all three plots
library(gridExtra)

grid.arrange(c, arrangeGrob(b, a, ncol = 2), nrow = 2)
