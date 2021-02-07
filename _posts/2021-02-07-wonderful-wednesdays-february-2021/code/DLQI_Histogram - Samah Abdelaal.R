# Load data
dql <- read.csv("ww2020_dlqi.csv")
attach(dql)
View(dql)
summary(dql)

# Load Library
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggcharts)

# Select relevant variables
dql_renamed <-
  dql %>%
  select(
    TRT, VISIT, DLQI_SCORE
    )

# Rename treatment levels
dql_renamed$TRT[dql_renamed$TRT=="A"] <- "Placebo"
dql_renamed$TRT[dql_renamed$TRT=="B"] <- "Active Treatment"

# Seperate visits

# Baseline visit
totalbaseline <- 
  dql_renamed %>% 
  filter(VISIT=="Baseline")

# Construct a histogram for each treatment arm at baseline visit
(d <- 
  ggplot(
    data = totalbaseline,
    aes(
      x = DLQI_SCORE
      ))
  + geom_histogram(
    binwidth = 1.5,
    color = "grey",
    fill = "deeppink3"
    ) +
  facet_grid(~ TRT)
  + theme_ng(grid = "X")
  + labs(
    x = "DLQI Score",
    y = "Count",
    title = "Total DLQI Score",
    subtitle = "At Baseline",
    caption = "Samah Abdelaal")
  + theme(
    axis.title.x = element_blank(),
    plot.title = element_text(size = 20,
                              face = "bold"),
    plot.subtitle = element_text(size = 18),
    plot.caption = element_text(size = 15,
                                face = "bold.italic")
    ))


# Week 16 visit
totalweek16 <- 
  dql_renamed %>% 
  filter(VISIT=="Week 16")

(e <- 
  ggplot(
    data = totalweek16,
    aes(
      x = DLQI_SCORE
      )
    )
  + geom_histogram(
    binwidth = 1.5,
    color = "grey",
    fill = "deeppink3"
    ) +
  facet_grid(~ TRT)
  + theme_ng(grid = "X")
  + labs(
    x = "DLQI Score",
    y = "Count",
    subtitle = "At Week 16"
    ) +
  theme(
    plot.subtitle = element_text(size = 18)
    ))

# Compine plots
library(gridExtra)

gridExtra::grid.arrange(d, e, nrow = 2)
