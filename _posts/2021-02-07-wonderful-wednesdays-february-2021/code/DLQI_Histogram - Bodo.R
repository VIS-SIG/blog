# Load data
dql <- read.csv("O:\\1_Global_Biostatistics\\Biostatistics Innovation Center\\BIC Project - Subgroup Analyses\\Screening\\R-Package\\Supports\\WW\\ww2020_dlqi.csv")
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

# Seperate treatments

# Active
totalB <- 
  dql_renamed %>% 
  filter(TRT=="Active Treatment")

# Construct a histogram for each treatment arm at baseline visit
(d <- 
  ggplot(
    data = totalB,
    aes(
      x = DLQI_SCORE
      ))
  + geom_histogram(
    binwidth = 1,
    color = "grey",
    fill = "deeppink3"
    ) +
  facet_grid(~ VISIT)
  + theme_ng(grid = "X")
  + labs(
    x = "Total DLQI Score",
    y = "Patients",
    title = "Improved Quality of life after 16 weeks of treatment",
    subtitle = "Active Treatment")
  + theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    plot.title = element_text(size = 17,
                              face = "bold"),
    plot.subtitle = element_text(size = 15, color = "deeppink3")
    ))


# Week 16 visit
totalA <- 
  dql_renamed %>% 
  filter(TRT=="Placebo")

(e <- 
  ggplot(
    data = totalA,
    aes(
      x = DLQI_SCORE
      )
    )
  + geom_histogram(
    binwidth = 1,
    color = "grey",
    fill = "green4"
    ) +
  facet_grid(~ VISIT)
  + theme_ng(grid = "X")
  + labs(
    x = "Total DLQI Score",
    y = "Patients",
    subtitle = "Placebo",
    caption = "Lower score equals better quality of life"
    ) +
  theme(
    strip.text.x = element_blank(),
    plot.subtitle = element_text(size = 15, color = "green4"),
    plot.caption = element_text(size = 12,
                                face = "italic")
    ))

# Compine plots
library(gridExtra)

gridExtra::grid.arrange(d, e, nrow = 2, heights = c(1.5,1))
