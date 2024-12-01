# Microbiome Challenge

# WonderfulWednesday

library(tidyverse)
library(stringr)
library(scales)
library(RColorBrewer)
library(glue)
library(janitor)
library(readr)

root_dir <- here::here()
weds_dir <- file.path(root_dir,
                      "PlatformTrials")
tt_theme <- "power"
author_initials <- "TomMarlow"

ed <- file.path(weds_dir, "ExampleDataNASH.csv")

ed_dat <- read.csv(ed, sep = ",")

pt_avg <- ed_dat %>%
  filter(!is.na(Disj_Power_BA)) %>%
  group_by(FinalCohortSampleSize, Maximumnumberofcohorts, TypeofDataSharing) %>%
  summarise(mean = mean(Disj_Power_BA)) %>%
  mutate(power = mean * 100)

fig1 <- ggplot(pt_avg,
               aes(x = FinalCohortSampleSize,
                   y = power,
                   colour = as.factor(Maximumnumberofcohorts))) +
  geom_line(linewidth = 2) +
  labs(title = "Power by cohort size and type of data sharing",
       colour = "Max number of cohorts") +
  xlab("Final cohort sample size") +
  ylab("Disjunctive Power (%)") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 10),
        strip.text =  element_text(size = 12),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 8),
        legend.position = "bottom") +
  scale_colour_viridis_d() +
  scale_fill_discrete(name = "Cohort Size") +
  facet_wrap(str_to_sentence(TypeofDataSharing) ~ .)

fig1

ggsave(
  plot = fig2,
  filename = glue("{root_dir}/{tt_theme}_{author_initials}_{Sys.Date()}.jpg"))