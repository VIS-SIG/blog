##############################################
# Author: Wei Quan
# Latest Update: 04 Feb 2025
# PSI Visualization SIG Wonderful Wednesday: Adverse events
# Sample ADAE dataset generated using teal.data package in R.
# Data limited to two treatments (Drug X, Placebo),although subjects may switch between the two
# Challenge: produce a visualization using this sample ADAE dataset The challenge is open-ended,
# and you can choose a feature of the data to focus on.
# Possible ideas:
# Investigate the co-occurrence of adverse events (the October 2020 webinar might provide some inspiration!)
# or ‘clusters’ of adverse events – do certain groups of adverse events tend to occur alongside one another or at certain times
# Investigate the time to a given event or adverse event profiles over time
# Explore the hierarchical nature of adverse events (MedDRA terms)
###############################################

## Load the packages -----------------
library(tidyverse)
library(survival)
library(ggcorrplot)
library(patchwork)

## Set up environment----------------
rm(list = ls())
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(warn = 0)

mytheme <- function() {
  list(
    theme_minimal(),
    theme(
      plot.title = element_text(size = 16),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 12)
    )
  )
}

## Read data---------

adae <- read.csv("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/refs/heads/master/data/2025/2025-01-08/DummyAEData.csv") %>%
  select(-X) %>%
  filter(SAFFL == "Y")

# total
length(unique(adae$USUBJID))
summary(adae)
head(adae)

## Summary table------------

adae_n_arm <- adae %>%
  group_by(ARMCD) %>%
  summarise(n_arm = n_distinct(USUBJID))

adae_n_soc <- adae %>%
  group_by(AESOC) %>%
  summarise(n_soc = n_distinct(USUBJID))

adae_n_pt <- adae %>%
  group_by(AETERM) %>%
  summarise(n_pt = n_distinct(USUBJID))

## Visual-----------

### SOC by PT-------------------
(adae_soc_pt <- adae %>%
  group_by(ARMCD, AESOC, AETERM) %>%
  summarise(n_pt = n_distinct(USUBJID)) %>%
  right_join(adae_n_arm) %>%
  group_by(AETERM) %>%
  mutate(n_all = sum(n_pt)) %>%
  ungroup() %>%
  mutate(n_pt = ifelse(ARMCD == "ARM B", -n_pt, n_pt)))


### tte--------------

(adae_tte <- adae %>%
  select(USUBJID, ARMCD, AESOC, AETERM, AETOXGR, ASTDY, AENDY) %>%
  arrange(USUBJID, ARMCD, AESOC, AETERM, AETOXGR) %>%
  mutate(event = 1))

adae_tte_counts <- adae_tte %>%
  group_by(AESOC, ARMCD) %>%
  summarise(n = n()) %>%
  ungroup()

adae_tte_g3_counts <- adae_tte %>%
  filter(AETOXGR >= 3) %>%
  group_by(AESOC, ARMCD) %>%
  summarise(n = n()) %>%
  ungroup()

(adae_tte_subject <-
  adae_tte %>%
  group_by(ARMCD, USUBJID) %>%
  mutate(
    any_AE = ifelse(n() > 0, 1, 0),
    time_to_onset_any_AE = ifelse(n() > 0, min(ASTDY), Inf),
    time_to_stop_any_AE = ifelse(n() > 0, max(ASTDY), Inf)
  ) %>%
  group_by(AESOC, ARMCD) %>%
  mutate(n = n_distinct(USUBJID)) %>%
  ungroup())

(p1 <- adae_tte %>%
  ggplot() +
  geom_violin(aes(x = AESOC, y = ASTDY, fill = ARMCD),
    scale = "count"
  ) +
  labs(
    title = "AE onset by SOC",
    x = "SOC",
    y = "Time from first dose (days)"
  ) +
  geom_text(
    data = adae_tte_counts,
    aes(
      x = AESOC, y = -40, label = paste0("N=", n),
      group = ARMCD
    ),
    position = position_dodge(width = 0.75),
    size = 3
  ) +
  scale_y_continuous(limits = c(-50, 1200)) +
  mytheme() +
  coord_flip())

(p2 <- adae_tte %>%
  ggplot() +
  geom_violin(aes(x = AESOC, y = AENDY - ASTDY + 1, fill = ARMCD),
    scale = "count"
  ) +
  labs(
    title = "AE resolution by SOC",
    x = "SOC",
    y = "Time since AE onset (days)"
  ) +
  geom_text(
    data = adae_tte_counts,
    aes(
      x = AESOC, y = -40, label = paste0("N=", n),
      group = ARMCD
    ),
    position = position_dodge(width = 0.75),
    size = 3
  ) +
  scale_y_continuous(limits = c(-50, 1200)) +
  mytheme() +
  coord_flip())

(p3 <- adae_tte %>%
  filter(AETOXGR >= 3) %>%
  ggplot() +
  geom_violin(aes(x = AESOC, y = ASTDY, fill = ARMCD),
    scale = "count"
  ) +
  labs(
    title = "AE onset by SOC (Grade >=3)",
    x = "SOC",
    y = "Time from first dose (days)"
  ) +
  geom_text(
    data = adae_tte_g3_counts,
    aes(
      x = AESOC, y = -40, label = paste0("N=", n),
      group = ARMCD
    ),
    position = position_dodge(width = 0.75),
    size = 3
  ) +
  scale_y_continuous(limits = c(-50, 1200)) +
  mytheme() +
  coord_flip())

(p4 <- adae_tte %>%
  filter(AETOXGR >= 3) %>%
  ggplot() +
  geom_violin(aes(x = AESOC, y = AENDY - ASTDY + 1, fill = ARMCD),
    scale = "count"
  ) +
  labs(
    title = "AE resolution by SOC (Grade >=3)",
    x = "SOC",
    y = "Time since AE onset (days)"
  ) +
  geom_text(
    data = adae_tte_g3_counts,
    aes(
      x = AESOC, y = -40, label = paste0("N=", n),
      group = ARMCD
    ),
    position = position_dodge(width = 0.75),
    size = 3
  ) +
  scale_y_continuous(limits = c(-50, 1200)) +
  mytheme() +
  coord_flip())

(combined_p <- (p1 | p2) / (p3 | p4) + plot_layout(guides = "collect"))

tiff("adae_tte.tiff", units = "in", width = 13, height = 7, res = 300)
print(combined_p)
dev.off()
