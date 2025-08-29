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

## visual

(p_pt <- adae_soc_pt %>%
  ggplot(aes(x = reorder(AETERM, abs(n_pt), decreasing = T), y = n_pt)) +
  geom_bar(
    stat = "identity", width = 0.5,
    aes(fill = AESOC, group = ARMCD, color = ARMCD)
  ) +
  scale_fill_brewer(palette = "Greens") +
  scale_color_brewer(palette = "Paired") +
  labs(
    title = "Number of patients with PT",
    x = "",
    y = ""
  ) +
  scale_y_continuous(labels = abs) +
  geom_hline(yintercept = 0, color = "grey20", size = 0.8) +
  mytheme() +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust = 0.5)))

tiff("adae_pt.tiff", units = "in", width = 8, height = 5, res = 150)
print(p_pt)
dev.off()

### Co-occurrence --------------------------

adae_dummy <- fastDummies::dummy_cols(adae, select_columns = "AETERM") %>%
  select(ARMCD, USUBJID, starts_with("AETERM_trm")) %>%
  group_by(ARMCD, USUBJID) %>%
  summarise(across(everything(), sum)) %>%
  ungroup()

cor <- round(cor(adae_dummy %>% select(starts_with("AETERM_trm")),
  method = "spearman"
), 2)

(p_cor <- ggcorrplot(cor,
  method = "circle",
  type = "full",
  lab = "TRUE",
  outline.color = "white",
  colors = c("black", "white", "red"),
  lab_size = 3,
  show.legend = F
) + mytheme() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) +
  labs(
    title = "AE Co-occurence at Patient Level",
    x = "",
    y = "",
    caption = "Correlation is calculated at patient level based on number of events, using Spearman's correlation matrix."
  ))

(p_pt_2 <- adae_soc_pt %>%
  ggplot(aes(x = AETERM, y = n_pt)) +
  geom_bar(
    stat = "identity", width = 0.5,
    aes(fill = AESOC, group = ARMCD)
  ) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Number of patients with PT",
    x = "",
    y = "Arm B - Arm A"
  ) +
  scale_y_continuous(labels = abs) +
  geom_hline(yintercept = 0, color = "grey20", size = 0.8) +
  mytheme() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ))

(combined_p_cor <- (p_pt_2 / p_cor))

tiff("adae_cor.tiff", units = "in", width = 10, height = 10, res = 300)
print(combined_p_cor)
dev.off()
