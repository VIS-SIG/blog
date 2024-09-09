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
tues_dir <- file.path(root_dir,
                      "microbiome")
tt_theme <- "microbiome"
author_initials <- "TomMarlow"
azTidyTuesday_date <- "20240228"

# Relative abundance of each microbial community in each woman in pregnancy and the perinatal period
ra <- file.path(tues_dir, "202402_Collaboration_DataVizESIG-BiomarkersESIG_relative_abundance.csv")
# Taxonomy information of each microbial community
ti <- file.path(tues_dir, "202402_Collaboration_DataVizESIG-BiomarkersESIG_taxonomy_info.csv")
# Clinical information of each woman (simulated data)
sm <- file.path(tues_dir, "202402_Collaboration_DataVizESIG-BiomarkersESIG_simulated_metadata.csv")

rel_dat <- read.csv(ra, sep = ";")
sim_dat <- read.csv(sm, sep = ";")
tax_dat <- read.csv(ti, sep = ";")

rel_id <- rel_dat %>%
  pivot_longer(.,
               cols = 2:ncol(.),
               names_to = "id",
               values_to = "rel_abu",
               values_drop_na = TRUE) %>%
  mutate(sample_id = substr(id, 2, nchar(id))) %>%
  filter(rel_abu != 0)

# Merge to get audio characteristics

microb <- merge(x = rel_id, 
                y = sim_dat, 
              by.x = c("sample_id"), 
              by.y = c("sample_id"), 
              all.y = FALSE) 
        

# Taxonomy details

micro_tax <- merge(x = microb, 
                y = tax_dat, 
                by.x = c("taxID"), 
                by.y = c("taxID"), 
                all.y = FALSE) %>%
  mutate(abu = as.numeric(gsub(",", ".", rel_abu)))

table(micro_tax$phylum, micro_tax$group)
table(micro_tax$class, micro_tax$group)

table(sim_dat$subject_id)

msd_pat <- micro_tax %>%
  group_by(group, subject_id, visit_number, class) %>%
  summarise(suma = sum(abu)) %>%
  mutate(sump = suma * 100) %>%
  mutate(visits = if_else(visit_number >= 5, 5, visit_number)) %>%
  mutate(Class = gsub("c__", "", class))

unique(msd_pat$Class)

cls_avg <- msd_pat %>%
  group_by(Class) %>%
  summarise(mean = mean(sump)) %>%
  arrange(-mean)

cls_pop <- as.data.frame(table(msd_pat$Class)) %>%
  arrange(-Freq) %>%
  rename(Class = Var1)

msd_pat <- micro_tax %>%
  group_by(group, subject_id, visit_number, phylum) %>%
  summarise(suma = sum(abu)) %>%
  mutate(sump = suma * 100) %>%
  mutate(visits = if_else(visit_number >= 5, 5, visit_number)) %>%
  mutate(Phylum = gsub("p__", "", phylum))
write.csv(msd_pat, file.path(tues_dir, "pat.csv"))

phy_avg <- msd_pat %>%
  group_by(Phylum) %>%
  summarise(mean = mean(sump)) %>%
  arrange(-mean)

phy_pop <- as.data.frame(table(msd_pat$Phylum)) %>%
  arrange(-Freq) %>%
  rename(Phylum = Var1)

# Remove low frequency phylums / classes

phy_hi <- c(unlist(phy_pop$Phylum[phy_pop$Freq >= 20]))
cls_hi <- c(unlist(cls_pop$Class[cls_pop$Freq >= 1000]))


# Line plot

lin_dat <- msd_pat %>%
  group_by(Class, group, visit_number) %>%
  summarise(meanab = mean(sump)) %>%
  mutate(Class = factor(Class,
                         levels = cls_avg$Class,
                         labels = cls_avg$Class)) %>%
  filter(Class %in% cls_hi) 

line_fig <- ggplot() +
  geom_line(
    data = lin_dat, linewidth = 2,
    aes(
      x = visit_number, y = meanab, group = group,
      colour = group
    )
  ) +
  xlab("") +
  ylab(NULL) +
  labs(
    title = "Mean relative abundance by microbiome class, term and visit number",
    caption = "Multi-Omic Microbiome Study: Pregnancy Initiative (MOMS-PI) is part of the longitudinal Human Microbiome Project (HMP)",
    colour = "Term",
    x = "Visit number",
    y = "Mean abundance %"
  ) +
  theme_bw() +
  theme(
    strip.text.x = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8, angle = 0, hjust = .5),
    legend.position = "bottom"
  ) +
  scale_color_viridis_d(option = "viridis") +
  scale_fill_viridis_d(option = "viridis") +
  facet_wrap(. ~ Class)

line_fig

ggsave(
  plot = line_fig,
  filename = glue("{root_dir}/azTidyTuesday_{azTidyTuesday_date}_{tt_theme}_{author_initials}_{Sys.Date()}.jpg"),
  width = 16,
  height = 9)
