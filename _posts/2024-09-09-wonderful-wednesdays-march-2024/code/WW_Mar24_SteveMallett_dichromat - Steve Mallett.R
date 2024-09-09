library(readr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(paletteer)
library(colorspace)
library(dichromat)

################################################
# Combine abundance, taxonomy and clinical data

clin1 <- read_csv2("./Data/Metadata.csv") %>%
  arrange(subject_id, visit_number)

tax <- read_csv2("./Data/Taxonomy_info.csv")

abun1 <- read_csv2("./Data/Relative_abundance.csv") %>%
  gather(key = "sample_id", value = "Value", -taxID) %>%
  group_by(taxID, sample_id) %>%
  filter(Value>0)

abun3 <- left_join(clin1, abun2, by = "sample_id") 
abun4 <- left_join(tax, abun3, by = "taxID") %>% 
  arrange(subject_id, visit_number, taxID)

################################################
# Identify most prevalent taxIDs and subset data

num_gen <- 6

tax_prev <- abun4 %>% 
  group_by(genus) %>% 
  summarise(mn = mean(Value)) %>% 
  arrange(desc(mn)) %>% 
  select(genus) 

tax_prev2 <- tax_prev[1:num_gen,] %>% 
  as_vector()

abun5 <- abun4 %>% 
  filter(genus %in% tax_prev2)

################################################
# Groups subjects by most prevalent genus

overall_most.prev<- abun5 %>% 
  group_by(genus) %>% 
  summarise(sum = sum(Value)) %>% 
  arrange(desc(sum)) %>% 
  select(genus) %>% 
  mutate(sort.order = row_number())

################################################
# Create plots

myColors <- brewer.pal(num_gen,"Set1")
protan <- dichromat(myColors , type = "protan")
deutan <- dichromat(myColors , type = "deutan")
tritan <- dichromat(myColors , type = "tritan")

names(myColors) <- levels(overall_most.prev$genus)

make.plot <- function(pal_lab,pal) {

  tmp <- abun5 %>% 
    filter(group == "Preterm delivery" & visit_number == 1) %>% 
    group_by(subject_id, genus) %>% 
    summarise(sum = sum(Value)) %>% 
    arrange(subject_id, desc(sum))
  # select(subject_id, genus, Value)

  sub_most.prev <- tmp %>% 
    group_by(subject_id, genus) %>% 
    arrange(subject_id, desc(sum)) %>% 
    group_by(subject_id) %>%
    slice(1) %>% 
    select(subject_id, genus)
  
  sub_most.prev2 <- left_join(sub_most.prev, overall_most.prev, by = "genus") %>% 
    ungroup() %>% 
    select(subject_id, sort.order)
  
  tmp2 <- left_join(tmp, sub_most.prev2, by = "subject_id")
  
  subID <- tmp2 %>% 
    group_by(sort.order, subject_id) %>% 
    distinct(subject_id) %>% 
    arrange(sort.order) %>% 
    ungroup() %>% 
    mutate(new.subjid = row_number()) %>% 
    select(subject_id, new.subjid)
  
  tmp3 <- left_join(tmp2, subID, by = "subject_id") 
  
  ggplot(tmp3, aes(fill=genus, y=sum, x=new.subjid)) + 
    geom_bar(show.legend = TRUE,
             position="fill", stat="identity", width=1, color="black", linewidth=0.1) +
    coord_polar(theta="x") +
    scale_fill_manual(name = "genus", values = pal) +
    scale_y_continuous(limits=c(-0.2, 1)) +
    theme(
      panel.background=element_rect(fill="white"),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_blank(),
      legend.text=element_text(size=14),
      legend.title=element_text(size=14)
    ) +
    labs(title=pal_lab)
}

p1 <- make.plot("Original", myColors)
p2 <- make.plot("Protanopic", protan)
p3 <- make.plot("Deutanopic", deutan)
p4 <- make.plot("Tritanopic", tritan)

all <- plot_grid(p1, p2, p3, p4, 
  align = 'vh',
  hjust = -1,
  ncol = 2,
  nrow = 2
)


ggsave("./WW_Mar24_colorblindness.png", all, width=24, height=12, dpi=300)

################################################
