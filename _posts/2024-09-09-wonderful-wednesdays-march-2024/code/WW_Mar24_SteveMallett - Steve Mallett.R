library(readr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(RColorBrewer)
library(paletteer)
library(colorspace)


################################################
# Combine abundance, taxonomy and clinical data

clin1 <- read_csv2("./Data/Metadata.csv") %>%
  arrange(subject_id, visit_number)

tax <- read_csv2("./Data/Taxonomy_info.csv")

abun1 <- read_csv2("./Data/Relative_abundance.csv") %>%
  gather(key = "sample_id", value = "Value", -taxID) %>%
  group_by(taxID, sample_id) %>%
  filter(Value>0)

abun3 <- left_join(clin1, abun1, by = "sample_id") 
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
names(myColors) <- levels(overall_most.prev$genus)

make.plot <- function(gp, vs) {

  tmp <- abun5 %>% 
    filter(group == gp & visit_number == vs) %>% 
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
    scale_fill_manual(name = "genus", values = myColors) +
    scale_y_continuous(limits=c(-0.2, 1)) +
    theme(
      panel.background=element_rect(fill="white"),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_blank(),
      legend.text=element_text(size=14),
      legend.title=element_text(size=14)
    ) +
    annotate("text", label = str_c("Visit ", vs), x = 0, y = -0.2, color = "black", size=5)
}

p1 <- make.plot("Preterm delivery", 1)
p2 <- make.plot("Preterm delivery", 2)
p3 <- make.plot("Preterm delivery", 3)
p4 <- make.plot("Preterm delivery", 4)
p5 <- make.plot("Term delivery", 1)
p6 <- make.plot("Term delivery", 2)
p7 <- make.plot("Term delivery", 3)
p8 <- make.plot("Term delivery", 4)

p_ptd <- plot_grid(
  p1 + theme(legend.position="none"),
  p2 + theme(legend.position="none"),
  p3 + theme(legend.position="none"),
  p4 + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  ncol = 4,
  nrow = 1
)

p_td <- plot_grid(
  p5 + theme(legend.position="none"),
  p6 + theme(legend.position="none"),
  p7 + theme(legend.position="none"),
  p8 + theme(legend.position="none"),
  align = 'vh',
  labels = c("  ", " ", " ", " "),
  hjust = -1,
  ncol = 4,
  nrow = 1
)

################################################
# Formatting and output to file

title0 <- ggdraw() +
  draw_label("Longitudinal Human Microbiome Project (MOMS-PI Pre-Term Birth Study)\n", size = 20, color = "blue")

title1 <- ggdraw() +
  draw_label("Pre-Term Delivery", size = 22)

title2 <- ggdraw() + 
  draw_label("Term Delivery", size = 22)

legend <- get_legend(
  p2 + theme(legend.box.margin = margin(0, 0, 0, 12))
)

cap <- "How to read this chart: Each wheel represents a set of samples from the Pre-Term or Term Delivery cohort at a particular visit. Each spoke in the wheel represents a single participant in the study, with coloured segments representing the relative abundance
of bacteria types (genus) in the sample. For simplicity, only the most prevalent genus types and the first four visits are included. Approximately 75-80% of samples had Lactobacillus as the dominant genus, with no obvious difference between visits or cohorts."

caption <- ggdraw() + 
  draw_label(cap, size = 14)

ptd2 <- plot_grid(title1, p_ptd, ncol=1, rel_heights = c(1.7, 25), align = "v")  
td2 <- plot_grid(title2, p_td, ncol=1, rel_heights = c(1.7, 25), align = "v") 
all1 <- plot_grid(ptd2, td2, ncol=1, rel_heights = c(1, 1), align = "v") 
all2 <- plot_grid(all1, legend, rel_widths = c(3, 0.5))
all3 <- plot_grid(title0, all2, caption, ncol=1, rel_heights = c(8, 100, 5), align = "v") +
  theme(plot.background = element_rect(fill = "white", colour = "blue"))

ggsave("./WW_Mar24_01_OkabeIto.png", all3, width=24, height=12, dpi=300)

################################################
