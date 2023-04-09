library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(ggalluvial)
library(RColorBrewer)

df <- read.csv2("ww eortc qlq-c30 missing.csv", sep=",") %>%
  as_tibble() 
df
df_1 <- df %>%
  pivot_longer(cols=starts_with("WEEK"), names_to = "AVISIT", values_to = "AVAL") %>%
  select(USUBJID, ARM, LASTVIS, AGE:AVAL) %>%
  mutate(AVAL=as.factor(if_else(AVAL=="", "Missing", AVAL)))
df_1  
levels(df_1$AVAL) <- c(as.character(rev(round(seq(0, 100, 8.333333333), 1))), "Missing")

cc <- scales::div_gradient_pal(low = "#a50026", mid="#ffffbf", high = "#313695", "Lab")(seq(0,1,length.out=13))

colors <- c(cc, "#D3D3D3")
show_col(colors)

ggplot(df_1,
       aes(
         x = AVISIT,
         stratum = AVAL,
         alluvium = USUBJID,
         fill = AVAL,
         label = AVAL
       )) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = paste("Wk", seq(0, 48, 3))) +

  geom_flow(stat = "alluvium", 
            lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  labs(title = "Quality of Life - Missing data depends on age and treatment received") +
  facet_wrap(AGEGR~ARM, nrow = 4, scales = "free_y", strip.position = c("left")) +
  # facet_grid(cols = vars(ARM), rows = vars(AGEGR), scales = "free") +
  theme_bw() +
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  theme(
    #panel.background = element_blank(),
    #axis.text.y = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    axis.text = element_text(angle = 90),
    legend.direction = "horizontal"
  ) 

ggsave(filename = "sankey_chart.png", device = "png", width = 16, height = 9)
