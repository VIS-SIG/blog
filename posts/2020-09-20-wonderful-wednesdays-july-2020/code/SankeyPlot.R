library(ggalluvial)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

Orange <- "#EF7F04"
Green <- "#68B937"
Blue <- "#00A6CB"
Grey <- "#4E5053"
Darkblue <- "#003569"
Yellow <- "#FFBB2D"


sankey <- read.csv('Sankey.csv')
sankey$CHG_CAT <- as.factor(sankey$CHG_CAT)
sankey$BROAD_CHG_CAT <- as.factor(sankey$BROAD_CHG_CAT)
sankey$AVISITN <- as.factor(sankey$AVISITN)
sankey$TRT01P <- as.factor(sankey$TRT01P)
sankey$TARGET <- as.factor(sankey$TARGET)

p <- sankey %>%
#  filter(TRT01PN==1) %>%
 ggplot(aes(x = AVISITN, stratum = CHG_CAT, alluvium=USUBJID, fill = CHG_CAT)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(color = "darkgray", aes.flow = "backward") +
  geom_stratum() +
  theme(legend.position = "bottom") 



p + scale_fill_manual(values = c("white", "#3FFDBE", "#39E3DA", "#4AD5FA", "#3991E3"), limits=c("1. Baseline", "2. <1g/dl", "3. 1-2g/dl", "4. 2-3g/dl","5. >3g/dl"), name="Change in Hemoglobin \n since previous visit") + 
  scale_x_discrete("Visit", labels = c("10" = "Baseline",
                                       "20" = "Week 4",
                                       "30" = "Week 8",
                                       "40" = "Week 12",
                                       "50" = "Week 16",
                                       "60" = "Week 20",
                                       "70" = "Week 24"
)) +
  scale_y_continuous("Number of patients") +
  facet_grid(.~ TRT01P) +
  ggtitle(label = "Hemoglobin change across observation period was more variable in the control group", subtitle = "often fluctuating by more than 2g/dl") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 20, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18),
    legend.key.size = unit(3, "line"))


