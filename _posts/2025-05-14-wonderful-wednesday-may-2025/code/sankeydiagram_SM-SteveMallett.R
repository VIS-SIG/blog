library(tidyverse)
library(ggalluvial)
library(ggtext)

df <- read_csv(WW_GCdose.csv) 

weekly <- df %>%
   mutate(
    dl = case_when(
      AVAL2 <= 0.2 ~ "4",
      AVAL2 <= 0.5 ~ "3",
      AVAL2 <= 1.0 ~ "2",
      AVAL2 > 1.0 ~ "1"
    )
  ) %>%
  group_by(SUBJID, AVISITN) %>%
  filter(!duplicated(AVAL2)) %>%
  ungroup()

colours <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")
weekly$dl <- as.factor(weekly$dl)
weekly$AVISITN <- as.factor(weekly$AVISITN)

p <- ggplot(data=weekly, aes(x = AVISITN, stratum = dl, alluvium=SUBJID, fill=dl)) +
  geom_flow(color = "black", aes.flow = "forward") +
  geom_stratum() +
  scale_x_discrete("Week", 
                   limits = c("0", "1", "2", "3", "4","5", "6", "7","8"),
                   labels = c("0" = "BL", "1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7","8" = "8"),
                   expand=c(0,0)) +
  scale_y_continuous("Number of Patients", 
                     expand=c(0,0)) +  
  scale_fill_manual(values = rev(colours),
                    labels = rev(c("<= 0.2", "> 0.2 and <= 0.5", "> 0.5 and <=1.0", "> 1.0")),
                    name = "GC Dose (mg/kg/day)") +
  labs(title="<b>Use of <span style='color:#d7301f'>High Dose GC</span> Reduces Steadily After Week 2, With Stable Dose Achieved by Week 8</span></b>") +
  theme_minimal(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_markdown(colour = "black",
                                      size = 16)
        )

out <- "C:\\Users\\malle\\OneDrive\\OneDrive Documents\\_Steve\\Work\\R\\WW May 2025\\"
ggsave(file.path(out, "GC_Sankey.png"), plot = p, width = 12, height = 8, dpi = 300)
