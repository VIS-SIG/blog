library(tidyverse)
library(ggstream)
library(ggtext)

out <- "."

df <- read_csv(WW_GCdose.csv) 

daily <- df %>%
  mutate(
    dl = case_when(
      AVAL2 <= 0.2 ~ "4",
      AVAL2 <= 0.5 ~ "3",
      AVAL2 <= 1.0 ~ "2",
      AVAL2 > 1.0 ~ "1"
    )
  ) %>%
  count(ASTDY, dl) %>%
  complete(ASTDY=1:56, dl = c("1", "2", "3", "4"), fill = list(n=0)) %>%
  select(ASTDY, dl, n)

colours <- c("#fef0d9", "#fdcc8a", "#fc8d59", "#d7301f")
daily$dl <- as.factor(daily$dl)

p0 <- ggplot(daily, aes(x = ASTDY, y = n, fill = dl)) +
   scale_fill_manual(values = rev(colours),
                    labels = rev(c("<= 0.2", "> 0.2 and <= 0.5", "> 0.5 and <=1.0", "> 1.0")),
                    name = "GC Dose (mg/kg/day)") +
  scale_x_continuous("Study Day",
                     limits = c(0, 56),
                   expand=c(0,0)) +
  labs(title="<b>Use of <span style='color:#d7301f'>High Dose GC</span> Reduces Steadily Over Time, With Stable Dose Achieved by Week 8</span></b>") +

  theme_minimal(base_size = 16) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_markdown(colour = "black",
                                  size = 16)
  )

p1 <- p0 +  geom_stream(type = "proportional") +
  scale_y_continuous(name="Proportion", expand=c(0,0)) 
ggsave(file.path(out, "GC_Streamgraph_prop.png"), plot = p1, width = 12, height = 8, dpi = 300)




