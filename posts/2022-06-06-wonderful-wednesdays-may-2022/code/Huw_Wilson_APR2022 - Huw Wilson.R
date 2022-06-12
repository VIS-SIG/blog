
# Clear environment, read in libraries and data
rm(list = ls())

library(tidyverse)
library(ggtext)

df <- read.csv("./HiSCR_dat.txt")

# Create required variables for graph
df_new <- df %>%
          mutate(
            
            # If abscesses increase
            abscesses.incr = if_else(
              condition = abscesses.w16 > abscesses.base,
              true = "Yes",
              false = "No",
              missing = NA_character_
            ),
            
            # If draining fistulae increase
            drain.fist.incr = if_else(
              condition = drain.fist.w16 > drain.fist.base,
              true = "Yes",
              false = "No",
              missing = NA_character_
            ),
            
            # If abscesses and draining fistulae increase
            drain.fist.abscesses.incr = if_else(
              condition = abscesses.incr == "No" & drain.fist.incr == "No",
              true = "No",
              false = "Yes",
              missing = NA_character_
            ),
            
            an.count.base = abscesses.base + infl.nod.base,
            an.count.w16 = abscesses.w16 + infl.nod.w16,
            
            # Percentage change in AN count
            an.count.change = (an.count.w16 - an.count.base)/an.count.base*100
          ) %>%
  
         select(c(TRT, abscesses.incr, drain.fist.incr,
                  drain.fist.abscesses.incr, an.count.change))

active.df <- filter(df_new, TRT == "ACT")
placebo.df <- filter(df_new, TRT == "PBO")

results <- data.frame()

# Find the percentage assigned a positive HiSCR for each value of i
# where i is the minimum required percentage reduction.
for (i in 0:100){
  active.abscesses <- sum(active.df$an.count.change <= -i & active.df$abscesses.incr == "No")/nrow(active.df)*100
  active.drain.fist <- sum(active.df$an.count.change <= -i & active.df$drain.fist.incr == "No")/nrow(active.df)*100
  active.both <- sum(active.df$an.count.change <= -i & active.df$drain.fist.abscesses.incr == "No")/nrow(active.df)*100
  
  placebo.abscesses <- sum(placebo.df$an.count.change <= -i & placebo.df$abscesses.incr == "No")/nrow(placebo.df)*100
  placebo.drain.fist <- sum(placebo.df$an.count.change <= -i & placebo.df$drain.fist.incr == "No")/nrow(placebo.df)*100
  placebo.both <- sum(placebo.df$an.count.change <= -i & placebo.df$drain.fist.abscesses.incr == "No")/nrow(placebo.df)*100
  
  # Store results
  results <- rbind(results,
                   data.frame(percent = i,
                               active.abscesses,
                               active.drain.fist,
                               active.both,
                               placebo.abscesses,
                               placebo.drain.fist,
                               placebo.both))
}

# Create a row for each percentage reduction, treatment arm, and condition
# to get the data into the right form for the graph
graph_df <- results %>%
            pivot_longer(cols = -"percent") %>%
  
            mutate(
              # Extract treatment arm
              TRT = if_else(
                condition = str_detect(name, pattern = "active"),
                true = "Active",
                false = "Placebo",
                missing = NA_character_),
              
              # Extract condition (<br> is used to cause line break in subplot title)
              condition = case_when(
                str_detect(name, pattern = "abscesses") ~ "No Increase in Abscesses",
                str_detect(name, pattern = "fist") ~ "No Increase in Draining Fistulae",
                TRUE ~ "No Increase in <br>Abscesses & Draining Fistulae"
              )) %>%
  
            select(-name)

# Order conditions for the graph
graph_df$condition <- factor(graph_df$condition,
                             levels = c("No Increase in Abscesses",
                                        "No Increase in Draining Fistulae",
                                        "No Increase in <br>Abscesses & Draining Fistulae"))

# Annotation dataframe for the Placebo and Active labels
arm.ann <- data.frame(x = c(5, 3),
                      y = c(90, 35),
                      TRT = c("Active", "Placebo"),
                      condition = rep("No Increase in Abscesses", 2))

# Order condition variable to ensure labels appear in correct subplot.
arm.ann$condition <- factor(arm.ann$condition,
                             levels = c("No Increase in Abscesses",
                                        "No Increase in Draining Fistulae",
                                        "No Increase in <br>Abscesses & Draining Fistulae"))

axis.color <- "gray40"

# Create graph
ggplot(data = graph_df,
       aes(x = percent,
           y = value,
           group = TRT)) +

  # Add lines
  geom_line(aes(color = TRT)) +
  
  # Subplot by condition
  facet_wrap(~condition) +
  
  # Restrict plotting area
  coord_cartesian(expand = F) +
  
  # Add % labels to the axes ticks
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  
  scale_y_continuous(limits = c(0, 100),
                     labels = scales::percent_format(scale = 1)) +
  
  # Add Active and Placebo labels
  geom_text(data = arm.ann,
            aes(x = x,
                y = y,
                group = condition,
                label = TRT,
                color = TRT),
            hjust = 0,
            size = 2.75) +

  # Define theme
  theme(panel.background    = element_rect(fill = "white"),
        axis.line           = element_line(color = axis.color),
        panel.grid.major    = element_line(color = "gray95"),
        axis.ticks          = element_line(color = axis.color),
        axis.text           = element_text(color = axis.color),
        axis.title          = element_text(color = axis.color,
                                           size = 8),
        axis.title.x        = element_text(hjust = 0,
                                           margin = margin(t = 15)),
        legend.position     = 'none',
        plot.margin         = margin(20, 20, 20, 20),
        strip.text          = element_markdown(size = 8),
        panel.spacing       = unit(0.4, 'in'),
        plot.title.position = 'plot',
        plot.subtitle       = element_markdown(color = "gray30",
                                               size = 9,
                                               margin = margin(b = 10)),
        plot.title          = element_markdown(size = 11),
        plot.caption        = element_markdown(size = 7,
                                               color = axis.color,
                                               margin = margin(t = 15))) +
  
  # Define plot labels
  labs(x = "Minimum Required Percentage Reduction in AN Count*",
       y = NULL,
       subtitle = " % of Patients assigned a Positive HiSCR Response (N<sub>P</sub> = N<sub>A</sub> = 200)",
       title = "____ <br> <br> How Changing the HiSCR Definition Changes the Results",
       caption = "*AN Count = Sum of the Number of Abscesses and Inflammatory Nodes <br> Author: Huw Wilson")

  





