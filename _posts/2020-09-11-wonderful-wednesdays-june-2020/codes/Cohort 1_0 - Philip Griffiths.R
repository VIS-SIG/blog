library(tidyverse)
library(Stack)
library(viridis)
library(IOHanalyzer)
library(car)
library(RColorBrewer)
display.brewer.all(colorblindFriendly = TRUE)
library(cowplot)
library(dgof)

#Colour Scheme
Orange <- "#EF7F04"
Green <- "#68B937"
Blue <- "#00A6CB"
Grey <- "#4E5053"
Darkblue <- "#003569"
Yellow <- "#FFBB2D"

#Load in data and make satisfaction and all other variables a factor. Also make a numeric version of satisfation for use late
Cohort <- read_csv('Cohort.csv') %>%
  mutate(satisfaction = factor(satisfaction, level=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10))) %>%
  mutate_all(factor) %>%
  mutate(NumSat = as.numeric(satisfaction)) %>%
  mutate(NumSat = NumSat-1) #To put it back on a 0-10 scale

#Calculate means for the groups
means <- Cohort %>%
  group_by(smoker) %>% 
  summarise(
    NumSat = mean(NumSat, na.rm = TRUE) 
  ) %>%
  filter(!is.na(smoker)) #filter out the means for patients without a value for smoking (some implicit NA's)

#Significance test of mean differences
model <- lm(NumSat ~ smoker, data = Cohort)
summary(model)

#test of distributions
SmokerSat <- Cohort %>%
  select(smoker, NumSat) %>%
  filter(smoker=="Y")

NonSmokerSat <- Cohort %>%
  select(smoker, NumSat) %>%
  filter(smoker=="N")

KS <- ks.test(SmokerSat$NumSat, NonSmokerSat$NumSat)


#Make plot of means, 
p <- ggplot(means, aes(x=smoker, y=NumSat)) +
  geom_bar(stat="identity", fill=Darkblue, color="black", xpd = FALSE) +
  scale_y_continuous(name = "Satisfaction (0-10)") +
  scale_x_discrete(name = "Smoking History", 
                   labels=c("Non-Smoker","Current Smoker"))


#Fake plot for  aesthetic purporposes (remove the bar going below "6" on the chart with the reduced y axis)
   #Start with fake dataset
smoker <- c("N", "Y")
NumSat <-c(6,6)
fake.data <- data.frame(smoker,NumSat)


# make plot 1 where the scale is changed to emphasize difference
plot1 <- p+coord_cartesian(ylim=c(6,8.1))+
  theme_classic() +
  theme(line = element_blank()) +
  geom_bar(data=fake.data,stat="identity", fill="white", color="white") +  #Add a white rectangle below the "6.0" line using fake data
  ggtitle("A statistically significant difference in Satisfaction between Non-smokers and Smokers \n highlighted by using misleading axis... \n") +
  theme (plot.title = element_text(family = "sans", color=Grey, face="bold", size=14, hjust=0.5)) + #Edit font
  annotate("segment", x = "N", xend = "Y", y = 8.0, yend = 8.0,
           colour = "black") +
  annotate("segment", x = "N", xend = "N", y = 8.0, yend = 7.9,
           colour = "black") +
  annotate("segment", x = "Y", xend = "Y", y = 8.0, yend = 7.9,
           colour = "black") + 
  annotate("segment", x = 1.5, xend = 1.58, y = 8.0, yend = 8.1,
           colour = "black") +
  annotate(geom = "text", x = 1.6, y = 8.05, label = "Significant!", hjust = "left", vjust = "bottom", size = 6) +
  annotate(geom = "text", x = 2.0, y = 8.05, label = "(p<0.001)", hjust = "left", vjust = "bottom", size = 2) 
  

# make plot 2 which uses oringinal 0-10 scale
plot2 <- p+coord_cartesian(ylim=c(0,10)) +
  theme_classic() +
  theme(line = element_blank()) + 
  ggtitle(".. But when plotted using the 0-10 scale, the true (small) magnitude of \n this difference is apparent \n ") +
  theme (plot.title = element_text(family = "sans", color=Grey, face="bold", size=14, hjust=0.5)) + #Edit font
  annotate("segment", x = "N", xend = "Y", y = 9.6, yend = 9.6,
           colour = "black") +
  annotate("segment", x = "N", xend = "N", y = 9.6, yend = 9.1,
           colour = "black") +
  annotate("segment", x = "Y", xend = "Y", y = 9.6, yend = 9.1,
           colour = "black") + 
  annotate("segment", x = 1.5, xend = 1.33, y = 9.6, yend = 10,
           colour = "black") +
  annotate(geom = "text", x = 1.3, y = 9.8, label = "Meaningful?", hjust = "right", vjust = "bottom", size = 6) 



# make plot 3 - a staked bar chart to show distributional changes
Cohort$smoker %>% mutate(fct_reorder(smoker, NumSat, .fun="mean"))


plot3 <- ggplot(data=subset(Cohort, !is.na(smoker)), aes(y = reorder(smoker, NumSat))) +
  geom_bar(aes(fill = satisfaction), position = position_fill(reverse = TRUE)) +
  scale_fill_brewer(palette="BrBG") +
  guides(fill = guide_legend(nrow = 1)) +
  ggtitle("Significant difference between the two response distributions \n (D=0.14, p<.001)") +
  theme_classic() +
  theme(line = element_blank()) +
  theme(legend.position = "bottom", legend.box = "horizontal")+
  xlab("Proportion of participants selecting each 'Satisfaction' value") + 
  scale_y_discrete(name = "Smoking History", 
                   labels=c("Current Smoker","Non-Smoker")) +
  scale_x_continuous(breaks = c(0, .25, .50, .75, 1.00))
  ggtitle("The response distribution shows that smokers reported lower satisfaction scores (0-5) more often than non-smokers \n and higher satisfaction scores (9-10) less often than non-smokers") +
  theme (plot.title = element_text(family = "sans", color=Grey, face="bold", size=18, hjust=0.5))  #Edit font
  
#plot3 <- plot3 +  annotate(geom = "curve", x = 0.50, y = 2.60, xend = 0.65, yend = 2.45, 
#           curvature = -.25, arrow = arrow(length = unit(3, "mm"))
#  ) +
#  annotate(geom = "text", x = 0.5, y = 2.55, label = "Significant difference in distributions \n (D=0.14, p<.001)", hjust = "center", size = 3)

#design layout
  # Make a title
title <- ggdraw() + 
  draw_label(
    "How Meaningful is a Mean Difference?",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 22
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

top_row <- plot_grid(plot1, plot2, labels = c("A", "B"), align = "h")
Faceted_graphs <- plot_grid(top_row, plot3, labels = c(' ', 'C'), label_size = 12, ncol = 1)

plot_grid(title, Faceted_graphs,  
          ncol = 1,
          # rel_heights values control vertical title margins
          rel_heights = c(0.1, 1)
  )


