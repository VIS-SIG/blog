

# Data viz PSI

#install.packages("readxl")
library("readxl")
library(lubridate)
library(dplyr)
library(ggplot2)
library(rayshader)
library(viridis)
library(gganimate)
library(scales)

setwd("C:/Users/lsh1703883/Documents/Data visualisation PSI")

viz_data <- read_excel("mobile app data.xlsx")

View(viz_data)

summary(viz_data)

str(viz_data)

viz_data$time <- lubridate::with_tz(viz_data$time, "Europe/London")
# hour
viz_data$hour <- hour(viz_data$time)
table(viz_data$hour)
# day 
viz_data$day <- wday(viz_data$date, label=TRUE)
table(viz_data$day)


# day of the week

viz_data <- viz_data %>% 
  group_by(AppID) %>% mutate(session=1:n()) 


viz_data <- viz_data %>% 
  group_by(AppID, date) %>% count(day,hour) 


viz_data <- na.omit(viz_data)

View(viz_data)


p <- ggplot(
  viz_data, 
  aes(x = as.factor(day), y=as.numeric(hour), size= 8)) + 
  geom_point(aes(fill = as.factor(AppID), colour=as.factor(AppID)))  + 
  xlab("Day of Activity in the App")+
  ylab("Time of Activity in the App ")+  
  labs(color = "Scenario")+ 
  scale_color_discrete()+
  theme_minimal()+
  guides(size = TRUE)+
  guides(alpha = FALSE)+
  guides(linetype = FALSE)+
  theme(legend.position = "none")+
  theme(axis.text = element_text(size = 12),
        title = element_text(size = 12,face="bold"),
        panel.border= element_rect(size=2,color="black",fill=NA))+
  scale_y_continuous(trans = "reverse",breaks=seq(3,21,3))
  
 
p

library(transformr)
library(av)

p <- p + transition_states(AppID, transition_length = 15, state_length = 15) + ggtitle(label = "Day and Time of Activity in the App", subtitle= 'Now showing App ID {closest_state}')

 
p <- p + theme(plot.subtitle=element_text(size=18, hjust=0, face="italic", color="dark blue")) + theme(plot.title=element_text(size=18, hjust=0.5, colour="black", vjust=-1))



final_animation<-animate(p,100,fps = 20,duration = 30, width = 950, height = 750, renderer = av_renderer())


anim_save("./data_viz.mp4",animation=final_animation)


