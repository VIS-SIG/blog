library(haven)
library(ggplot2)
library(ggtext)

data <- read_sas("/.../data.sas7bdat")

ggplot(data=data) +
  geom_line(data=data, aes(x=Rank, y=Prob, group=Treatment), color="#cccccc") +
  geom_rect(data=data[1,], xmin=0, ymin=-0.05, xmax=1.5, ymax=1, fill = alpha("#eeeeee", .5)) +
  geom_rect(data=data[1,], xmin=2.5, ymin=-0.05, xmax=3.5, ymax=1, fill = alpha("#eeeeee", .5)) +
  geom_rect(data=data[1,], xmin=4.5, ymin=-0.05, xmax=5.5, ymax=1, fill = alpha("#eeeeee", .5)) +
  geom_rect(data=data[1,], xmin=6.5, ymin=-0.05, xmax=7.5, ymax=1, fill = alpha("#eeeeee", .5)) +
  geom_rect(data=data[1,], xmin=8.5, ymin=-0.05, xmax=9.5, ymax=1, fill = alpha("#eeeeee", .5)) +
  geom_rect(data=data[1,], xmin=10.5, ymin=-0.05, xmax=11.5, ymax=1, fill = alpha("#eeeeee", .5)) +
  geom_text(aes(x=Rank, y=Prob, label=Treatment, color=factor(flag)), show.legend=FALSE, size=7) +
  scale_x_continuous(limits=c(1, 12), breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  scale_y_continuous("Probability", limits=c(0, 1), breaks=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)) +
  scale_color_manual(" ", values=c("#a50f15", "#252525", "#d9d9d9")) +
  theme_minimal() +
  theme(text = element_text(size=20),
        plot.title = element_text(size = 22),
        plot.subtitle = element_text(size = 20),
        legend.title=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.line.x=element_line(size=0.25)) +
labs (title ="<b style='color:#a50f15'>Mirtazapine</b> is most likely to be the highest ranked treatment",
      subtitle="Other most likely high-ranked treatments are <b style='color:#a50f15'>Escitalopram</b>, <b style='color:#a50f15'>Venlafaxine</b>,  <b style='color:#a50f15'>Sertraline</b> and  <b style='color:#a50f15'>Citalopram</b>")+
theme(plot.title = element_markdown(),
      plot.subtitle = element_markdown())

