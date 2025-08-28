if(!require("pacman")) install.packages("pacman")
p_load("tidyverse", "this.path", "readxl", "gganimate", "gifski")

setwd(this.dir())
dat<-read_xlsx("WWW_APR2025.xlsx", 1) %>%
  mutate(AVALC = reorder(AVALC, AVAL, decreasing = TRUE))

bign <- dat %>%
  filter(AVISITN==1) %>%
  group_by(TRT) %>%
  mutate(N=n()) %>%
  select(TRT, N) %>%
  distinct()

#Proportion of subjects at each level at each time
prop <- dat %>%
  merge(bign) %>%
  group_by(TRT, AVISITN, AVALC) %>%
  mutate(count = n(), prp=count/N, perc = round(100*prp, 2)) %>%
  select(TRT, AVISITN, AVALC, AVAL, count, prp, perc) %>%
  distinct()

#Non-animated fitted line facet plots
line_static<-prop %>%
  filter(AVISITN != 1) %>%
  ggplot(aes(x=AVISITN, y=perc, group=TRT, col=TRT)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", se=FALSE) +
  xlim(0,20) +
  ylim(0,60) +
  xlab("Visit Number") +
  ylab("% of subjects") +
  labs(col="Treatment") +
  facet_wrap(~AVALC) +
  theme(panel.grid.minor = element_blank())

ggsave("line_static.png", line_static, width=1920, height=1080, units="px")

#Animated bar plot
bar<-prop %>%
  filter(AVISITN != 1) %>%
  ggplot(aes(x=reorder(AVALC, AVAL), y=perc, group=TRT, fill=TRT)) +
  geom_bar(stat="identity", position="dodge") +
  ylim(c(0,60)) +
  transition_time(AVISITN) +
  enter_grow() +
  exit_fade() +
  labs(x="Response", 
       y="Percentage of Responsders", 
       title="Visit: {frame_time}",
       fill = "Treatment") +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=30),
        plot.title = element_text(size=40),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        legend.key.size = unit(2, "cm"))

animate(bar, nframes=19, fps=1, width=1920, height=1080, renderer=gifski_renderer())
anim_save("bar.gif")

#Animated moving line plot
line<-prop %>%
  filter(AVISITN != 1) %>%
  ggplot(aes(x=AVISITN, y=prp, col=TRT)) +
  geom_point(size=10) +
  geom_line(linewidth=5) +
  facet_wrap(~AVALC) +
  scale_x_continuous(breaks=c(2,5,10,15,20)) +
  ylim(c(0,0.6)) +
  transition_reveal(AVISITN) +
  ease_aes("sine-in-out") +
  labs(x="Visit", 
       y="Proportion of Patients", 
       title="Visit: {frame_along}",
       col="Treatment") +
  theme(axis.text = element_text(size=20),
        axis.title = element_text(size=30),
        plot.title = element_text(size=40),
        legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        legend.key.size = unit(2, "cm"),
        strip.text = element_text(size=30))

animate(line, nframes=19, fps=1.9, width=1920, height=1080, renderer=gifski_renderer())
anim_save("line.gif")
