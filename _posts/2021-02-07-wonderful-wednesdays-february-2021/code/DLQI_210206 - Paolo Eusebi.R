library(tidyverse)
library(data.table)
library(grid)
library(cowplot)
library(RCurl)


x <- getURL("https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2021/2021-01-13/ww2020_dlqi.csv")

d <- read.csv(text = x)
d1a <- d %>%
  gather(key = PARAMCD,
         value = AVAL, DLQI101:DLQI_SCORE, factor_key=TRUE) %>%
  filter(!PARAMCD %in% c("DLQIMCID", "DLQIRESP")) %>%
  mutate(VISIT = ifelse(VISIT=="Baseline", "Wk 0", "Wk 16"),
         VISITN = if_else(VISIT=="Wk 0", 0, 1))
d1a

d1b <- d1a %>%
  group_by(TRT, PARAMCD, VISITN, VISIT, AVAL) %>%
  summarise(n = n())%>%
  mutate(freq = n / sum(n))
d1b

d1a$TRT <- relevel(as.factor(d1a$TRT), "B")
d1b$TRT <- relevel(as.factor(d1b$TRT), "B")


tit_col = "grey50"
cap_col = "grey50"

ggplotib <- function(paramcd = NULL,
                     title = NULL, 
                     caption = NULL, 
                     breaks = 0:3,
                     transparency = 0.01){
  d1a_2 <- d1a %>%
    filter(PARAMCD %in% paramcd) %>%
    group_by(TRT, USUBJID, VISITN, VISIT) %>%
    summarise(AVAL_SUB = sum(AVAL))
  
  d1b_2 <- d1a_2 %>%
    group_by(TRT, VISITN, VISIT, AVAL_SUB) %>%
    summarise(n = n())%>%
    mutate(freq = n / sum(n))
  p1 <- ggplot() +
    geom_line(data = d1a_2, aes(x = VISITN, y = AVAL_SUB, group = USUBJID, col=TRT),
              alpha = transparency, size = 2) +
    geom_point(data = d1b_2, aes(x = VISITN, y = AVAL_SUB, size = freq, col=TRT)) +
    facet_grid(cols = vars(TRT)) +
    scale_x_continuous(breaks = c(0, 1), labels = c("Wk 0", "Wk 16"), limits = c(-0.1,1.1)) +
    scale_y_continuous(breaks = breaks) +
    theme_minimal() +
    labs(x = "", y = "", title = title, subtitle = caption) +
    theme(panel.grid = element_blank(),
          title = element_text(size = 12, colour = tit_col),
          plot.subtitle = element_text(size = 10, colour = cap_col, hjust = 0), 
          axis.text.x = element_text(size = 12, colour = "grey50"),
          plot.background = element_rect(fill="black"),
          strip.text = element_blank()) +
    guides(color = F, size = F)
  p1
}


# Unidimensional ----------------------------------------------------------
p1 <- ggplotib(paramcd = "DLQI101",
              title="Item 1",
              caption="Itchy, sore, painful, or stinging skin") +
  theme(axis.text.y = element_text(colour = "grey50"))
p1
p2 <- ggplotib(paramcd = "DLQI102",
              title="Item 2",
              caption="Embarassment") + 
  theme(axis.text.y = element_text(colour = "grey50"))
p2
p3 <- ggplotib(paramcd = "DLQI103",
              title="Item 3",
              caption="Interference with shopping / home / gardening") + 
  theme(axis.text.y = element_text(colour = "grey50"))
p3
p4 <- ggplotib(paramcd = "DLQI104",
              title = "Item 4",
              caption = "Influence on clothing") + 
  theme(axis.text.y = element_text(colour = "grey50"))
p4
p5 <- ggplotib(paramcd = "DLQI105",
              title = "Item 5",
              caption = "Social or leisure activities affected") + 
  theme(axis.text.y = element_text(colour = "grey50"))
p5
p6 <- ggplotib(paramcd = "DLQI106",
              title = "Item 6",
              caption = "Difficult to do any sport?") +
  theme(axis.text.y = element_text(colour = "grey50"))
p6
p7 <- ggplotib(paramcd = "DLQI107",
              title = "Item 7",
              caption = "Prevented you from working / studying?") + 
  theme(axis.text.y = element_text(colour = "grey50"))
p7
p8 <- ggplotib(paramcd = "DLQI108",
               title = "Item 8",
               caption = "Problems with partner / close friends / relatives") + 
  theme(axis.text.y = element_text(colour = "grey50"))
p8
p9 <- ggplotib(paramcd = "DLQI109",
               title = "Item 9",
               caption = "Sexual difficulties") + 
  theme(axis.text.y = element_text(colour = "grey50"))
p9
p10 <- ggplotib(paramcd = "DLQI110",
                title = "Item 10",
                caption = "Problem from treatment") + 
  theme(axis.text.y = element_text(colour = "grey50"))
p10

pt <- ggplotib(paramcd = "DLQI_SCORE", 
               breaks = 0:30, 
               transparency = 0.02,
               title = "DLQI",
               caption = "Total Score") +
  theme(axis.text.y = element_text(colour = "grey50"))
pt

# t_b
tfs <- 24
x <- 0.05
t1 <- textGrob(expression(bold("Active treatment") * phantom(bold(" vs. Placebo"))),
               x = x, y = 0.7, gp = gpar(col = "#F8766D", fontsize = tfs), just = "left")
t2 <- textGrob(expression(phantom(bold("Active treatment vs.")) * bold(" Placebo")),
               x = x, y = 0.7, gp = gpar(col = "#00BFC4", fontsize = tfs), just = "left")
t3 <- textGrob(expression(phantom(bold("Active treatment ")) * bold("vs.") * phantom(bold(" Placebo"))),
               x = x, y = 0.7, gp = gpar(col = "grey", fontsize = tfs), just = "left")
t4 <- textGrob(expression("Strips describe the flow of the patients from different categories between visits"),
               x = x, y = 0.4, gp = gpar(col = "grey", fontsize = 10), just = "left")
t5 <- textGrob(expression("Circles are proportional to the percentage of patients at every visit"),
               x = x, y = 0.25, gp = gpar(col = "grey", fontsize = 10), just = "left")

tb <- ggplot(data = d) +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill="black"))  +
  coord_cartesian(clip = "off") +
  annotation_custom(grobTree(t1, t2, t3, t4, t5)) +
  theme(legend.position = 'none')
tb

b <- plot_grid(tb, p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
             nrow = 11, rel_heights = c(0.5, rep(1, 10)))
ggsave(plot=b, filename="b.png", path=("~") , width = 6, height = 32, device = "png")   

# Multidimensional --------------------------------------------------------
p12 <- ggplotib(paramcd = c("DLQI101", "DLQI102"),
                breaks = 0:6,
                transparency = 0.015,
                title="Item 1 + Item 2",
                caption="Symptoms and feelings") +
  theme(axis.text.y = element_text(colour = "grey50"))
p12

p34 <- ggplotib(paramcd = c("DLQI103", "DLQI104"),
                breaks = 0:6,
                transparency = 0.015,
                title="Item 3 + Item 4",
                caption="Daily activities") +
  theme(axis.text.y = element_text(colour = "grey50"))
p34

p56 <- ggplotib(paramcd = c("DLQI105", "DLQI106"),
                breaks = 0:6,
                transparency = 0.015,
                title="Item 5 + Item 6",
                caption="Leisures") +
  theme(axis.text.y = element_text(colour = "grey50"))
p56

p89 <- ggplotib(paramcd = c("DLQI108", "DLQI109"),
                breaks = 0:6,
                transparency = 0.015,
                title="Item 8 + Item 9",
                caption="Interpersonal relationships") +
  theme(axis.text.y = element_text(colour = "grey50"))
p89

tfs <- 42
x <- 0.0175
t1 <- textGrob(expression(bold("Active treatment") * phantom(bold(" vs. Placebo"))),
               x = x, y = 0.7, gp = gpar(col = "#F8766D", fontsize = tfs), just = "left")
t2 <- textGrob(expression(phantom(bold("Active treatment vs.")) * bold(" Placebo")),
               x = x, y = 0.7, gp = gpar(col = "#00BFC4", fontsize = tfs), just = "left")
t3 <- textGrob(expression(phantom(bold("Active treatment ")) * bold("vs.") * phantom(bold(" Placebo"))),
               x = x, y = 0.7, gp = gpar(col = "grey", fontsize = tfs), just = "left")
t4 <- textGrob(expression("Strips describe the flow of the patients from different categories between visits"),
               x = x, y = 0.4, gp = gpar(col = "grey", fontsize = 10), just = "left")
t5 <- textGrob(expression("Circles are proportional to the percentage of patients at every visit"),
               x = x, y = 0.25, gp = gpar(col = "grey", fontsize = 10), just = "left")

t <- ggplot(data = d) +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill="black"))  +
  coord_cartesian(clip = "off") +
  annotation_custom(grobTree(t1, t2, t3, t4, t5)) +
  theme(legend.position = 'none')
t

t2 <- ggplot(data = d) +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill="black"))  +
  theme(legend.position = 'none')
t2
col1 <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
               nrow = 10, rel_heights = c(rep(1, 10)))
col2 <- plot_grid(p12, p34, p56, p7, p89, p10,
                nrow = 6, rel_heights = c(2, 2, 2, 1, 2, 1))
col3 <- plot_grid(pt, t2,
                nrow = 2, rel_heights = c(4, 6))
cols <- plot_grid(col1, col2, col3, ncol = 3, rel_widths = c(3, 3, 3))
o <- plot_grid(t, cols, nrow = 2, rel_heights = c(0.5, 10))
ggsave(plot = o, filename="o.png", path=("~") , width = 18, height = 32, device = "png")   

