# Barchart:

setwd("C:/Users/uhlmalo1/OneDrive - Novartis Pharma AG/My_Data/Miscellaneous/PSI_VIS_SIG/Wonderful_Wednesdays_Webinars/2023-05-10")

plot.fun <- function(dat, name, v.just = 1.5, gci.s = "<=3", y.max = 100,
                     title.text = "The <span style = 'color: #08519C'>Active</span> and
         <span style = 'color: #BDD7E7'>Comparator</span> result in similar clinical global impression",
                     col.ann = c(rep(c("black", "black", "white"), 2)),
                     title.h.just = 0.6) {
  require(ggplot2)
  require(ggtext)
  ggplot(dat, aes(y=Value, x=VISITNUM, fill=Treatment)) + 
    geom_bar(position="dodge", stat="identity") +
    ylab("") + xlab ("") +
    ylim(-12, y.max) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.ticks = element_blank(),# axis.text = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.subtitle = element_text(size = 15, color = "grey40", hjust = 0.14),
          plot.caption = element_text(color = "grey60", size = 12, hjust = 0.85),
          plot.title = element_markdown(color = "grey40", size = 20,
                                        face = "bold", hjust = title.h.just),
          plot.margin = margin(0.3, 0.2, -0.38, -0.2, "in")) +
    annotate("text", x=1, y=-4, label= "Week 12", size = 4.25, color = "grey40") +
    annotate("text", x=2, y=-4, label= "Week 52", size = 4.25, color = "grey40") +
    # annotate("text", x=3, y=-4, label= "Week 24", size = 4.25, color = "grey40") +
    # annotate("text", x=2.27, y=-12,
    #          label= "Good glycemic control is defined as Glucose values within a range of 72 and 140 mg/dL.",
    #          size = 3.5, color = "grey60") +
    scale_fill_manual(breaks = c("Active", "Comparator", "Placebo"),
                      values = c(brewer.pal(n = 5, name = "Blues")[c(5, 2)], "grey90")) +
    geom_text(aes(label=val.t), vjust = v.just, size = 4, position = position_dodge(.9),
              col = col.ann) +
    labs(title = title.text,
         subtitle = paste0("Bars show the relative frequency of subjects with CGI-S", gci.s))#,
  # caption = "Good glycemic control is defined as Glucose values within a range of 72 and 140 mg/dL.")
  ggsave(name, width = 12, height = 6, units = "in", dpi = 150)
}

dat <- read.csv("CGI_S_3_groups_csv.csv")
dat$X1 <- (dat$X1 + dat$X2 + dat$X3) / dat$Total.sample.size * 100
# dat$X1 <- (dat$X1) / dat$Total.sample.size * 100
dat <- dat[, 1:3]
names(dat) <- c("VISITNUM", "Treatment", "Value")
dat$val.t <- paste(format(round(dat$Value, 1), nsmall = 1), "%")
dat$VISITNUM <- as.factor(dat$VISITNUM)
plot.fun(dat, "barplot_3.png")

dat <- read.csv("CGI_S_3_groups_csv.csv")
dat$X1 <- (dat$X1 + dat$X2) / dat$Total.sample.size * 100
# dat$X1 <- (dat$X1) / dat$Total.sample.size * 100
dat <- dat[, 1:3]
names(dat) <- c("VISITNUM", "Treatment", "Value")
dat$val.t <- paste(format(round(dat$Value, 1), nsmall = 1), "%")
dat$VISITNUM <- as.factor(dat$VISITNUM)
plot.fun(dat, "barplot_2.png", gci.s = "<=2", y.max = 70,
         title.text = "The <span style = 'color: #08519C'>Active</span> results
         in better clinical global impression than the
         <span style = 'color: #BDD7E7'>Comparator</span>",
         title.h.just = 1.25)

dat <- read.csv("CGI_S_3_groups_csv.csv")
dat$X1 <- (dat$X1) / dat$Total.sample.size * 100
# dat$X1 <- (dat$X1) / dat$Total.sample.size * 100
dat <- dat[, 1:3]
names(dat) <- c("VISITNUM", "Treatment", "Value")
dat$val.t <- paste(format(round(dat$Value, 1), nsmall = 1), "%")
dat$VISITNUM <- as.factor(dat$VISITNUM)
plot.fun(dat, "barplot_1.png", v.just = -0.5, gci.s = "=1", y.max = 50,
         title.text = "The <span style = 'color: #08519C'>Active</span> results
         in better clinical global impression than the
         <span style = 'color: #BDD7E7'>Comparator</span>",
         col.ann = c(rep(c("black", "black", "black"), 2)),
         title.h.just = 1.25)
