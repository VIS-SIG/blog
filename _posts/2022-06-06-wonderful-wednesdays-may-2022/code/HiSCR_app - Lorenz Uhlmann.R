ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      numericInput("ANcount", "Percentage change in AN count:", min = -100, max = 100, value = 50),
      numericInput("incr.absc", "Max. increase in abscesses:", min = -10, max = 10, value = 0),
      numericInput("incr.dr", "Max. increase in drain. fistulae:", min = -10, max = 10, value = 0),
      width = 2
    ),
    
    mainPanel(
      h4("HiSCR odds ratio [95% CI] between groups with p-value and HiSCR response rates"),
      htmlOutput("OR"),
      plotOutput("rateplot"),
      width = 5
    )
  )
))


server <- function(input, output) {
  # require(ggtext)
  require(ggplot2)
  dat <- read.csv("HiSCR_dat.csv", header = T)
  hiscr.h <- function(TABSCS.base, TINFNC.base, TDRFSC.base,
                      TABSCS.fu, TINFNC.fu, TDRFSC.fu,
                      AN.decr = 50, incr.absc = 0, incr.dr = 0) {
    AN.base <- TABSCS.base + TINFNC.base
    AN.fu <- TABSCS.fu + TINFNC.fu
    hiscr <- (AN.fu <= (100 - AN.decr)/100 * AN.base & TABSCS.fu <= (TABSCS.base + incr.absc) &
                TDRFSC.fu <= (TDRFSC.base + incr.dr))
    return(hiscr)
  }
  f.r <- function(x, k = 1) format(round(x, k), nsmall = k)
  sum.yes <- function(x) length(which(x == "Yes")) / length(x) * 100
  dat.prep <- reactive({
    dat$HiSCR <- ifelse(hiscr.h(dat$abscesses.base, dat$infl.nod.base, dat$drain.fist.base,
                                dat$abscesses.w16, dat$infl.nod.w16, dat$drain.fist.w16,
                                input$ANcount, input$incr.absc, input$incr.dr), "Yes", "No")
    dat
  })
  output$rateplot <- renderPlot(height = 100, width = 500, {
    dat <- dat.prep()
    res <- aggregate(dat$HiSCR, list(dat$TRT), sum.yes)
    # res <- res[c(which(res$Group.1 == "PBO"), which(res$Group.1 == "ACT")), ]
    res$val.t <- paste0(f.r(res$x), "%")
    res$Group.1 <- factor(res$Group.1, levels = c("PBO", "ACT"))
    hjust.r <- ifelse(min(res$x) < 11, -0.1, 1.1)
    col.r <- ifelse(min(res$x) < 11, "black", "white")
    # barplot(table(HiSCR, dat$TRT), beside = T)
    ggplot(res, aes(y=x, x=Group.1, width = 0.9)) + 
      geom_bar(stat="identity", fill = "#3182BD") +
      ylab("") + xlab ("") +
      ylim(-12, 100) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_blank(),
            axis.ticks = element_blank(),# axis.text = element_text(size = 12),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            # plot.subtitle = element_text(size = 15, color = "grey40", hjust = 0.32),
            # plot.caption = element_text(color = "grey60", size = 12, hjust = 0.85),
            # plot.title = element_markdown(color = "grey40", size = 20,
            #                               face = "bold", hjust = -0.3),
            plot.margin = margin(0.2, 0.2, -0.3, -0.2, "in")) +
      annotate("text", x=1, y=-8, label= "Placebo", size = 5, color = "grey40") +
      annotate("text", x=2, y=-7, label= "Active", size = 5, color = "grey40") +
      # annotate("text", x=3, y=-4, label= "Week 52", size = 4.25, color = "grey40") +
      # annotate("text", x=2.27, y=-12,
      #          label= "Good glycemic control is defined as Glucose values within a range of 72 and 140 mg/dL.",
      #          size = 3.5, color = "grey60") +
      # scale_fill_manual(breaks = c("High dose", "Medium dose", "Low dose", "SOC"),
      # values = c(brewer.pal(n = 5, name = "Blues")[c(5, 4, 2)], "grey90")) +
      geom_text(aes(label=val.t), vjust = 0.5, hjust = hjust.r, size = 5, position = position_dodge(.1),
                col = col.r) +
      coord_flip()# +
      # labs(title = "The <span style = 'color: #3182BD'>medium</span> and
      #  <span style = 'color: #08519C'>high</span> dose result in the best glycemic control",
      #      subtitle = "Bars show the relative frequency of subjects under good glycemic control")
  })
  output$OR <- renderText({
    dat <- dat.prep()
    p.val <- chisq.test(table(dat$HiSCR, dat$TRT))$p.val
    if (p.val < 0.001) {
      p.val <- "<0.001"
    } else {
      p.val <- f.r(p.val, 3)
    }
    dat$HiSCR.num <- as.numeric(as.factor(dat$HiSCR)) - 1
    fit <- glm(HiSCR.num ~ TRT, data = dat, family = binomial)
    or.est <- f.r(exp(coef(summary(fit))[2, 1]), 2)
    ci.raw <- exp(confint.default(fit))
    ci.est <- paste0("[", f.r(ci.raw[2, 1], 2), "; ", f.r(ci.raw[2, 2], 2), "]")
    res <- HTML(paste("<b>", "OR (active vs. placebo): ", or.est, ci.est, p.val, "</b>"))
    res
  })
}


shinyApp(ui = ui, server = server)