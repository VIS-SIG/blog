library(shiny)
library(ggplot2)
library(plotly)

ref_dat <- dur_dat %>% dplyr::filter(baseline_haz == 0.05) %>%
  rename(ref_HR = HR)

makeplot <- function(df = dur_dat, basehaz){
  
  dur_dat2 <- dur_dat %>% dplyr::filter(baseline_haz == basehaz)
  
  plot<- ggplot(dur_dat2, aes(HR, optim_dur, size = required_events)) +
    geom_point(show.legend = FALSE, alpha = 0.9, colour = "pink") +
    geom_point(data = ref_dat, aes(ref_HR, optim_dur),
               alpha = 0.4, colour = "blue", fill = "white") + 
    guides(size = guide_legend("Required\n failures")) +
    ylab("Total study duration (months)") +
    xlab("HR") +
    geom_vline(xintercept = 0.775, colour = "green") +
    geom_hline(yintercept = 30, colour = "green") + 
    scale_y_continuous(limits = c(25, 55), breaks = seq(25, 55, by = 5)) + 
    theme_light() +
    theme(panel.grid.minor = element_blank(),
          panel.border = element_blank())
  
  return(plot)
  
}

P1 <- makeplot(basehaz = 0.03)
P2 <- makeplot(basehaz = 0.035)
P3 <- makeplot(basehaz = 0.04)
P4 <- makeplot(basehaz = 0.045)
P5 <- makeplot(basehaz = 0.05)
P6 <- makeplot(basehaz = 0.055)
P7 <- makeplot(basehaz = 0.06)
P8 <- makeplot(basehaz = 0.065)
P9 <- makeplot(basehaz = 0.07)


ui <- fluidPage(titlePanel(h4("Change in total study duration (versus reference in blue) by varying of assumed HR and active comparator hazard")),
                titlePanel(h4("Primary Author: Federico Bonofiglio")),
                titlePanel(h5("Modified by: Rhys Warham")),
                titlePanel(h5("The size of the data points represents the number of required failures")),
                selectInput("basehaz", label = h5("Active Comparator Hazard"),
            choices = list("0.030", "0.035", "0.040", "0.045",
                           "0.050 (Reference Hazard)", "0.055", "0.060",
                           "0.065", "0.070")),
            plotlyOutput("barplot"))

server <- function(input, output){
  
  barplottest <- reactive({
    if ( "0.030" %in% input$basehaz) return(P1)
    if ( "0.035" %in% input$basehaz) return(P2)
    if( "0.040" %in% input$basehaz) return(P3) 
    if ( "0.045" %in% input$basehaz) return(P4)
    if ( "0.050 (Reference Hazard)" %in% input$basehaz) return(P5)
    if( "0.055" %in% input$basehaz) return(P6) 
    if ( "0.060" %in% input$basehaz) return(P7)
    if ( "0.065" %in% input$basehaz) return(P8)
    if( "0.070" %in% input$basehaz) return(P9) 
  })
  
  output$barplot <- renderPlotly({   
    dataplots = barplottest()
    print(dataplots)
  }) 
  
}

shinyApp(ui = ui, server = server)

