## PSI_April_KM_v1_0

## load in packages
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinycssloaders)
library(broom)
library(survival)
library(survminer)
library(plotly)


tick <- icon("check")

Orange <- "#EF7F04"
Green <- "#68B937"
Blue <- "#00A6CB"
Grey <- "#4E5053"
Darkblue <- "#003569"
Yellow <- "#FFBB2D"

ADTTE <- read_csv('2020-04-08-psi-vissig-adtte_v2.csv')

ADTTE$EVNTDESC <- as.factor(ADTTE$EVNTDESC)
ADTTE$STR01L <- as.factor(ADTTE$STR01L)
ADTTE$STR02L <- as.factor(ADTTE$STR02L)
ADTTE$TRT01P <- as.factor(ADTTE$TRT01P)
ADTTE$AGE <- as.integer(ADTTE$AGE)

ADTTE2 <- ADTTE
# Define UI for app that draws a bar chart in ggplot ----

# Use a fluid Bootstrap layout
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Interactive Kaplan-Meier Plot"),

  # Generate a row with a sidebar
  sidebarLayout(

    # Define the sidebar with inputs for each plot separately
    sidebarPanel(
      ## Conditional panels- only see first set of filters on first tab
      conditionalPanel(condition = "input.tabset == 'withn'",
                       strong("Filters for Plot:"),
                       prettyCheckboxGroup("Strat1", "Hormone receptor status:", 
                                           choices=levels(ADTTE$STR01L), 
                                           selected = levels(ADTTE$STR01L),
                                           inline = TRUE, icon = tick),
                       prettyCheckboxGroup("Strat2", "Prior radiotherapy at randomisation:",
                                           choices= levels(ADTTE$STR02L),
                                           selected = levels(ADTTE$STR02L),
                                           inline = TRUE, icon = tick),
                       prettyCheckboxGroup("Trt", "Treatment Arm:",
                                           choices= levels(ADTTE$TRT01P),
                                           selected = levels(ADTTE$TRT01P),
                                           inline = TRUE, icon = tick),
                       sliderInput("ptAGE", "Age Range:",
                                   min = min(ADTTE$AGE), max = max(ADTTE$AGE),
                                   value = c("min","max"))),
      
      ## Conditional panels- only see second set of filters on second tab
      conditionalPanel(
        condition = "input.tabset == 'QuantComp'",
        strong("Filters for Plot A:"),
        prettyCheckboxGroup("Strat1a", "Hormone receptor status:", 
                            choices=levels(ADTTE$STR01L), 
                            selected = levels(ADTTE$STR01L),
                            inline = TRUE, icon = tick),
        prettyCheckboxGroup("Strat2a", "Prior radiotherapy at randomisation:",
                            choices= levels(ADTTE$STR02L),
                            selected = levels(ADTTE$STR02L),
                            inline = TRUE, icon = tick),
        prettyCheckboxGroup("Trta", "Treatment Arm:",
                            choices= levels(ADTTE$TRT01P),
                            selected = levels(ADTTE$TRT01P),
                            inline = TRUE, icon = tick),
        sliderInput("ptAGEa", "Age Range:",
                    min = min(ADTTE$AGE), max = max(ADTTE$AGE),
                    value = c("min","max")),
        br(),
        hr(),
        br(),
        
        strong("Filters for Plot B:"),
        prettyCheckboxGroup("Strat1b", "Hormone receptor status:", 
                            choices=levels(ADTTE$STR01L), 
                            selected = levels(ADTTE2$STR01L),
                            inline = TRUE, icon = tick),
        prettyCheckboxGroup("Strat2b", "Prior radiotherapy at randomisation:",
                            choices= levels(ADTTE$STR02L),
                            selected = levels(ADTTE$STR02L),
                            inline = TRUE, icon = tick),
        prettyCheckboxGroup("Trtb", "Treatment Arm:",
                            choices= levels(ADTTE$TRT01P),
                            selected = levels(ADTTE$TRT01P),
                            inline = TRUE, icon = tick),
        sliderInput("ptAGEb", "Age Range:",
                    min = min(ADTTE$AGE), max = max(ADTTE$AGE),
                    value = c("min","max")),
        
      ),
      "Events are defined as death or disease progression."
    ),
    
    
    
    
    
    # Create a spot for the plot
    mainPanel(
      ## Creates two separate panels- one for KM plot and number of patients, one for two KM plots to allow comparisons
      tabsetPanel(type = "tabs",
                  id = "tabset",
                  tabPanel("KM plot with number of patients", plotOutput("PlotA"), br(), hr(), br(), plotOutput("PlotB"), value = "withn"),
                  
                  
                  tabPanel("KM plots to compare between subgroups", plotOutput("PlotC"), br(), hr(), br(), plotOutput("PlotD"), value = "QuantComp")
                  
                  
      )
      
    )  
  )
  
)  


# Define a server for the Shiny app


server <- function(input, output) {
  
  # Create four separate plots based on inputs
  
  ## Fit model and plot KM curve for first tab
  output$PlotA <- renderPlot({
    
    #Filter and Subset Data
    
    plotdata <- ADTTE %>% filter(STR01L %in% input$Strat1, STR02L %in% input$Strat2,
                                 TRT01P %in% input$Trt)
    plotdata2 <- subset(plotdata,
                        AGE  >= input$ptAGE[1] & AGE <= input$ptAGE[2])  
    

    
    fita <- survfit(Surv(AVAL, CNSR == 0) ~ TRT01P, data = plotdata2)
    plot <-        ggsurvplot(fita,
                              risk.table = TRUE,
                              data = plotdata2,
                              # size = 0.0001,
                              censor.shape = "",
                              palette = c("TRT01P=tablemab + vismab 52 weeks" = Darkblue,"TRT01P=tablemab x 12 week -> vismab 34 weeks" = Green, 
                                          "TRT01P=tablemab x 52 weeks" = Blue, "TRT01P=vismab x 52 weeks" = Orange),
                              # risk.table = 'nrisk_cumcensor',
                              # tables.theme = theme_cleantable(),
                              # risk.table.col = "strata",
                              # # cumevents = TRUE,
                              title = "Interactive Kaplan-Meier plot: Explore time to event across 4 treatment arms",
                              xlab = "Time (days)",
                              ylab = "Event free survival",
                              legend = c(.18,.25),
                              legend.title = "Treatment Group",
                              
                              # legend.labs = c("Control", "Treatment"),
                              break.x.by = 90,
                              xlim = c(0, 1980),
                              ggtheme = theme_classic()) 
    plot$plot + ggplot2::geom_vline(xintercept=365, linetype='dotted', col = "black") +
      ggplot2::geom_vline(xintercept=1095, linetype='dashed', col = "black") +
      ggplot2::geom_vline(xintercept=1825, linetype='solid', col = "black") +
      ggplot2::annotate(geom = "text", x = 375, y = 0, label = "Year 1", hjust = "left", size = 4.5) +
      ggplot2::annotate(geom = "text", x = 1105, y = 0, label = "Year 3", hjust = "left", size = 4.5) +
      ggplot2::annotate(geom = "text", x = 1835, y = 0, label = "Year 5", hjust = "left", size = 4.5) 
    
  })
  
  ## Filter data and pull out number of patients for n's at bottom of first tab
  output$PlotB <- renderPlot({
    plotdata <- ADTTE %>% filter(STR01L %in% input$Strat1, STR02L %in% input$Strat2,
                                 TRT01P %in% input$Trt)
    plotdata2 <- subset(plotdata,
                        AGE  >= input$ptAGE[1] & AGE <= input$ptAGE[2])  
    
    
    
    fita <- survfit(Surv(AVAL, CNSR == 0) ~ TRT01P, data = plotdata2)
    plot2 <-        ggsurvplot(fita,
                               data = plotdata2,
                               # size = 0.0001,
                               censor.shape = "",
                               palette = c("TRT01P=tablemab + vismab 52 weeks" = Darkblue,"TRT01P=tablemab x 12 week -> vismab 34 weeks" = Green, 
                                           "TRT01P=tablemab x 52 weeks" = Blue, "TRT01P=vismab x 52 weeks" = Orange),
                               # risk.table = 'nrisk_cumcensor',
                               # tables.theme = theme_cleantable(),
                               # risk.table.col = "strata",
                               # # cumevents = TRUE,
                               #title = "Interactive Kaplan-Meier plot: Explore time to event across 4 treatment arms",
                               title = "Interactive Kaplan-Meier plot: Explore time to event across 4 treatment arms",
                               xlab = "Time (days)",
                               ylab = "Event free survival",
                               legend = c(.18,.25),
                               legend.title = "Treatment Group",
                               break.x.by = 180,
                               xlim = c(0, 1980),
                               ggtheme = theme_classic(),
                               risk.table = TRUE, 
                               risk.table.col = "strata",  
                               risk.table.title="Patients remaining in study"
    ) 
    plot2$table + theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
    
  })  
## Filter data and create KM plot for top of second tab
  output$PlotC <- renderPlot({
    
    #Filter and Subset Data
    
    plotdatc <- ADTTE %>% filter(STR01L %in% input$Strat1a, STR02L %in% input$Strat2a,
                                 TRT01P %in% input$Trta)
    plotdata2c <- subset(plotdatc,
                         AGE  >= input$ptAGEa[1] & AGE <= input$ptAGEa[2])  
    
    
    
    fitc <- survfit(Surv(AVAL, CNSR == 0) ~ TRT01P, data = plotdata2c)
    plotb <-        ggsurvplot(fitc,
                               risk.table = TRUE,
                               data = plotdata2c,
                               # size = 0.0001,
                               censor.shape = "",
                               censor = FALSE,
                               palette = c("TRT01P=tablemab + vismab 52 weeks" = Darkblue,"TRT01P=tablemab x 12 week -> vismab 34 weeks" = Green, 
                                           "TRT01P=tablemab x 52 weeks" = Blue, "TRT01P=vismab x 52 weeks" = Orange),
                               # risk.table = 'nrisk_cumcensor',
                               # tables.theme = theme_cleantable(),
                               # risk.table.col = "strata",
                               # # cumevents = TRUE,
                               title = "Plot A:",
                               xlab = "Time (days)",
                               ylab = "Event free survival",
                               legend = c(.18,.25),
                               legend.title = "Treatment Group",
                               
                               # legend.labs = c("Control", "Treatment"),
                               break.x.by = 90,
                               xlim = c(0, 1980),
                               ggtheme = theme_classic()) 
    plotb$plot + ggplot2::geom_vline(xintercept=365, linetype='dotted', col = "black") +
      ggplot2::geom_vline(xintercept=1095, linetype='dashed', col = "black") +
      ggplot2::geom_vline(xintercept=1825, linetype='solid', col = "black") +
      ggplot2::annotate(geom = "text", x = 375, y = 0, label = "Year 1", hjust = "left", size = 4.5) +
      ggplot2::annotate(geom = "text", x = 1105, y = 0, label = "Year 3", hjust = "left", size = 4.5) +
      ggplot2::annotate(geom = "text", x = 1835, y = 0, label = "Year 5", hjust = "left", size = 4.5)
    
  })
  
  
## Filter data and create KM plot for bottom of second tab  
  output$PlotD <- renderPlot({
    
    #Filter and Subset Data
    
    plotdatad <- ADTTE %>% filter(STR01L %in% input$Strat1b, STR02L %in% input$Strat2b,
                                  TRT01P %in% input$Trtb)
    plotdata2d <- subset(plotdatad,
                         AGE  >= input$ptAGEb[1] & AGE <= input$ptAGEb[2])  
    
    
    
    fitd <- survfit(Surv(AVAL, CNSR == 0) ~ TRT01P, data = plotdata2d)
    plotd <-        ggsurvplot(fitd,
                               risk.table = TRUE,
                               data = plotdata2d,
                               censor = FALSE,
                               # size = 0.0001,
                               censor.shape = FALSE,
                               palette = c("TRT01P=tablemab + vismab 52 weeks" = Darkblue,"TRT01P=tablemab x 12 week -> vismab 34 weeks" = Green, 
                                           "TRT01P=tablemab x 52 weeks" = Blue, "TRT01P=vismab x 52 weeks" = Orange),
                               # risk.table = 'nrisk_cumcensor',
                               # tables.theme = theme_cleantable(),
                               # risk.table.col = "strata",
                               # # cumevents = TRUE,
                               title = "Plot B:",
                               xlab = "Time (days)",
                               ylab = "Event free survival",
                               legend = c(.18,.25),
                               legend.title = "Treatment Group",
                               # legend.labs = c("Control", "Treatment"),
                               break.x.by = 90,
                               xlim = c(0, 1980),
                               ggtheme = theme_classic()) 
    plotd$plot +  ggplot2::geom_vline(xintercept=365, linetype='dotted', col = "black") +
      ggplot2::geom_vline(xintercept=1095, linetype='dashed', col = "black") +
      ggplot2::geom_vline(xintercept=1825, linetype='solid', col = "black") +
      ggplot2::annotate(geom = "text", x = 375, y = 0, label = "Year 1", hjust = "left", size = 4.5) +
      ggplot2::annotate(geom = "text", x = 1105, y = 0, label = "Year 3", hjust = "left", size = 4.5) +
      ggplot2::annotate(geom = "text", x = 1835, y = 0, label = "Year 5", hjust = "left", size = 4.5)
    
    
    
    
  })
}    





# Create Shiny app ----
shinyApp(ui = ui, server = server)
