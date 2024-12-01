library(shiny)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)
library(introdataviz)
library(ggforce)


max_c <- c("All", "3", "5", "7")
cir <- c("All", "0.01", "0.03")
seq <- c("All", "0.065", "0.9", "1")
yn <- c("No", "Yes")
varlist1 <- c("Avg_Pat", "Avg_Pat_First_Suc", "Avg_Perc_Pat_Sup_Plac", "Avg_Cohorts", "FDR", "PTP", "PTT1ER","FWER", "FWER_BA",  "Disj_Power", "Disj_Power_BA")
varlist2 <- c("ShortTermEndpointQuality", "Maximumnumberofcohorts", "TypeofDataSharing", "CohortInclusionRate", "FinalCohortSampleSize", "InterimFutilityStopping", "TreatmentEfficacySetting")
varlist3 <- c("None", "ShortTermEndpointQuality", "Maximumnumberofcohorts", "TypeofDataSharing", "CohortInclusionRate", "FinalCohortSampleSize", "InterimFutilityStopping", "TreatmentEfficacySetting")
varlist4 <- c("None", "CohortInclusionRate", "InterimFutilityStopping")

my_lgrey <-"#f0f0f0" 
my_dgrey <- "#636363"
my_green <- "#639CA4FF"
my_orange <- "#BE7245FF"
fill_cols <- c(my_green, my_orange)

my_theme <- theme(text=element_text(
  colour = my_dgrey,
  size = 20),
  panel.background=element_rect(fill="white"),
  panel.border=element_rect(fill=NA),
  panel.grid=element_line(colour = my_lgrey,
                          linewidth = 0.5,
                          linetype = 1),
  axis.line=element_line(colour = my_dgrey,
                         linewidth = 0.5,
                         linetype = 1),
  axis.text=element_text(
    colour = my_dgrey,
    size = 20),
  axis.title=element_blank(),
  plot.caption = element_text(hjust = 0, face= "italic"),
  legend.title = element_blank()
) 

ui <- fluidPage(
  titlePanel("Platform Trial Design Tool"),
fluidRow(
  column(2,
         selectInput("yvar", "Y-Axis Variable", varlist1, selected="Disj_Power"),
         selectInput("xvar", "X-Axis Variable", varlist2, selected="FinalCohortSampleSize"),
         selectInput("trelvar", "Trellis Variable", varlist3),
         selectInput("groupvar", "Group Variable", varlist4),
         radioButtons("lg", "Log Transform?", yn),
         ),
 column(5,
        plotOutput("distPlot", width = "1200px", height = "1000px"))
))

server <- function(input, output) {

  
  mydata <- read_csv("./data/ExampleDataNASH.csv") 

    output$distPlot <- renderPlot({
  
      mydata <- mydata %>%
        mutate(
          yv = get(input$yvar),
          xv = factor(get(input$xvar)))
      
      if(input$trelvar != "None") {
        mydata <- mydata %>%
          mutate(
            rv = factor(get(input$trelvar)))
      }

      if(input$groupvar != "None") {
        mydata <- mydata %>%
          mutate(
            gv = factor(get(input$groupvar)))
      }      

     if(input$groupvar == "None") {
        my_plot <- ggplot() +
          geom_violin(data=mydata, aes(x=xv, y=yv, group = xv),
                      fill=my_green, alpha=0.6, trim=FALSE)
      }      
      else{
          my_plot <- ggplot(mydata, aes(x=xv, y=yv, 
                                        group = interaction(gv, xv),
                                        fill = gv)) +
            geom_split_violin(alpha=0.6, trim=FALSE) +
            scale_fill_manual(values=fill_cols)
          
      }
      if(input$trelvar != "None") {
        
        my_plot <- my_plot +
          facet_wrap(~rv)

      }

       if(input$lg == "Yes") {
        
        my_plot <- my_plot +
          scale_y_log10()
        
      }
      else {}
      my_plot <- my_plot + my_theme
      my_plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
