library(shiny)
library(tidyverse)

trts <- c("Experimental", "Standard of Care")

ui <- fluidPage(

    titlePanel("Old Faithful Geyser Data"),

    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("trt", "Select Treatment", trts)
        ),

        mainPanel(
           plotOutput("myPlot")
        )
    )
)

server <- function(input, output) {

  df <- read.csv2("./data/ww eortc qlq-c30 missing.csv", sep=",") %>%
    pivot_longer(cols=starts_with("WEEK"), names_to = "AVISIT", values_to = "AVAL") %>%
    mutate(AVAL=as.numeric(AVAL)) %>%
    select(USUBJID, ARM, LASTVIS, AGE:AVAL) %>%
    # group_by(ARM, LASTVIS, AVISIT) %>%
    summarize(AVAL = mean(AVAL, na.rm=TRUE)) 
    # mutate(LASTVISC=as.factor(paste("Week", sprintf("%02.f", LASTVIS))),
    #        AVISITN = as.numeric(gsub("WEEK","",AVISIT))) %>%
    # mutate(LASTVISC=fct_reorder(LASTVISC, LASTVIS))
    # 
    output$myPlot <- renderPlot({

      df2 <- df %>%
      filter(ARM == input$trt)     
      
  f <- input$trt
  f
  
      ggplot(data=df2, aes(x=AVISITN, y=AVAL, group=LASTVISC, color=ARM)) +
        geom_line() +
        geom_point() +
          scale_color_discrete(type=c("#1b9e77", "#d95f02")) +
        theme(
          # plot.background = element_rect(fill="white"),
          #     panel.background = element_rect(fill="white"),
          #     legend.background = element_rect(fill="black"),
          #     legend.box.background = element_rect(fill="black"),
          #     legend.key = element_blank(),
              legend.text = element_text(colour="grey"),
              panel.grid = element_line(colour="lightgrey"),
              panel.grid.minor = element_blank(),
              strip.background = element_blank(),
              plot.title=element_text(colour = "grey", size = 14, face = "bold"),
              strip.text = element_text(colour = "grey50", size = 10),
              axis.text = element_text(angle = 90))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
