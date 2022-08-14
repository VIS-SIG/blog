
# Authors: Huw Wilson and Rhys Warham
# Creation Date: 02AUG2022

# Read in libraries
library(tidyverse)
library(shiny)
library(plotly)
library(rsconnect)

# Read in data and calculate cumulative probabilities
RankingData <- read.csv("./bayesian_ranking.csv") %>%
               pivot_longer(cols = -Treatment,
                            names_to = "Rank",
                            values_to = "Probability") %>%
               group_by(Treatment) %>%
               mutate(cumProbability = cumsum(Probability),
                      Rankn = as.numeric(str_extract(Rank, "[0-9]+")),
               SUCRAflag = if_else(
                          condition = Rankn %in% seq_len(n()-1),
                          true = "Y",
                          false = "N")) %>%
               ungroup()

# Calculate SUCRA and overall rank for each treatment
SUCRAvalues <- RankingData %>%
               group_by(Treatment, SUCRAflag) %>%
               mutate(SUCRA = sum(cumProbability)/(n())) %>%
               ungroup() %>%
               filter(SUCRAflag == "Y") %>%
               select(Treatment, SUCRA) %>%
               unique() %>%
               mutate(Order = as.numeric(reorder(Treatment, -SUCRA)))

# Merge SUCRA and overall rank onto the original dataset
df <- left_join(RankingData, SUCRAvalues, by = "Treatment") %>%
      select(-c(Rank, SUCRAflag))


# Define UI
ui <- fluidPage(
    
   h2("Treatment Ranking Dashboard"),
   p("Rhys Warham, Huw Wilson"),
   br(),

   column(5,
    selectInput("treatment",
                "Select a treatment",
                choices = unique(df$Treatment)),
    
    p("Surface Under the Cumulative Ranking (SUCRA) can be used to determine the overall ranking of a treatment. 
      It is calculated as the sum of cumulative rank probabilities up to T-1 divided by T-1. Higher values indicate higher overall ranking."),
    
    plotOutput("SUCRAplot", height = "500px")),
    
    column(width = 7,
           
           tabsetPanel(
               tabPanel("Line Graphs",
                        br(),
                        column(6,
                        selectInput("line_type",
                                    "Type",
                                    choices = c("Probability",
                                                "Cumulative Probability"),
                                    selected = "Cumulative Probability")),
                        column(6,
                               selectInput("facet",
                                           "Facet",
                                           choices = c("Yes", "No"),
                                           selected = "No")),
                        br(), br(), br(),
                        plotlyOutput("line_graph", height = "500px")),
               tabPanel("Bar Chart",
                        br(),
                        selectInput("stack_type",
                                    "Stack by",
                                    choices = c("Treatment", "Rank")),
                        plotOutput("bar_chart", height = "500px"))))

)

# Choose colors for graphs
chosen.trt.color <- "dodgerblue"
other.trt.color <- "gray70"

# Define server logic
server <- function(input, output) {
    
    # Reactive dataset that flags the selected treatment
    graph_df <- reactive({df %>%
                          mutate(ChosenTrt = if_else(
                              condition = Treatment == input$treatment,
                              "Y",
                              
                              "N"))})
    
    # Create the SUCRA plot
    output$SUCRAplot <- renderPlot({
        
        SUCRAdata <- graph_df() %>%
                     select(Treatment, SUCRA, ChosenTrt, Order) %>%
                     mutate(yLabel = paste0(sprintf("%2.0f", Order), ". ", Treatment)) %>%
                     unique()
        
        ggplot(data = SUCRAdata,
               aes(x = SUCRA,
                   y = reorder(yLabel, SUCRA),
                   fill = ChosenTrt)) +
            
            geom_col(width = 0.85) +
            
            scale_x_continuous(limits = c(0, 1)) +
            
            scale_fill_manual(values = c(other.trt.color, chosen.trt.color)) +
            
            theme(legend.position = 'none',
                  axis.title.y = element_blank(),
                  axis.text.y = element_text(hjust = 0),
                  axis.ticks.y = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  axis.line = element_line(color = "black"),
                  axis.title.x = element_text(margin = margin(t = 15),
                                              size = 12),
                  axis.text = element_text(size = 12))
    })

    # Create the line graph
    output$line_graph <- renderPlotly({
        
        if (input$line_type == "Probability"){
            line_p <- geom_line(aes(y = Probability,
                                    text = paste0(Treatment, "<br>",
                                                  "Rank: ", Rankn, "<br>",
                                                  "Probability: ", sprintf("%0.4f", Probability))))}
        
        else if (input$line_type == "Cumulative Probability"){
            line_p <- geom_line(aes(y = cumProbability,
                                    text = paste0(Treatment, "<br>",
                                                  "Rank: ", Rankn, "<br>",
                                                  "Cumulative Probability: ", sprintf("%0.4f", cumProbability))))}
        
        if (input$facet == "Yes"){
            facet_p <- facet_wrap(~reorder(Treatment, -SUCRA))}
        else {
            facet_p <- geom_blank()}
        
        p <- ggplot(data = graph_df(),
                   aes(x = Rankn,
                       group = Treatment,
                       color = ChosenTrt,
                       size = ChosenTrt)) +
             line_p +
             facet_p +
            
             scale_size_manual(values = c(0.8, 1.2)) +
            
             scale_x_continuous(breaks = seq_len(nrow(SUCRAvalues)),
                               limits = c(1, nrow(SUCRAvalues)),
                               expand = c(0, 0)) +
            
             scale_y_continuous(limits = c(0, 1),
                               expand = c(0, 0)) +
            
             scale_color_manual(values = c(other.trt.color, chosen.trt.color)) +
            
             theme(legend.position  = 'none',
                   axis.title.y     = element_blank(),
                   panel.spacing.y  = unit(0.5, 'in'),
                   panel.spacing.x  = unit(0.1, 'in'),
                   panel.background = element_rect(fill = "gray98")) +

             labs(x = "Rank")
        
        ggplotly(p, tooltip = "text") %>%
        config(displayModeBar = F)
            
    })
    
    

    # Create the stacked bar charts
    output$bar_chart <- renderPlot({
        
        if (input$stack_type == "Rank"){
            
            # Stack by Rank (Treatment on y axis)
            ggplot() +
            geom_col(data = graph_df(),
                     aes(x = Probability,
                         y = reorder(Treatment, SUCRA),
                         alpha = reorder(Rankn, -Rankn),
                         fill = ChosenTrt)) +
                
                scale_fill_manual(values = c(other.trt.color, chosen.trt.color)) +
                
                theme(panel.background = element_rect(fill = "white"),
                      legend.position  = 'top',
                      axis.title.y     = element_blank(),
                      axis.ticks.y     = element_blank(),
                      axis.text        = element_text(size = 12),
                      axis.title.x     = element_text(size = 12),
                      axis.text.y      = element_text(margin = margin(r = -30)),
                      legend.text      = element_text(size = 12)) +
                
                guides(fill = 'none',
                       alpha = guide_legend(nrow = 1, reverse = T, title = NULL))
        }
        
        else if (input$stack_type == "Treatment"){
        
            # Stack by Treatment (Rank on y-axis)
            ggplot() +
            geom_col(data = graph_df(),
                     aes(x = Probability,
                         y = reorder(Rankn, -Rankn),
                         alpha = reorder(Treatment, SUCRA),
                         fill = ChosenTrt)) +
            scale_fill_manual(values = c(other.trt.color, chosen.trt.color)) +
            
            theme(panel.background = element_rect(fill = "white"),
                  legend.position  = 'top',
                  axis.title.y     = element_blank(),
                  axis.ticks.y     = element_blank(),
                  axis.text.y      = element_text(margin = margin(r = -30)),
                  legend.text      = element_text(size = 12),
                  axis.text        = element_text(size = 12),
                  axis.title.x     = element_text(size = 12)) +
                
            guides(fill = 'none',
                   alpha = guide_legend(nrow = 1, reverse = T, title = NULL))
        }
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
