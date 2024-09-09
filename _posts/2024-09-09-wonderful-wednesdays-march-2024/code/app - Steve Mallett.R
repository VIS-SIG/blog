library(shiny)
library(readr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(introdataviz)

# Define UI for application that draws a histogram
num_gen <- 15
abun5 <- read_csv("./Data/abun5.csv")

genus_list <- abun5 %>% 
  group_by(genus) %>% 
  summarise(mn = mean(Value)) %>% 
  arrange(desc(mn)) %>% 
  select(genus) %>% 
  slice_head(n=num_gen) 

genus_list_v <- collect(select(genus_list, genus))[[1]]

ui <- fluidPage(
  
    titlePanel("Multi-Omic Microbiome Study:Pregnancy Initiative (MOMS-PI)"),

    sidebarLayout(
        sidebarPanel(
            radioButtons("genus_select", "Select Genus", genus_list_v),
            radioButtons("visit_select", "Select Visit", c("All", "1", "2", "3", "4" ,"5")),
            sliderInput("age_select", "Select Age Range", value = c(20, 50), step = 5, min = 20, max = 50, width=400),
            radioButtons("income_select", "Select Annual Income", c("All", "<US$20,000", "US$20,000-59,999", "US$60,000+")),
            width=3),

        mainPanel(
          shinycssloaders::withSpinner(
            plotOutput("myplot", width = "1200px", height = "800px")
          )
        )
    )
)

server <- function(input, output) {


  rain_height <- .5
  
    output$myplot <- renderPlot({

      if(input$visit_select == "All") {}
      else {
        abun5 <- abun5 %>% 
          filter(visit_number == input$visit_select)
      }
      
      if(input$income_select == "All") {}
      else {
        abun5 <- abun5 %>% 
          filter(subject_annual_income == input$income_select)
      }   
      
     abun5 <- abun5 %>% 
        filter(genus == input$genus_select) %>% 
        filter(subject_age > input$age_select[1], subject_age < input$age_select[2]) %>% 
        mutate(log_val = log(Value))
     
 # browser()
      ggplot(data=abun5, aes(x=" ", y=log_val)) +
        geom_flat_violin(aes(fill=group),
                         trim=FALSE, alpha = 0.3,
                         position = position_nudge(x = rain_height+.05)) +
        geom_point(aes(colour = group), shape = 1, size = 3, alpha = .9, show.legend = FALSE,
                   position = position_jitter(width = rain_height, height = 0)) +
        scale_y_continuous("Log(Abundance)") +
        # scale_color_brewer(palette = "Set1") +
        # scale_fill_brewer(palette = "Set1") +
        coord_flip() +
        theme(
          panel.background=element_blank(),
          axis.title.y=element_blank(),
          axis.text.x=element_text(size = 15),
          axis.line.x=element_line(color="grey"),
          axis.title.x=element_text(size = 18),
          legend.text=element_text(size = 18),
          legend.title=element_blank()
        )
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
