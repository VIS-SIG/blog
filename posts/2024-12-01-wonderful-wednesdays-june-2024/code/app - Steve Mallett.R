library(shiny)
library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

max_c <- c("All", "3", "5", "7")
cir <- c("All", "0.01", "0.03")
seq <- c("All", "0.065", "0.9", "1")
ds <- c("No", "Yes")
vp <- c("Beeswarm", "Violin Plot")
pe <- c("Power", "Error Rate")
# tes <- c("All", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14")

cohort_text <- "all: At interim/final analysis, all SoC and backbone monotherapy data available from all cohorts; 
cohort: No sharing occurs; 
concurrent: At interim/final analysis, all SoC and backbone monotherapy data that was collected during the active enrollment time of the cohort under investigation are used; 
dynamic: Whenever in any cohort an interim or final analysis is performed, the degree of data sharing of SoC and backbone monotherapy data from
other cohorts increases with the homogeneity of the observed response rate of the respective arms"
my_lgrey <-"#f0f0f0" 
my_dgrey <- "#636363"

ui <- fluidPage(
  titlePanel("Decision rules for identifying combination therapies in
open-entry, randomized controlled platform trials"),
fluidRow(
  column(2,
         radioButtons("pe", "Display Power or Error Rate?", pe),
         radioButtons("ds", "Display By Data Sharing Category?", ds),
         radioButtons("vp", "Chart Type", vp),
         radioButtons("mc", "Maximum Number of Cohorts", max_c),
         radioButtons("cir", "Cohort Inclusion Rate", cir),
         radioButtons("seq", "Short Term Endpoint Quality", seq)


         ),
 column(5,
        plotOutput("distPlot", width = "1200px", height = "1000px"))
))

server <- function(input, output) {

  
  data1 <- read_csv("./data/ExampleDataNASH.csv") %>%  
    mutate(PTT1ER = log10(PTT1ER)) %>%
    mutate(FWER = log10(FWER))
    # arrange(FinalCohortSampleSize) %>%
    # select(FinalCohortSampleSize, PTP, Disj_Power, TypeofDataSharing) %>%
    # filter(!is.na(PTP))
 
    output$distPlot <- renderPlot({
 
      temp <- data1
      
      if(input$mc == "All") {}
      else {
        temp <- temp %>% 
          filter(Maximumnumberofcohorts == as.numeric(input$mc))
      }
      if(input$cir == "All") {}
      else {
        temp <- temp %>% 
          filter(CohortInclusionRate == as.numeric(input$cir))
      }
      if(input$seq == "All") {}
      else {
        temp <- temp %>% 
          filter(ShortTermEndpointQuality == as.numeric(input$seq))
      }

      if(input$pe == "Power") {
        ptp <- temp$PTP
        disj <- temp$Disj_Power
        t <- c(ptp, disj)
        t2 <- rep("Per-Cohort-Power", length(ptp))
        t3 <- rep("Disjunctive Power", length(disj))
      }
      else {
        ptt1er <- temp$PTT1ER
        fwer <- temp$FWER
        t <- c(ptt1er, fwer)
        t2 <- rep("Per-Cohort-Type 1 Error (log)", length(ptt1er))
        t3 <- rep("FWER (log)", length(fwer))
      }
      
      ss <- temp$FinalCohortSampleSize  
      ds <- paste0("Data Sharing: ", temp$TypeofDataSharing)
      
      ss2 <- rep(ss, 2)
      ds2 <- rep(ds,2)
      

      t4 <- c(t2, t3)
      new <- cbind(ss2, ds2, t, t4) %>%
        as.data.frame() %>%
        mutate(tn = as.numeric(t))
      
      my_plot <- ggplot(new, aes(x=ss2, y=tn, color=factor(t4))) +
        scale_x_discrete("Cohort Sample Size") +
        scale_y_continuous(" ") +
        scale_color_manual(" ", 
                           values = c("#1b9e77", "#d95f02")) +
        theme(text=element_text(
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
          axis.title=element_text(
            colour = my_dgrey,
            size =20),
          plot.caption = element_text(hjust = 0, face= "italic")
        )          
      
      if(input$ds == "Yes") {
        
        my_plot <- my_plot +
          facet_wrap(~ds2)

      }
      else {}
      
      if(input$vp == "Violin Plot") {
        my_plot <- my_plot +
          geom_violin()
      }
      else {
        my_plot <- my_plot +
          geom_beeswarm(cex=0.5, method="hex") 
        }
      my_plot
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
