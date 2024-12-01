library(shiny)
library(tidyverse)
library(ggplot2)
library(introdataviz)
library(stringr)
library(ggforce)

my_lgrey <-"#f0f0f0" 
my_dgrey <- "#636363"
my_green <- "#639CA4FF"
my_orange <- "#BE7245FF"
fill_cols <- c(my_green, my_orange)

my_theme <- theme(text=element_text(
  colour = my_dgrey,
  size = 14),
  panel.background=element_rect(fill="white"),
  panel.border=element_rect(fill=NA),
  panel.grid=element_line(colour = my_lgrey,
                          linewidth = 0.5,
                          linetype = 1),
  axis.line=element_line(colour = my_dgrey,
                         linewidth = 0.5,
                         linetype = 1),
  axis.text=element_text(
    colour = my_dgrey),
  axis.title=element_text(colour = my_dgrey),
  plot.caption = element_text(hjust = 0, face= "italic"),
  legend.title = element_text(colour = my_dgrey,
                              size = 12),
    plot.title = element_text(colour = my_dgrey)
) 
# my_title <- "When the therapy is effective, inclusion of interim futility stopping rules reduces the disjunctive power when the short term endpoint quality is poor.\n"
  my_cap <- "Treatment effect scenario: Both monotherapies are superior to SoC and combination therapy is better than monotherapies."
  
  mydata <- read_csv("./data/ExampleDataNASH.csv") %>%
    mutate(tes = if_else(TreatmentEfficacySetting==6, "Monotherapies > SoC. Combination > monotherapies", "Null Hypothesis")) %>%
    mutate(steq = paste0("Short Term Endpoint Quality: ", ShortTermEndpointQuality)) %>%
    filter(TreatmentEfficacySetting %in% c(1)) %>%
    filter(ShortTermEndpointQuality %in% c(0.65, 1)) %>%
    filter(FinalCohortSampleSize %in% c(100, 300, 500))
    
      my_plot <- ggplot(mydata, aes(x=factor(FinalCohortSampleSize), y=Disj_Power, 
                                    group = interaction(InterimFutilityStopping, factor(FinalCohortSampleSize)),
                                    fill = InterimFutilityStopping)) +
        geom_sina(alpha=0.6, size=3, shape=21) +
        # scale_fill_manual(values=fill_cols) +
        scale_fill_manual("Interim\nFutility\nStopping?\n", values=fill_cols) +
        scale_y_continuous("Disjunctive Power\n") +
        scale_x_discrete("Final Cohort Sample Size\n") +
        facet_wrap(~steq) +
        labs(
          # title = str_wrap(my_title, 200),
             caption = str_wrap(my_cap, 100))  + 
        my_theme

ggsave("./WW_Jul24_03.png",  my_plot, width=1250, height=900, units="px", dpi=150)


