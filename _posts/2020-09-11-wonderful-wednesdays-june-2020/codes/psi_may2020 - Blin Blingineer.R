library(ggplot2)
if (!require('treemapify')) install.packages('treemapify');
if (!require('dplyr')) install.packages('dplyr');
if (!require('RColorBrewer')) install.packages('RColorBrewer');
library(treemapify)
library(dplyr)
library(RColorBrewer)

# read data
data <- read.csv(url('https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2020/2020-05-13/Satisfaction_wW2005.csv'), na.strings = c(".", ""), header = TRUE)

foo_summarize<- function(dat = data, my_var = NULL, group_label = "NA"){
    
   suppressWarnings(
     ans<- dat %>%
        filter(!is.na(satisfaction)) %>%
        group_by({{my_var}}) %>%
        summarize(n = n(), 
                  mean_satisfaction = mean(satisfaction)) %>%
        mutate(percent = n / sum(n)*100) %>%
        mutate(group = group_label) %>%
        rename(Variable = names(.)[1])
   )
  
   return(ans)
}

#age
d_age<- foo_summarize(dat=data, my_var=age, group_label = "Age")
d_bmi<- foo_summarize(dat=data, my_var=bmi, group_label = "BMI")
d_w_hours<- foo_summarize(dat=data, my_var=w_hours, group_label = "Working hours")
d_todoctor<- foo_summarize(dat=data, my_var=todoctor, group_label = "Doctor visits per year")
d_income<- foo_summarize(dat=data, my_var=income, group_label = "Net income")
d_smoker<- foo_summarize(dat=data, my_var=smoker, group_label = "Smoker")
d_gender<- foo_summarize(dat=data, my_var=gender, group_label = "Gender")
d_employed<- foo_summarize(dat=data, my_var=employed, group_label = "Employment status")
d_graduat<- foo_summarize(dat=data, my_var=graduat, group_label = "Graduation")
d_graduat_f<- foo_summarize(dat=data, my_var=graduat_f, group_label = "Graduation of father")
d_graduat_m<- foo_summarize(dat=data, my_var=graduat_m, group_label = "Graduation of mother")
d_high_grad<- foo_summarize(dat=data, my_var=high_grad, group_label = "Highest educational grade")
d_high_grad_f<- foo_summarize(dat=data, my_var=high_grad_f, group_label = "Highest educational grade (father)")
d_high_grad_m<- foo_summarize(dat=data, my_var=high_grad_m, group_label = "Highest educational grade (mother)")

d_all<- do.call(rbind, list(d_age, d_bmi, d_w_hours, d_todoctor, d_income, d_smoker, d_gender, d_employed, d_graduat,
                            d_graduat_f, d_graduat_m, d_high_grad, d_high_grad_f, d_high_grad_m))

# replace NA with a category: Missing. Only explanatory vars are affected. Response (satisfaction) has no missings
d_all$Variable<- as.character(d_all$Variable)
d_all$Variable[which(is.na(d_all$Variable))]<- "Missing"

# create descrete response variable to display colors in the plot better
d_all$satisfation_categories<- cut(d_all$mean_satisfaction, 
                                   breaks=seq(round(min(d_all$mean_satisfaction)), round(max(d_all$mean_satisfaction)), 0.5), 
                                   labels=c("(6,6.5]", "(6.5,7]", "(7,7.5]", "(7.5,8]", "(8,8-5]", "(8.5-9]"), right = TRUE)

# create response with label
d_all$satisfaction_lab<- paste(d_all$Variable, " (", round(d_all$mean_satisfaction, 1), ")", sep = "")

# colors for plot
col_palette<- display.brewer.pal(n = 6, name = 'RdYlBu')
palette<- brewer.pal(n = 6, name = 'RdYlBu')



# plot
ggplot(d_all, aes(area=percent, label = Variable, fill = satisfation_categories, subgroup = group)) + 
            geom_treemap() + 
            geom_treemap_subgroup_border(size = 1.5, color = "black") +
            geom_treemap_text(grow = T, reflow = T, place = "topleft", layout = "scol", min.size = 1,
                              color = "Darkblue", size = 10) +
            scale_fill_manual(values = palette) + 
            facet_wrap( ~ group) + 
            theme(legend.position = "bottom", 
                  legend.text = element_text(color="black", size = 18), 
                  legend.title = element_text(size = 18),
                  plot.title = element_text(size = 24),
                  plot.subtitle = element_text(size = 20), 
                  plot.caption = element_text(size = 14),
                  strip.text = element_text(size = 14, face = "bold", color = "Darkblue")) +
            labs(title = 'How would you rate your satisfaction with your life overall on a scale from 0-10? (10=completely satisfied)',
                 subtitle = 'Descriptive analysis by subgroup: tiles are proportional to rel. frequencies of the choices within a subgroup',
                 caption ="Created with ggplot2-treemapify",
                 fill = "Mean Satisfaction")


ggsave("treeplot_may2020.png", width = 24, height = 12, units="in", dpi = 300)

