
#----------------------------------------------------
# PSI Wonderful Wednesdays Challenge February 2023
#----------------------------------------------------

# Clear environment and read in libraries
rm(list = ls())
library(tidyverse)
library(scales)
library(ggtext)

# Simulate 4 studies with different treatments/sample sizes
subjects <- as.character(seq(1, 850))

trts <- c(sample(c('Placebo', 'A', 'B'),
                 size = 300, replace = T),
          sample(c('Placebo', 'C'),
                 size = 100, replace = T),
          sample(c('B', 'C'),
                 size = 300, replace = T),
          sample(c('B', 'D'),
                 size = 150, replace = T))

studies <- c(rep('Study 1', 300),
             rep('Study 2', 100),
             rep('Study 3', 300),
             rep('Study 4', 150))

visits <- c(rep(seq(0, 100, length.out = 10), each = 300),
            rep(seq(0, 75,  length.out  = 8), each = 100),
            rep(seq(0, 100, length.out = 10), each = 300),
            rep(seq(0, 50,  length.out = 6),  each = 150))

# Random study effects
study_effs <- c(rep(rnorm(1, sd = 3), 300),
                rep(rnorm(1, sd = 3), 100),
                rep(rnorm(1, sd = 3), 300),
                rep(rnorm(1, sd = 3), 150))

# Random subject effects
sub_effs <- rnorm(850, 0, 3)

# Repeat values for each visit e.g. in study 1, each subject needs 10 records
split <- function(vec){
  return(
    c(rep(vec[1:300], 10),
      rep(vec[301:400], 8),
      rep(vec[401:700], 10),
      rep(vec[701:850], 6)))
}

# Create study dataframe
df <- data.frame(subject   = split(subjects),
                 study     = split(studies),
                 trt       = split(trts),
                 study_eff = split(study_effs),
                 sub_eff   = split(sub_effs),
                 time      = as.character(round(visits, 0)))

# Find each subject/treatment combo - will need to merge this on later
subject_df <- df %>%
              select(subject, trt) %>%
              mutate(trt = if_else(trt != 'Placebo',
                                   paste0('Treatment ', trt),
                                   trt)) %>%
              unique()

# Coefficients for treatment variables
beta0 <- 15
betaA <- 0.2
betaB <- -0.1
betaC <- 0.1
betaD <- 0.05

# Coefficients for interaction variables
gamma <- 0.01
gammaA <- 0.025
gammaB <- -0.1
gammaC <- 0.0001
gammaD <- -0.05


# Find DLQI score for each 
dlqi_calc <- function(n, A, B, C, D, time, study_eff, sub_eff){
  
  dlqi <- beta0 + betaA*A + betaB*B +
          betaC*C + betaD*D + 
          gamma*time + gammaA*A*time + gammaB*B*time +
          gammaC*C*time + gammaD*D*time +
          sub_eff + study_eff
  
  return(rnorm(n, dlqi, 3))
}


dlqi_df <- df %>%
  
           # Create variables needed for dlqi_calc
           pivot_wider(id_cols = c(subject, study, study_eff, sub_eff, time),
                       names_from = trt,
                       values_from = trt,
                       values_fn = length,
                       values_fill = 0) %>%
           mutate(time = as.numeric(time),
                  dlqi = dlqi_calc(n(), A, B, C, D, time, study_eff, sub_eff),
                  
                  # Apply cut-off thresholds
                  dlqi = case_when(
                                   dlqi < 0 ~ 0,
                                   dlqi > 30 ~ 30,
                                   T ~ dlqi)) %>%
           left_join(subject_df, by = 'subject') %>%
           group_by(trt, study, time) %>%
           mutate(mean = mean(dlqi),
                  
                  # Add some jitter so plot points are not over-layed
                  time = case_when(study == 'Study 1' & trt == 'Placebo' ~ time - 2,
                          study == 'Study 1' & trt == 'Treatment B' ~ time + 2,
                          study == 'Study 2' & trt == 'Placebo' ~ time - 1,
                          study == 'Study 2' & trt == 'Treatment C' ~ time + 1,
                          study == 'Study 3' & trt == 'Treatment B' ~ time - 1,
                          study == 'Study 3' & trt == 'Treatment C' ~ time + 1,
                          study == 'Study 4' & trt == 'Treatment D' ~ time + 1,
                          study == 'Study 4' & trt == 'Treatment B' ~ time - 1,
                          T ~ time))
  
# Color the subscript treatment labels
color_label <- function(color, trt){
    paste0("N",
    "<sub><span style = 'color:", color, ";'>", trt, "</span></sub></span>")
}

nP <- color_label(hue_pal()(5)[1], 'P')
nA <- color_label(hue_pal()(5)[2], 'A')
nB <- color_label(hue_pal()(5)[3], 'B')
nC <- color_label(hue_pal()(5)[4], 'C')
nD <- color_label(hue_pal()(5)[5], 'D')

# Create the desired facet labels
study.labs <- c(paste0('Study 1 <br><br>',
                       nP, ' = ', nA, ' = ', nB, ' = 100'),
                paste0('Study 2 <br><br>' ,
                       nP, ' = ', nC, ' = ', '50'),
                paste0('Study 3 <br><br>',
                       nB, ' = ', nC, ' = ', '100'),
                paste0('Study 4 <br><br>',
                       nB, ' = ', nD, ' = ', '75'))

names(study.labs) <- c('Study 1', 'Study 2', 'Study 3', 'Study 4')

# Create the plot
png("plot.png", res = 500, height = 5, width = 7, units = 'in')

ggplot(data = dlqi_df) +
  
  geom_point(aes(x = time,
                 y = dlqi,
                 color = trt), alpha = 0.1, size = 0.3) +
  
  facet_wrap(~study, ncol = 1, strip.position = 'right',
             labeller = labeller(study = study.labs)) +
  
  geom_line(aes(x = time,
                y = mean,
                group = trt,
                color = trt),
            size = 0.5, alpha = 0.75) +
  
  theme(panel.background       = element_rect(fill = 'gray10'),
        panel.border           = element_rect(color = 'gray20', 
                                              size  = unit(1, 'in'), 
                                              fill  = NA),
        plot.margin            = margin(20, 20, 20, 20),
        panel.grid.minor       = element_blank(),
        plot.background        = element_rect(fill  = 'gray10', 
                                              color = 'gray10'),
        panel.grid.major       = element_line(color ='gray13'),

        legend.position        = c(0.43, 1.17),
        legend.justification   = 'top',
        legend.background      = element_rect(fill = 'gray10'),
        legend.title           = element_blank(),
        legend.key             = element_rect(fill = 'gray10'),
        legend.text.align      = 0,
        legend.text            = element_text(color = 'gray90',
                                              size = 8),
        
        strip.background       = element_rect(fill = 'gray20'),
        panel.spacing          = unit(0.15, 'in'),
        strip.text.y           = element_markdown(angle = 0, 
                                                  color = 'white',
                                                  face  = 'bold', 
                                                  size  = 8),

        axis.text             = element_text(color = 'gray70'),
        axis.title.x          = element_text(hjust  = 0, 
                                             color  = 'white',
                                             face   = 'bold',
                                             margin = margin(t = 12.5),
                                             size   = 8),
        plot.title            = element_text(color = 'white', 
                                             hjust = 0,
                                             face = 'bold',
                                             margin = margin(b = 7),
                                             size = 12),
        plot.subtitle         = element_text(color = 'gray90', 
                                             face = 'italic',
                                             size = 7, 
                                            margin = margin(b = 32)),
        plot.title.position   = 'plot',
        plot.caption.position = 'plot',
        plot.caption          = element_text(size = 7, 
                                             color = 'white', 
                                             face = 'bold.italic')) +
  
  
  coord_cartesian(ylim = c(0, 30), clip = 'off') +
  
  guides(color = guide_legend(nrow = 1)) +
  
  labs(x        = 'Relative Day',
       y        = NULL,
       title    = 'Mean DLQI Score over Time for Four Different Studies',
       caption  = 'Huw Wilson',
       subtitle = 'Lower DLQI scores indicate less severe symptoms.')

dev.off()
