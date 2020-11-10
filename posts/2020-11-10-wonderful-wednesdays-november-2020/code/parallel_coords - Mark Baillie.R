## function to scale data
scale_this <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


library(tidyverse)

## read in data
final_in <- read_csv("mediation_data.csv")

## check data 
final_in %>% glimpse()

## scale data and put in long format
data <- 
  final_in %>% 
  select(!c("itch_LOCF", "BSA_LOCF", "redness_LOCF", "DLQI_LOCF")) %>%
  mutate(id = row_number(),
         itch = scale_this(itch),
         BSA = scale_this(BSA),
         redness = scale_this(redness),
         DLQI = scale_this(DLQI)) %>%
  pivot_longer(!c(TRT, id), names_to = "var", values_to = "val") 

### add an indicator for LOCF variables 
missing <- 
  final_in %>% 
  select(c("TRT", "itch_LOCF", "BSA_LOCF", "redness_LOCF", "DLQI_LOCF")) %>%
  mutate(id = row_number()) %>%
  pivot_longer(!c(TRT, id), names_to = "var", values_to = "LOCF") %>%
  mutate(var = str_remove(var, "_LOCF"))

## join to main data set
data <- 
  data %>%
  left_join(missing)

## check data set
data %>% glimpse()

## check LOCF vals
table(data$LOCF)


## plot data 
data %>%
  mutate(
    name = fct_relevel(var,
                       "DLQI", "itch", "redness",
                       "BSA"),
    TRT = fct_relevel(TRT, "Rx", "placebo")
  ) %>%
  ggplot(aes(
    x = name,
    y = val,
    group = id,
    colour = LOCF
  )) +
  geom_hline(yintercept = 0, colour = "black", alpha = 0.4, size = 1.1) +
  geom_point(alpha = 0.7, size = 0.5) +
  geom_line(alpha = 0.25, size = 0.5) +
  labs(title = "Relationship between outcome, treatment and LOCF imputation",
       subtitle = "Measurements are scaled (lower is a better) by outcome",
       caption = "\n The solid black line at zero represents the mean outcome (irrespective of treatment).\nA larger number of patients reported a better BSA and redness profile in the Rx arm.\nThe missing data pattern differs across groups, requiring further investigation.") +
  xlab("") +
  ylab("") +
  facet_wrap( ~ TRT, ncol = 1) +
  theme_minimal() +
  theme(legend.position = "bottom")


## Save plot
page_width <- 200
page_height <- 150
d_dpi <- 300
ggsave(file = paste0("parallel_coords.png"), 
       width = page_width, height = page_height, 
       units = "mm", dpi = d_dpi)