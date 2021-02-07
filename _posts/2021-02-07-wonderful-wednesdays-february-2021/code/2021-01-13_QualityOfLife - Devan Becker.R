library(GGally)
library(dplyr)
library(tidyr)
library(patchwork)

qol <- read.csv("Data/2021-01-13-QualityOfLife.csv")

head(qol)
glimpse(qol)

qol2 <- qol %>% 
    mutate_if(function(x){length(unique(x)) < 6}, factor)

ggpairs(qol2[qol2$VISIT == "Week 16", c(2, 3, 15)], 
    mapping = aes(colour = TRT))
ggpairs(qol[qol$VISIT == "Week 16", 5:14])
ggpairs(qol2[qol2$VISIT == "Week 16", 5:14])
ggpairs(qol[qol$VISIT == "Week 16", 5:14], 
    mapping = aes(colour = qol[qol$VISIT == "Week 16",]$TRT))
ggpairs(qol2[qol2$VISIT == "Week 16", 5:14], 
    mapping = aes(colour = qol2[qol2$VISIT == "Week 16",]$TRT))


#### Goal 1: Multidimensional nature of DLQI ####
# As integers, they're all highly correlated
# As categorical variables, need an intelligent way to summarise
# Something like PCA or NMF, but for categories?
# Collection of stacked bar plots?
# Percent agreement between categories?
# Compound Poisson model for discrete categories?
#### Goal 2: Show the effect of treatment, incorporate multidimensionality ####


# Attempt 1: Conditional Distributions

dlq <- select(qol, starts_with("DLQI")) %>% 
    select(-DLQI_SCORE)
heatmaps <- lapply(0:3, function(w){
    m1 <- sapply(1:10, function(x){
        f1 <- filter(dlq, dlq[, x] == w)
        if(nrow(f1) > 0) {
            return(apply(f1, 2, function(y) mean(y == w, na.rm = TRUE)))
        } else {
            return(rep(NA, 10))
        }
    })
    colnames(m1) <- rownames(m1)
    m1 <- m1 %>% as.data.frame() %>% 
        tibble::rownames_to_column(var = "from") %>% 
        pivot_longer(-from, names_to = "to", values_to = "both_0")
    return(m1)
})


numwords <- c("Zero", "One", "Two", "Three")
wrap_plots(lapply(1:4, function(i) {
    zeros <- apply(dlq, 2, function(x) sum(x == i - 1, na.rm = TRUE))
    ggplot(heatmaps[[i]]) +
        aes(x = to, y = from, fill = both_0) + 
        geom_tile() +
        labs(x = paste0("...how often is this measure ", i - 1, "?"), 
            y = paste0("When this measure is ", i - 1, "..."),
            title = paste0(numwords[i], " Together"),
            fill = paste0("Both ", numwords[i])) +
        scale_fill_viridis_c(option = "C") + 
        annotate("text", x = 1:10, y = 1:10, label = zeros) +
        theme(aspect.ratio = 1, 
            axis.text.x = element_text(angle = 90, vjust = 0.5))
}))
    
    
