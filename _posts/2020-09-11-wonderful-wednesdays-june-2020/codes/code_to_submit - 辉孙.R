##### code for funnel plot for submittion
library(doParallel)
library(data.table)
library(plotly)
library(tidyverse)

data_transform <- function(x, 
                           labels = c("low", "high"), 
                           except = NULL, 
                           threshold = 0.1, 
                           explicit_conv = NULL, 
                           explicit_conv_override = FALSE, 
                           binary_categories = FALSE, 
                           n_binary_categories=1, 
                           verbose = FALSE) {
  
  x_new <- x
  cutoff <- floor(threshold * nrow(x_new)) # if number of unique values is less than 10% of overall dataset size, treat corresponding covariate as categorical
  if(!is.null(explicit_conv) && explicit_conv_override){
    cutoff <- Inf
  }
  for (column in colnames(x_new)[!colnames(x_new) %in% except]) {
    fac <- as.factor(x[, column])
    if ((nlevels(fac) > cutoff) || column %in% explicit_conv) {
      if (!binary_categories) {
        n_quant <- length(labels) # number of quantiles
        quant <- quantile(x_new[, column], probs = 0:n_quant / n_quant, na.rm = TRUE)
        if (verbose) {message(sprintf("Transforming %s to categorical using following quantiles", column))}
        if (verbose) {message(paste0(capture.output(quant), collapse = "\n"))}
        x_new[, column] <- cut(x_new[, column], quant, include.lowest = TRUE, labels = labels)
      } else {
        n_quant <- n_binary_categories+1 # number of quantiles
        quant <- quantile(x_new[, column], probs = 0:n_quant / n_quant, na.rm = TRUE)
        if (verbose) {message(sprintf("Transforming %s to binary categorical columns using following quantiles", column))}
        if (verbose) {message(paste0(capture.output(quant), collapse = "\n"))}
        for (j in 2:(length(quant) - 1)) {
          x_new[, sprintf("%s_q%s", column, substr(names(quant)[j], 1, 2))] <-
            as.factor(cut(x_new[, column], c(quant[1], quant[j], quant[length(quant)]), include.lowest = TRUE, labels = FALSE))
          levels(x_new[, sprintf("%s_q%s", column, substr(names(quant)[j], 1, 2))]) <- c(0, 1)
        }
        # remove orignal column
        x_new[, column] <- NULL
      }
    }
    else if (typeof(x[, column]) != "integer") {
      if (verbose) {message(sprintf("Convert %s to factors", column))}
      x_new[, column] <- as.factor(x[, column])
    }
  }
  
  return(x_new)
}




return_subgroups <- function(x, 
                             eval_fun, 
                             covs, 
                             packages=NULL, 
                             comb = 3, 
                             comb_lowerbound = 1, 
                             n_cores = 1, 
                             first_N = Inf, 
                             verbose = !(first_N==Inf)) {
  
  
  
  data_cov_comb <- function(vec, k, n = length(vec),verbose = FALSE) {
    # returns all n over k combinations of elements in vec
    # n should be size of vec and k<=n
    if(verbose){message(sprintf("Computing %s combinations out of",k))}
    if(verbose){message(paste0(capture.output(vec), collapse = "\n"))}
    return(gtools::combinations(n, k, vec))
  }
  
  
  data_level_comb <- function(x, covs) {
    # takes a dataframe x where every column is of type integer/factor and computes all combinations between all the levels of covariates specified in covs
    return(tidyr::expand(x, x[covs]))
  }
  
  compute_eval <- function(x, covs, eval_fun, first_N_active = FALSE) {
    # computes the evaluation function on all subgroup combination of given covariates in covs (vector of strings)
    # returns a list with number of entrys equal to the number of possible covariate level combinations.
    # Each entry consits of two elements:
    # first entry contains the filtered dataset containing only subgroup datapoints
    # second entry contains single line dataframe output of eval_func augmented by subgroup labels
    comb <- data_level_comb(x, covs)
    
    if (first_N_active) {
      if (first_N_count <= nrow(comb)) {
        comb <- comb[1:first_N_count, ]
        first_N_count <<- 0
      } else {
        first_N_count <<- first_N_count - nrow(comb)
      }
    }
    
    subframes <- foreach(i = 1:nrow(comb)) %do% {
      x_filtered <- x
      for (j in 1:length(covs)) {
        x_filtered <- dplyr::filter(x_filtered, x_filtered[covs[j]] == as.character(unlist(comb[i, j])))
      }
      result <- eval_fun(x_filtered)
      result[covs] <- comb[i, ]
      
      return(result)
    }
    return(subframes)
  }
  
  #check if provided covs are contained in x
  for (name in covs) {
    if (!(name %in% colnames(x))) {
      stop(sprintf("Variable %s was not found in %s", name, toString(substitute(x))))
    }
  }
  
  #turn on verbose if first_N != Inf 
  if (first_N!=Inf){verbose <- TRUE} 
  
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  if(verbose){message(sprintf("Number of cores: %s", getDoParWorkers()))}
  
  # computes the evaluation function on all subgroup combination of given covariates in covs (vector of strings) for 1,2 and 3 combinations(general: comb_lowerbound:comb)
  if (first_N == Inf) {
    results <- foreach(i = comb_lowerbound:comb, .combine = c) %do% {
      covs_comb <- data_cov_comb(covs, i,verbose=verbose)
      track_time <- system.time({
        result <- foreach(j = lapply(as.list(1:dim(covs_comb)[1]), function(x) covs_comb[x[1],]), .combine = c, .packages = c("foreach", packages)) %dopar% {
          compute_eval(x, j, eval_fun)
        }
      })
      if(verbose){message(paste0(capture.output(track_time), collapse = "\n"))}
      return(result)
    }
  } else {
    if(verbose){message(sprintf("Computing first %d subgroups using single core", first_N))}
    first_N_count <<- first_N
    results <- foreach(i = comb_lowerbound:comb, .combine = c) %do% {
      covs_comb <- data_cov_comb(covs, i,verbose=verbose)
      track_time <- system.time({
        result <- foreach(j = 1:min(nrow(covs_comb), first_N), .combine = c, .packages=c(packages)) %do% {
          if (first_N_count == 0) {
            return()
          }
          res <- compute_eval(x, covs_comb[j, ], eval_fun, first_N_active = TRUE)
        }
      })
      if(verbose){message(paste0(capture.output(track_time), collapse = "\n"))}
      return(result)
    }
  }
  
  stopCluster(cl)
  
  return(dplyr::bind_rows(results))
}

############################################################################################
############# read data
dat <- read.csv("./Satisfaction_wW2005.csv") %>%
  as.data.frame() %>% dplyr::select(-X, -ID) 

### global mean and se
dat_mean <- mean(dat$satisfaction)
dat_se <- sd(dat$satisfaction)/sqrt(nrow(dat))
dat_sd <- sd(dat$satisfaction)

fit <- function(x) {
  cf1 <- tryCatch({
    cf <- mean(x$satisfaction)
    se <- sd(x$satisfaction)/sqrt(nrow(x))
    c(cf, se,nrow(x))
  },
  warning = function(w) { #warning handling
    NA
  }, error = function(w) { #error handling
    c(dat_mean,dat_se,0)
  }
  )
  return(data.frame(av = cf1[1], se = cf1[2], N = cf1[3]))
}

subgroups <- return_subgroups(dat, 
                              fit,
                              comb = 3, 
                              comb_lowerbound = 1,
                              covs = colnames(dat)[1:14],
                              # first_N = 100, 
                              n_cores = 4,
                              verbose = TRUE)
# head(subgroups)

nams <- names(subgroups)[-c(1:3)]
subgroups$text <- NULL
subgroups$text <- apply(subgroups, 1, function(x){
  covs <- x[-c(1:3)]
  ind <- which(!is.na(covs))
  p1 <- NULL
  for(i in 1:length(ind)){
    p1 <- c(p1, sprintf("%s=%s", nams[ind[i]], covs[ind[i]]))
  }
  p1 <- paste(p1, collapse=",")
  p2 <- sprintf("|satisfaction=%.1f, N=%d", as.numeric(x[1]),as.numeric(x[3]))
  paste0(p1, p2)
})


## calculate dfs (assuming balance)
n_start <- 2
N <- n_start:nrow(dat)
q95 <- qt(0.975, df= N - 1)
ci <- data.frame(N = N,
                 lb = dat_mean - q95*dat_sd/sqrt(N),
                 ub = dat_mean + q95*dat_sd/sqrt(N))

fig <- subgroups %>% filter(N >= n_start) %>% 
  plot_ly(x = ~ N, y = ~av) %>%
  add_markers(name = 'Subgroup Estimates',text = ~text, hoverinfo = "text",
              color = I("blue"), alpha=0.2)%>%
  add_markers(x = nrow(dat), y = dat_mean,
              name = 'Overall Estimates',text = I(paste0("satisfaction = ",round(dat_mean,2))), 
              hoverinfo = "text",color = I("red"),alpha=0.2)%>%
  add_lines(x = ~ ci$N, y = ~ ci$lb, name = 'Lower 95% PI', hoverinfo = 'skip', 
            color = I("black"), alpha=0.5) %>%
  add_lines(x = ~ ci$N, y = ~ ci$ub, name = 'Upper 95% PI', hoverinfo = 'skip', 
            color = I("black"), alpha=0.5) %>%
  add_lines(x = I(n_start:nrow(dat)), y = dat_mean, 
            name = "Overall mean",hoverinfo = 'skip', color = I("red"),alpha=0.5) %>%
  layout(xaxis = list(range = c(n_start,(nrow(dat)+4)),title = "Subgroup size"),
         yaxis = list(range = c(1, 10),title = "Average satisfication"))
fig
htmlwidgets::saveWidget(fig, "./funnel_satisfaction.html")




