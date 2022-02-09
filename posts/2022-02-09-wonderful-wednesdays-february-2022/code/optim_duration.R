#' Numerical optimization of total study duration
#' @description Optimizes total study duration for a fixed accrual time. Hence, it returns
#'     optimal follow-up time after the last patient is enrolled for a fixed sample
#'     size. Thus, the function can be used to investigate follow-up variation by varying of
#'     baseline hazard, HR, or other quantities used for sample size calculation according
#'     to the exponential model given by Lachin et al (1986) (see package gsDesign).

library(gsDesign)
library(optimx)
library(numDeriv)
library(pso)
library(dplyr)
library(ggplot2)
library(gganimate)
library(transformr)
library(gifski)

# reproducing the sample size calculation (exponential model) I obtain a larger N (+200 patients ca)
h_c <- 0.05/12
HR <- 0.775
res <- nSurvival(h_c, h_c*HR, 30, 24, alpha = 0.05, sided = 2)
res
res$n
res$nEvents

# objective function
to_minimize <- function(tot_dur, accr_time = 24, h_c = 0.05/12, HR = 0.775,
                        given_N = 10157)
{

  if (tot_dur <= accr_time)  # set a constraint
    out <- 1e+200
  else
  {
    res <- nSurvival(lambda1 =h_c, lambda2 =h_c*HR,
                     Ts = tot_dur, Tr = accr_time,
                     alpha = 0.05, sided = 2)

    out <- abs(round(res$n, 0) - given_N)  # absolute value --> minimum at 0

  }

  return(out)
}

to_minimize(24)

# returns optimal duration and updated Nevents for fixed N (uses 'optimize' as default as suggested by optimx help if problem is uni dimensional)
optim_duration <- function(tot_dur, accr_time = 24,
                           h_c = 0.05/12, HR =0.775,
                           easy_one = TRUE)
{
  if (easy_one)
    opt <- optimize(to_minimize, c(accr_time + 1, 300), h_c = h_c, HR = HR)
  else
    opt <- optimx(par = c(tot_dur = tot_dur),
                  fn = to_minimize,
                  method = "L-BFGS-B",
                  lower = accr_time + 1,  # lower bound for total duration can't be less than enroll time
                  upper = 300,
                  itnmax=c(50),
                  h_c = h_c,
                  HR = HR
                  #control = list(all.methods = TRUE)
    )

  # summary(opt, order = "convcode") %>%
  #   select(-value, -niter, -gevals, -fevals)

  # update nEvents given new study duration
  nFailures_new <- nSurvival(lambda1 =h_c, lambda2 =h_c*HR,
            Ts = opt[[1]], Tr = accr_time,
            alpha = 0.05, sided = 2)$nEvents
  return(
    list(optim_tot_dur = opt[[1]],
      updated_nFailures = nFailures_new)
    )

}


# test
optim_duration(30)
optim_duration(25, h_c = 0.06/12)


# generates data frame with simulation results ready for gganimate plotting
duration_sim <- function(h_c_seq = seq(0.03, 0.07, 0.005),
                         HR_seq = seq(0.75, 0.8, 0.0025),
                         easy_one = TRUE)
{
  do.call("rbind",
          lapply(h_c_seq, function(i)
            do.call("rbind",
                    lapply(HR_seq, function(j)
                    {
                      out <- optim_duration(25, h_c = i/12, HR = j,
                                            easy_one = easy_one)
                      data.frame(optim_dur = out$optim_tot_dur,
                                 HR = j, baseline_haz = i,
                                 required_events = round(out$updated_nFailures, 0))
                    }
                    )
            )
          ))
}



system.time(
  dur_dat <- duration_sim()

)

# data-set for background reference result (in order to create static layer you have to eliminate the 'transition' variable !!) NOTE: how argument 'size' is transfered from main aes to static layer (also adding legend with no conflict)

ref_dat <- dur_dat %>% filter(baseline_haz == 0.05) %>%
  select(-baseline_haz) %>%
  rename(ref_HR = HR)

# MIGRATED INTO RMARKDOWN HTML
# ggplot(dur_dat, aes(HR, optim_dur, size = required_events)) +
#   geom_point(show.legend = FALSE, alpha = 0.8, colour = "pink") +
#   geom_point(data = ref_dat, aes(ref_HR, optim_dur),
#              alpha = 0.4, colour = "blue", fill = "white") +    # reference background
#   guides(size = guide_legend("Required\n failures")) +
#   ggtitle("Change in total study duration (versus reference in blue)\n by varying of assumed HR and active comparator hazard\n (Author: Federico Bonofiglio)",
#           subtitle = "Active comparator hazard is {round(frame_time, 3)}") +
#   ylab("Total study duration (months)") +
#   xlab(expression(
#     "increasing treatment effect" %<-% HR %->% "decreasing treatment effect" )) +
#   geom_vline(xintercept = 0.775, colour = "green", alpha = 0.5) +
#   geom_hline(yintercept = 30, colour = "green", alpha = 0.5) +
#   transition_time(baseline_haz) +
#   ease_aes('linear')
#
# anim_save("Wednesday_challenge.gif", path = file.path(getwd(), "out"))


# write.table(dur_dat, file.path(getwd(), "out", "dur_dat"))

