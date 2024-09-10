# Wonderful Wednesday
# Statistical Power

# by Tom Marlow


# T-test that shows poewr to detcet difference of 5 ot 8% in EF

# Assumptions

library(tidyverse)
library(pwr)

fn_pwr <- function(n, d) {
  power <- pwr.t.test(n = n,
                      d = d,
                      sig.level = 0.05,
                      alternative = "two.sided",
                      type = "two.sample")$power
  return(power * 100)
}

ssize = c(2:12)
msd <- tibble(effect = rep(c(6, 8, 10), each = 3),
              sd_inc = rep(c(1, 1.25, 1.5), 3)) %>%
  mutate(sd = 4 * sd_inc) %>%
  mutate(delta = effect / sd)

pwr_tab <- tibble(effect = factor(rep(msd$effect, each = length(ssize)),
                                  levels = c(6, 8, 10),
                                  labels = c("6%", "8%", "10%")),
                  sd_inc = as.factor(rep(msd$sd_inc, each = length(ssize))),
                  n = rep(ssize, length(msd$delta)),
                  d = rep(msd$delta, each = length(ssize))) %>%
  mutate(power = fn_pwr(n, d)) 

pwr_fig <- ggplot(data = pwr_tab,
                  aes(x = n, y = power, colour = sd_inc)) +
  geom_line() +
  geom_hline(yintercept = 80) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = ssize, name = "Sample size") +
  scale_y_continuous(breaks = seq(0, 100, by = 20)) +
  labs(
    y = "Power (%)",
    title = "Power to detect deltas of 6, 8 and 10% in ejection fraction",
    subtitles = "Two-sided, two-sample t-test, alpha=0.05, sd=4",
    colour = "SD increase"
  ) +
  geom_point(size = 2, shape = 21, fill = "white") +
  theme_bw() +
  theme(
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    legend.position = "bottom") +
  scale_colour_viridis_d() +
  facet_wrap(effect ~ .)

pwr_fig
