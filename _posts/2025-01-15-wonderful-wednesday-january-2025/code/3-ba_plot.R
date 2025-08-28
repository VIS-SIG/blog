library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


# Load data
d <- read_excel('Data/2024-12-11-psi-vissig-goniometer.xlsx') |>
  mutate(time=as.numeric(time))

# Bland-Altman plot - Inter-goniometer agreement
df<-d |>
    pivot_wider(names_from = `rater`, values_from = `y`) |>
    mutate(
      time = paste("T", time),
      diff = manual - electro,
      avg = (manual + electro) / 2)
df2 <- df |>
  group_by(time) |>
  summarise(
    g_avg = mean(avg),
    g_diff = mean(diff),
    sd_diff = sd(diff),
    n = n()
  )
df2  
df <- df |>
  left_join(df2)

ggplot(data=df, aes(x = avg, y = diff, col = ifelse(diff>(g_diff+1.96*sd_diff) | diff<(g_diff-1.96*sd_diff), "1", "0"))) +
  geom_point() +
    geom_rug(col="grey") +
  # orange and blue colors do not show legend  
  scale_color_manual(values = c("black", "red")) +
  geom_hline(aes(yintercept = g_diff)) +
    geom_hline(aes(yintercept = g_diff+1.96*sd_diff), linetype = "dashed") +
    geom_hline(aes(yintercept = g_diff-1.96*sd_diff), linetype = "dashed") +
  #geom_smooth(method = "lm", se = F) +
  labs(
    title = "Bland-Altman plot - Inter-goniometer agreement (electro vs. manual)",
    x = "Average",
    y = "Difference",
    caption = "Solid line represents average of differences. \nDashed lines represent limits of agreement (±1.96xSD). \nData outside 95% limits of agreement are labeled"
  ) +
  facet_wrap(time~.) +
  #plot data outside g_diff+1.96*sd_diff
  geom_text(data=df, aes(label = ifelse(diff>(g_diff+1.96*sd_diff) | diff<(g_diff-1.96*sd_diff), id, NA), hjust = -0.5)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme_bw() +
  theme(legend.position = "none")
# save the plot
ggsave("Figures/inter-goniometer-agreement.png", width = 10, height = 5)

# Bland-Altman plot - Intra-goniometer agreement
df<-NULL
for (i in 1:3){
  c <- combn(1:3,2)[,i]
  d1 <- d |>
    filter(time %in% c) |>
    pivot_wider(names_from = `time`, values_from = `y`) 
  
  names(d1) <- c("id", "goniometer", "A", "B") 
  
  d2 <- d1 |>
    mutate(
      diff = A - B,
      avg = (A + B) / 2,
      comparison=paste("T",c[1],"vs", "T", c[2]))
  
  df <- rbind(df, d2)
}

df2 <- df |>
  group_by(goniometer, comparison) |>
  summarise(
    g_avg = mean(avg),
    g_diff = mean(diff),
    sd_diff = sd(diff),
    n = n()
  )

df <- df |>
  left_join(df2)
ggplot(data=df, aes(x = avg, y = diff, col = ifelse(diff>(g_diff+1.96*sd_diff) | diff<(g_diff-1.96*sd_diff), "1", "0"))) +
  geom_point() +
  geom_rug(col="grey") +
  # orange and blue colors do not show legend  
  scale_color_manual(values = c("black", "red")) +
  geom_hline(aes(yintercept = g_diff)) +
  geom_hline(aes(yintercept = g_diff+1.96*sd_diff), linetype = "dashed") +
  geom_hline(aes(yintercept = g_diff-1.96*sd_diff), linetype = "dashed") +
  #geom_smooth(method = "lm", se = F) +
  labs(
    title = "Bland-Altman plot - Intra-goniometer agreement",
    x = "Average",
    y = "Difference",
    caption = "Solid line represents average of differences. \nDashed lines represent limits of agreement (±1.96xSD). \nData outside 95% limits of agreement are labeled"
  ) +
  # add text to geom_hline
  facet_wrap(goniometer~comparison) +
  geom_text(aes(label = ifelse(diff>(g_diff+1.96*sd_diff) | diff<(g_diff-1.96*sd_diff), id, NA), hjust = -0.5)) +
  theme_bw() +
  theme(legend.position = "none")
# save the plot
ggsave("Figures/intra-goniometer-agreement.png", width = 9, height = 6)

