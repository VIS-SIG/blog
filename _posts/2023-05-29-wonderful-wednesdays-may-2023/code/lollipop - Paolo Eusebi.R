library(RCurl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(cowplot)
x <-
  getURL(
    "https://raw.githubusercontent.com/VIS-SIG/Wonderful-Wednesdays/master/data/2023/2023-05-10/CGI_S_3_groups_csv.csv"
  )
y <- read.csv(text = x) %>%
  rename(Group = CGI)
y

l <- y %>%
  pivot_longer(cols = X1:X7,
               names_to = "Category",
               values_to = "n") %>%
  mutate(Category = as.numeric(gsub("X", "", Category))) %>%
  group_by(Week, Group) %>%
  arrange(Category) %>%
  mutate(
    CumN = cumsum(n),
    Week = paste("Week", Week),
    CumFreq = CumN / Total.sample.size,
    Freq = n / Total.sample.size,
    `Cumulative %` = round(CumFreq * 100, 1),
    `%` = round(Freq * 100, 1)
  ) %>%
  ungroup() %>%
  group_by(Week, Category) %>%
  arrange(Group, .by_group = T) %>%
  mutate(
    Odds = CumFreq / (1 - CumFreq),
    OR = ifelse(Group == "Active", round(Odds / Odds[2], 2), NA),
    seOR = ifelse(Group == "Active", sqrt(
      1 / CumN + 1 / (Total.sample.size - CumN) + 1 / CumN[2] + 1 / (Total.sample.size[2] -
                                                                       CumN[2])
    ), NA),
    ORlower95CI = ifelse(Group == "Active", round(OR - 1.96 * seOR, 2), NA),
    ORupper95CI = ifelse(Group == "Active", round(OR + 1.96 * seOR, 2), NA)
  )
l

k = 3
labs <- c("Normal, not at all ill -   1",
          "Normal to borderline ill - <=2",
          "Normal to mildly ill - <=3")

p1 <-
  ggplot(data = l[l$Category <= k,], aes(x = `Cumulative %`, y = Category, col =
                                           Group)) +
  geom_point(size = 5) +
  facet_grid(rows = vars(Week)) +
  labs(title = "Cumulative Percentage of response", y = "", x = "") +
  scale_y_continuous(breaks = 1:k, labels = labs) +
  scale_x_continuous(limits = c(0, 100)) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(title = "%") +
  geom_text_repel(aes(label = paste(sprintf(
    "%.1f", `Cumulative %`
  ), "%")),
  nudge_y = 0.1)
p1

legend_b <- get_legend(p1 +
                         guides(color = guide_legend(nrow = 1)) +
                         theme(legend.position = "bottom"))


p2 <-
  ggplot(data = l[l$Category <= k &
                    !is.na(l$OR),], aes(x = OR, y = Category)) +
  geom_point(size = 3, pch = 15) +
  geom_linerange(aes(xmin = ORlower95CI, xmax = ORupper95CI)) +
  geom_vline(xintercept = 1, linetype = 3) +
  facet_grid(rows = vars(Week)) +
  scale_y_continuous(breaks = 1:k, labels = labs) +
  scale_x_continuous(limits = c(0, 5)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.y = element_blank()) +
  labs(x = "", title = "OR") +
  geom_text_repel(aes(label = sprintf("%.2f", OR)),
                  nudge_y = 0.1)
p2

plot_row <- plot_grid(p1, p2, nrow = 1, rel_widths = c(2, 1))

title <- ggdraw() +
  draw_label(
    "Active treatment is more effective than comparator",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 24,
  ) +
  theme(plot.margin = margin(0, 0, 0, 7))

p <- plot_grid(title,
               plot_row,
               legend_b,
               ncol = 1,
               rel_heights = c(0.1, 1, 0.05))

p
