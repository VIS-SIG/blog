library(tidyverse)
library(splines)

df1 <- read_csv("LSM_Score.csv") %>%
  mutate(CFB = EOS_kPa - Baseline_kPa)

### Weight ###
model <- lm(CFB ~ Group + Sex + Age + ns(Weight, df = 3) + Group*Weight + Group*Sex, data = df1)

df1 %>%
  summarise(Weight)
summary(model)

temp <- expand.grid(
  Group = c("LIV.52 DS", "Placebo"),
  Sex       = c("Male", "Female"),
  # Steatosis = c("No Steatosis", "Grade improvement", "Deteriorate"),
  Age       = mean(df1$Age),           
  Weight = seq(50, 100, by = 1)           
)

pred <- predict(model, newdata = temp, interval = "confidence")

df2 <- temp %>%
  mutate(
    fit = pred[, "fit"],
    lwr = pred[, "lwr"],
    upr = pred[, "upr"]
  )

my_black <- "#252525"
plot <- ggplot(df2, aes(x = Weight, y = fit,
                     color = Group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Group),
              alpha = 0.15, color = NA) +
  scale_y_continuous(
    breaks=seq(-5, 5, by = 1)) +
  # scale_color_discrete(guide="none") +
  scale_fill_discrete(guide="none") +
  facet_wrap(~Sex) +
  labs(x = "Weight (kg)", y = "Change in LSM (kPa)") +
  theme_minimal() +
  theme(panel.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour = "#f0f0f0",
                                      linewidth = 0.5,
                                      linetype = 1),
        panel.border=element_rect(fill = NA,
                                  colour = my_black,
                                  linewidth = 1,
                                  linetype = 1),
        strip.background = element_rect(fill = NA,
                                        colour = my_black,
                                        linewidth = 1,
                                        linetype = 1),
        strip.text = element_text(
          colour = my_black,
          size = 12),
        axis.line.y=element_line(colour = my_black,
                                 linewidth = 0.5,
                                 linetype = 1),
        axis.text.x=element_text(
          colour = my_black,
          size = 14),
        axis.text.y=element_text(
          colour = my_black,
          size = 14),
        axis.title=element_text(
          colour = my_black,
          size =14),
        legend.text=element_text(
          colour = my_black,
          size = 14),
        legend.title = element_text(size = 14))
ggsave(plot, filename = "WW_Oct01.png", width = 16, height = 9)

