library(tidyverse)
library(ggridges)
library(readr)
library(ggtext)
library(rlang)

## Read data and manipulate
data <- read_csv("BIG_DATA_PSI_WW_DEC2020.csv") %>%
  mutate(
    TRT01PC = if_else(TRT01P == "INT", "Intensive treatment", "Standard of care"),
    STUDYIDC = paste0("Study ", STUDYID)
  )


plot_data <-
  data %>%
  pivot_longer(
    cols = c(
      AGE,
      ALBSI,
      BASOSI,
      BASE,
      BICARSI,
      BILISI,
      BMI,
      BUNSI,
      CHOL_HDL,
      CHOLSI,
      CREATSI,
      EOSSI,
      GGTSI,
      GLUCPSI,
      HCT,
      HDLSI,
      HDT,
      HGBSI,
      KSI,
      LDLSI,
      LPASI,
      LYMLESI,
      LYMSI,
      TRIGFSI,
      URATESI,
      WBCSI
    ),
    names_to = "var",
    values_to = "val"
  )


ridge_plot <- 
  plot_data %>%
  ggplot(aes(
    x = val,
    y = factor(STUDYID),
    fill = paste(STUDYIDC, TRT01PC)
  )) +
  geom_density_ridges(alpha = .4,
                      rel_min_height = .01,
                      color = "white") +
  scale_fill_cyclical(
    values = c("tomato", "dodgerblue"),
    name = "",
    labels = c(`Study 1 Intensive treatment` = "Intensive treatment",
               `Study 1 Standard of care` = "Standard of care"),
    guide = "legend"
  ) +
  theme_ridges(grid = FALSE) +
  facet_wrap( ~ var, scales = "free", ncol = 5) +
  labs(
    x = "",
    y = "Study",
    fill = "Treatment",
    title = "Visualising baseline characteristics and demographics by study",
    subtitle = "Presented are a selection of continously measured variables",
    caption = "Data: BIG_DATA_PSI_WW_DEC2020.csv"
  ) +
  theme(legend.position = "bottom")

ggsave(
  "ridge_plot.png",
  ridge_plot,
  height = 12,
  width = 16,
  dpi = 330
)
