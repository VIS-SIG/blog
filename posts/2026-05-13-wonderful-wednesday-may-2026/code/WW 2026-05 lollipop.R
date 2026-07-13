library(tidyverse)

# ── Data ──────────────────────────────────────────────────────────────────────
lollipop <- tribble(
  ~group,       ~years_first_line,
  "Achieved CTT",   11,
  "Failed CTT",      2
) |>
  mutate(group = factor(group, levels = c("Failed CTT", "Achieved CTT")))

# ── Colours ───────────────────────────────────────────────────────────────────
col_achieved <- "#1D9E75"   # teal
col_failed   <- "#A32D2D"   # red

# ── Plot ──────────────────────────────────────────────────────────────────────
p <- ggplot(lollipop, aes(x = years_first_line, y = group, colour = group)) +

  # Reference line at x = 0
  geom_vline(xintercept = 0, colour = "grey80", linewidth = 0.4) +

  # Lollipop stems
  geom_segment(
    aes(x = 0, xend = years_first_line, yend = group),
    linewidth = 1.2
  ) +

  # Lollipop heads
  geom_point(size = 7) +

  # Value labels inside the heads
  # geom_text(
  #   aes(label = paste0(years_first_line, " yrs")),
  #   colour = "white", size = 2.8, fontface = "bold"
  # ) +

  # 9-year delay annotation
  annotate(
    "segment",
    x = 2, xend = 11, y = 1.5, yend = 1.5,
    colour = "grey40", linewidth = 0.6,
    arrow = arrow(ends = "both", length = unit(4, "pt"), type = "closed")
  ) +
  annotate(
    "text",
    x = 6.5, y = 1.62,
    label = "9-year delay",
    size = 3, colour = "grey30", fontface = "italic"
  ) +

  # ── Scales ──────────────────────────────────────────────────────────────────
  scale_colour_manual(
    values = c("Achieved CTT" = col_achieved, "Failed CTT" = col_failed),
    guide  = "none"
  ) +
  scale_x_continuous(
    name   = "Years on first-line therapy before treatment intensification",
    limits = c(0, 14),
    breaks = seq(0, 14, 2)
  ) +
  scale_y_discrete(name = NULL) +

  # labs(
  #   caption = "Intensification triggered at HbA1c ≥6.5% (CTT 1 base case).\nAchieved group: HbA1c ≤6.5%, weight reduction ≥10%, no hypoglycaemia.\nFirst-line therapy preferred: lower complication risk, no hypoglycaemia, no weight gain."
  # ) +

  # ── Theme ───────────────────────────────────────────────────────────────────
  theme_minimal(base_size = 12) +
  theme(
    panel.background   = element_rect(fill = "white", colour = NA),
    plot.background    = element_rect(fill = "white", colour = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.4),
    axis.text.y        = element_text(size = 11, face = "bold", colour = "grey20"),
    axis.title.x       = element_text(size = 10, colour = "grey30",
                                      margin = margin(t = 8)),
    plot.caption       = element_text(size = 8, colour = "grey50",
                                      lineheight = 1.4, margin = margin(t = 10)),
    plot.margin        = margin(16, 20, 10, 12)
  )

# ── Save ──────────────────────────────────────────────────────────────────────
ggsave(
  "lollipop.png",
  plot   = p,
  width  = 8,
  height = 3.2,
  dpi    = 200,
  bg     = "white"
)

message("Saved to treatment_timeline.png")
