

plot_theme <- list(
  theme_bw(),
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 10, hjust = 0),
    text = element_text(family = "Helvetica"),
    panel.grid.minor = element_line(linetype = "dotted", colour = "grey80")
  )
)