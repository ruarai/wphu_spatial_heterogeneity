

plot_theme <- list(
  theme_bw(),
  theme(
    strip.background = element_blank(),
    strip.text = element_text(size = 10, hjust = 0),
    text = element_text(family = "Helvetica"),
    panel.grid.minor = element_line(linetype = "dotted", colour = "grey80")
  )
)


alpha_vals <- scales::rescale(rev(1/1.7^(1:4)), to = c(0.2, 0.99))
quant_fills <- shades::opacity("#006699", alpha_vals)