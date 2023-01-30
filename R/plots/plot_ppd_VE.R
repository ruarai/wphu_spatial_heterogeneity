
library(targets)
library(tidyverse)
library(lubridate)

library(tidybayes)

source("R/plots/plot_theme.R")
source("R/plots/make_results_quants.R")

model_data <- tar_read(model_data)
model_fit <- read_rds(tar_read(model_fit))




draws_VE <- spread_draws(model_fit$draws(), VE)



ggplot(draws_VE) +
  ggridges::geom_density_line(aes(x = VE),
                                fill = ggokabeito::palette_okabe_ito(5) %>%
                                  shades::lightness(60),
                                colour = "black",
                              n = 256) +
  
  coord_cartesian(xlim = c(0, 1)) +
  
  plot_theme
