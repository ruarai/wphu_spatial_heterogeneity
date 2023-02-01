
library(targets)
library(tidyverse)
library(lubridate)

library(tidybayes)

source("R/plots/plot_theme.R")
source("R/plots/make_results_quants.R")

model_data <- tar_read(model_data)
model_fit <- read_rds(tar_read(model_fit))




bayesplot::mcmc_trace(
  model_fit$draws(c("mu_wt", "mu_delta",
                    "gamma", "b", "VE"))
) +
  
  plot_theme


ggsave(
  "results/diag_trace.png",
  bg = "white",
  width = 10, height = 7, dpi = 300
)

bayesplot::mcmc_pairs(
  model_fit$draws(c("gamma", "b[1]"))
)
