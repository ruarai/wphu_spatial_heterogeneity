
library(targets)
library(tidyverse)
library(lubridate)

library(tidybayes)

source("R/plots/plot_theme.R")
source("R/plots/make_results_quants.R")

model_data <- tar_read(model_data)
model_fit <- read_rds(tar_read(model_fit))


sim_cases_wt <- spread_draws(model_fit$draws(), n_cases_sim_wt[t, LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_wt[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  select(date, LGA, draw = .draw, n_cases = n_cases_sim_wt)


sim_cases_delta <- spread_draws(model_fit$draws(), n_cases_sim_delta[t, LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_delta[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  select(date, LGA, draw = .draw, n_cases = n_cases_sim_delta)



error_wt <- sim_cases_wt %>%
  left_join(model_data$fit_data_tbl_wt %>% select(date, LGA, n_cases_true = n_cases)) %>%
  
  drop_na(n_cases) %>% 
  
  mutate(abs_error = abs(log(n_cases + 1) - log(n_cases_true + 1))) %>%
  
  group_by(LGA, draw) %>% 
  
  summarise(mean_abs_error_wt = mean(abs_error))


error_delta <- sim_cases_delta %>%
  left_join(model_data$fit_data_tbl_delta %>% select(date, LGA, n_cases_true = n_cases)) %>%
  
  drop_na(n_cases) %>% 
  
  mutate(abs_error = abs(log(n_cases + 1) - log(n_cases_true + 1))) %>%
  
  group_by(LGA, draw) %>% 
  
  summarise(mean_abs_error_delta = mean(abs_error))

error_total <- error_wt %>%
  left_join(error_delta, by = c("LGA", "draw")) %>% 
  
  mutate(mean_abs_error_total = mean_abs_error_wt / 2 + mean_abs_error_delta / 2) %>%
  
  fix_LGA() %>% 
  
  group_by(LGA) %>%
  mutate(mean_LGA = mean(mean_abs_error_total)) %>%
  ungroup() %>%
  arrange(mean_LGA) %>%
  mutate(LGA = factor(LGA, unique(LGA)))
  
ggplot(error_total) +
  
  ggridges::geom_density_ridges(aes(x = mean_abs_error_total, y = LGA),
                                fill = ggokabeito::palette_okabe_ito(1),
                                colour = "black",
                                scale = 1.2) +
  
  xlab(NULL) +
  ylab(NULL) +
  
  coord_cartesian(clip = "off") +
  
  plot_theme +
  theme(panel.border = element_blank())



ggsave(
  "results/ppd_fit_MAE.png",
  bg = "white",
  width = 4, height = 3, dpi = 300
)

