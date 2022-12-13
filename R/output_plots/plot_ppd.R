
library(targets)
library(tidyverse)
library(lubridate)

library(tidybayes)

source("R/plot_theme.R")
source("R/make_results_quants.R")

model_data <- tar_read(model_data)
model_fit <- read_rds(tar_read(model_fit))






alpha_vals <- scales::rescale(rev(1/1.7^(1:4)), to = c(0.2, 0.99))
quant_fills <- shades::opacity("#006699", alpha_vals)


plot_cases <- function(obs_fit_data, case_quants) {
  ggplot() +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant),
                case_quants) +
    
    geom_point(aes(x = date, y = n_cases), 
               obs_fit_data,
               size = 0.8) +
    
    geom_point(aes(x = date, y = n_cases), 
               obs_fit_data,
               colour = "white",
               size = 0.1, stroke = 0.3)  +
    
    scale_fill_manual(values = quant_fills) +
    
    facet_wrap(~LGA) +
    plot_theme +
    
    xlab(NULL) + ylab(NULL) +
    
    theme(legend.position = "none")
}

make_quants <- . %>%
  
  pivot_wider(names_from = draw,
              names_prefix = "draw_",
              values_from = value) %>%
  make_results_quants(c(0.5, 0.9, 0.95, 0.99), col_prefix = "draw_")

sim_cases_quants_wt <- spread_draws(model_fit$draws(), n_cases = n_cases_sim_wt[LGA][t]) %>%
  ungroup() %>%
  mutate(LGA = model_data$LGA_names[LGA],
         date = model_data$dates_wt[t],) %>%
  
  select(LGA, date, draw = .draw, value = n_cases_sim_wt) %>%
  make_quants()

sim_cases_quants_delta <- spread_draws(model_fit$draws(), n_cases = n_cases_sim_delta[LGA][t]) %>%
  ungroup() %>%
  mutate(LGA = model_data$LGA_names[LGA],
         date = model_data$dates_delta[t],) %>%
  
  select(LGA, date, draw = .draw, value = n_cases_sim_delta) %>%
  make_quants()

p_cases_wt <- plot_cases(
  model_data$fit_data_tbl_wt,
  sim_cases_quants_wt
) +
  ggtitle("Simulated and observed cases by symptom onset date \u2012 Second wave epidemic")

p_cases_wt
p_cases_wt + scale_y_log10() + coord_cartesian(ylim = c(0.75, NA))

plot_cases(
  model_data$fit_data_tbl_delta,
  sim_cases_quants_delta
) +
  ggtitle("Simulated and observed cases by symptom onset date \u2012 Delta epidemic")




spread_draws(model_fit$draws(), c(b)[LGA]) %>%
  filter(.draw %% 4 == 1) %>% 
  mutate(LGA = model_data$LGA_names[LGA]) %>%
  
  group_by(LGA) %>%
  mutate(LGA_mean = mean(b)) %>%
  ungroup() %>%
  arrange(LGA_mean) %>%
  mutate(LGA = factor(LGA, levels = unique(LGA))) %>% 
  
  ggplot() +
  geom_point(aes(y = LGA, x = b),
             position = position_jitter(height = 0.3),
             size = 0.3) +
  
  plot_theme

growth_rate_quants_wt <- spread_draws(model_fit$draws(), mu_wt, gamma, c(b)[LGA]) %>%
  ungroup() %>%
  filter(.draw %% 10 == 1) %>% 
  mutate(LGA = model_data$LGA_names[LGA]) %>% 
  
  right_join(model_data$fit_data_tbl_wt %>% select(LGA, date, mobility), by = "LGA") %>%
  
  mutate(g = mu_wt * (1 - b * as.vector(mobility)) - gamma) %>%
  
  select(LGA, date, draw = .draw, value = g) %>%
  make_quants()

ggplot() +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = LGA,
                  group = interaction(LGA, quant), alpha = quant),
              growth_rate_quants_wt) +
  
  #scale_fill_manual(values = quant_fills) +
  
  scale_alpha_manual(values = alpha_vals) +
  
  xlab(NULL) + ylab("Growth rate") +
  
  
  plot_theme +
  theme(legend.position = "none")


growth_rate_quants_wt <- spread_draws(model_fit$draws(), mu_wt, gamma, c(b)[LGA]) %>%
  ungroup() %>%
  filter(.draw %% 10 == 1) %>% 
  mutate(LGA = model_data$LGA_names[LGA]) %>% 
  
  right_join(model_data$fit_data_tbl_wt %>% select(LGA, date, mobility) %>%
               group_by(LGA) %>% summarise(m_min = min(mobility), m_max = max(mobility), m_mean = mean(mobility)),
             by = "LGA") %>% 
  #filter(LGA == "Melbourne (C)") %>%
  
  expand_grid(m = seq(0.1, 2, by = 0.05)) %>% 
  filter(m >= m_min, m <= m_max) %>% 
  
  mutate(g = -mu_wt * b * 100 / gamma,
         m_adj = m - m_mean) %>%
  
  select(LGA, m, m_adj, draw = .draw, value = g) %>%
  make_quants()


ggplot(growth_rate_quants_wt) +
  geom_linerange(aes(x = LGA, ymin = lower, ymax = upper, group = interaction(LGA, quant), alpha = quant, fill = LGA)) +
  
  scale_alpha_manual(values = alpha_vals) +
  
  #geom_hline(yintercept = 0) +
  
  plot_theme



