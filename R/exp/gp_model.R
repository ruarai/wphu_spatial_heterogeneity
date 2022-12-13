


library(cmdstanr)


model <- cmdstan_model("src/growth_rate_vacc_gp.stan")


case_data <- tar_read(case_data)
mobility_data <- tar_read(mobility_data)
date_period_wt <- c(ymd("2020-05-25"), ymd("2020-10-15"))

source("R/model_data_functions.R")
source("R/population.R")

LGAs <- unique(case_data$LGA)
LGAs <- LGAs[order(LGAs)]




case_counts_wt <- get_case_counts(case_data, date_period_wt)

mobility_smooth_wt <- smooth_mobility_data(mobility_data, date_period_wt)


fit_data_wt <- case_counts_wt %>%
  select(LGA, date, n_cases) %>%
  left_join(population_LGAs, by = "LGA") %>% 
  left_join(mobility_smooth_wt, by = c("LGA", "date")) %>%
  
  mutate(t = as.numeric(date - date_period_wt[1]) + 1) %>%
  
  arrange(LGA, t) %>%
  
  filter(LGA == "Melton (C)")

dates_wt <- fit_data_wt$date %>% unique()
data_list <- list(
  t_max_wt = length(dates_wt),
  
  n_cases_wt = fit_data_wt$n_cases,
  
  mobility_wt = fit_data_wt$mobility,
  
  LGA_names = LGAs,
  dates_wt = fit_data_wt$date %>% unique(),
  
  fit_data_tbl_wt = fit_data_wt,
  
  t_ix = fit_data_wt$t,
  
  rho = 10,
  alpha = 0.1,
  
  nb_theta = 2000
)


model_data_stan <- data_list[c("t_max_wt", "n_cases_wt", "mobility_wt", "nb_theta",
                               "t_ix", "rho", "alpha")]

# fit <- model$optimize(
#   data = model_data_stan,
#   seed = 3
# )

fit <- model$sample(
  data = model_data_stan, 
  seed = 5, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 100,
  iter_warmup = 1000,
  iter_sampling = 1000
)


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

source("R/make_results_quants.R")
make_quants <- . %>%
  
  pivot_wider(names_from = draw,
              names_prefix = "draw_",
              values_from = value) %>%
  make_results_quants(c(0.5, 0.9, 0.95, 0.99), col_prefix = "draw_")

sim_cases_quants_wt <- spread_draws(fit$draws(), n_cases = n_cases_sim_wt[t]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_wt[t],) %>%
  
  select(date, draw = .draw, value = n_cases_sim_wt) %>%
  make_quants()

p_cases_wt <- plot_cases(
  fit_data_wt,
  sim_cases_quants_wt
) +
  ggtitle("Simulated and observed cases by symptom onset date \u2012 Second wave epidemic")

p_cases_wt
p_cases_wt + scale_y_log10() + coord_cartesian(ylim = c(0.75, NA))


error_quants_wt <- spread_draws(fit$draws(), n_cases = f_tilde[t]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_wt[t],) %>%
  
  select(date, draw = .draw, value = f_tilde) %>%
  make_quants()

ggplot(error_quants_wt) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  
  scale_fill_manual(values = quant_fills) +
  
  geom_hline(yintercept = 0) +
  
  plot_theme
