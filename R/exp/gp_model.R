


library(cmdstanr)


model <- cmdstan_model("src/growth_rate_vacc_gp.stan")


case_data <- tar_read(case_data)
mobility_data <- tar_read(mobility_data)
vaccination_data <- tar_read(vaccination_data)

source("R/get_model_data.R")
model_data <- get_model_data(case_data, mobility_data, vaccination_data)

model_data_stan <- model_data[c("t_max_wt", "n_pops", "n_cases_wt", "mobility_wt", "nb_theta",
                                "t_max_delta", "n_cases_delta", "mobility_delta", "p_vacc",
                                "t_ix_wt", "t_ix_delta", "rho", "alpha")]

fit <- model$optimize(
  data = model_data_stan,
  iter = 10000
)

fit <- model$sample(
  data = model_data_stan, 
  seed = 4, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 100,
  iter_warmup = 1000,
  iter_sampling = 1000
)


plot_cases <- function(obs_fit_data, case_quants) {
  ggplot() +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant),
                case_quants)  +
    
    geom_line(aes(x = date, y = median),
              case_quants,
              colour = last(quant_fills)) +
    
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

sim_cases_quants_wt <- spread_draws(fit$draws(), n_cases = n_cases_sim_wt[t, LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_wt[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  select(date, LGA, draw = .draw, value = n_cases_sim_wt) %>%
  make_quants()

sim_cases_quants_delta <- spread_draws(fit$draws(), n_cases = n_cases_sim_delta[t, LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_delta[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  select(date, LGA, draw = .draw, value = n_cases_sim_delta) %>%
  make_quants()

p_cases_wt <- plot_cases(
  model_data$fit_data_tbl_wt,
  sim_cases_quants_wt
) +
  ggtitle("Simulated and observed cases by symptom onset date \u2012 Second wave epidemic")

p_cases_wt


p_cases_delta <- plot_cases(
  model_data$fit_data_tbl_delta,
  sim_cases_quants_delta
) +
  ggtitle("Simulated and observed cases by symptom onset date \u2012 Delta")

p_cases_delta


error_quants_wt <- spread_draws(fit$draws(), n_cases = log_f_pred_wt[t, LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_wt[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  select(date, LGA, draw = .draw, value = log_f_pred_wt) %>%
  make_quants()

ggplot(error_quants_wt) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  geom_line(aes(x = date, y = median),
            colour = last(quant_fills)) +
  
  scale_fill_manual(values = quant_fills) +
  
  geom_hline(yintercept = 0) +
  
  facet_wrap(~LGA, ncol = 3) +
  
  plot_theme +
  theme(legend.position = "none")

error_quants_delta <- spread_draws(fit$draws(), n_cases = log_f_pred_delta[t, LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_wt[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  select(date, LGA, draw = .draw, value = log_f_pred_delta) %>%
  make_quants()

ggplot(error_quants_delta) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = quant)) +
  geom_line(aes(x = date, y = median),
            colour = last(quant_fills)) +
  
  scale_fill_manual(values = quant_fills) +
  
  geom_hline(yintercept = 0) +
  
  facet_wrap(~LGA, ncol = 3) +
  
  plot_theme +
  theme(legend.position = "none")

sim_g <- spread_draws(
  fit$draws(), gamma, mu_wt, log_f_pred_wt[t, LGA], b[LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_wt[t],
         LGA = model_data$LGA_names[LGA]) %>%
  
  left_join(model_data$fit_data_tbl_wt, by = c("date", "t", "LGA")) %>%
  
  mutate(g = mu_wt * (1 - mobility * b) - gamma,
         g_adj = mu_wt * (1 - mobility * b) - gamma + log_f_pred_wt) 

sim_g_quants <- sim_g %>%
  select(date, LGA, draw = .draw, value = g) %>%
  make_quants()

sim_g_adj_quants <- sim_g %>%
  select(date, LGA, draw = .draw, value = g_adj) %>%
  make_quants()



ggplot()  +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
              fill = ggokabeito::palette_okabe_ito(5),
              sim_g_adj_quants) +
  geom_line(aes(x = date, y = median),
            sim_g_adj_quants,
            colour = ggokabeito::palette_okabe_ito(5)) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
              fill = ggokabeito::palette_okabe_ito(3),
              sim_g_quants) +
  geom_line(aes(x = date, y = median),
            sim_g_quants,
            colour = ggokabeito::palette_okabe_ito(3)) +
  
  scale_alpha_manual(values = alpha_vals) +
  
  geom_hline(yintercept = 0) +
  
  facet_wrap(~LGA) +
  
  plot_theme


sim_g %>%
  filter(.draw %% 500 == 1,
         LGA == "Melbourne (C)") %>%
  
  ggplot() +
  geom_line(aes(x = date, y = g, group = .draw)) +
  geom_line(aes(x = date, y = g_adj, group = .draw),
            
            colour = ggokabeito::palette_okabe_ito(3)) +
  
  facet_wrap(~.draw,
             ncol = 2) +
  
  plot_theme


spread_draws(
  fit$draws(), gamma, mu_wt, log_infections_t_delta[t, LGA]) %>%
  ungroup() %>%
  mutate(date = model_data$dates_delta[t],
         LGA = model_data$LGA_names[LGA])  %>%
  select(date, LGA, draw = .draw, value = log_infections_t_delta) %>%
  make_quants() %>% 
  ggplot()  +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
              fill = ggokabeito::palette_okabe_ito(5)) +
  geom_line(aes(x = date, y = median),
            colour = ggokabeito::palette_okabe_ito(5)) +
  
  scale_alpha_manual(values = alpha_vals) +
  
  facet_wrap(~LGA) +
  
  geom_point(aes(x = date, y = log(n_cases)),
             model_data$fit_data_tbl_delta) +
  
  plot_theme

