
fit <- tar_read(model_fit) %>% read_rds()
fit$draws(c("b[1]", "b[2]", "b[3]", "mu_wt", "mu_delta", "logit_VE", "gamma", "I_log_0_wt[1]")) %>%
  mcmc_pairs()

fit$draws(c("b[1]", "b[2]", "b[3]")) %>%
  mcmc_hist()

fit$draws("VE") %>% mcmc_hist(binwidth = 0.01) + coord_cartesian(xlim = c(0, 1))
fit$draws("gamma") %>% `/`(1, .) %>% mcmc_hist(binwidth = 0.05) + xlab("Infectious duration")


fit$draws(c("gamma", "mu_wt", "mu_delta", "a[1]", "b[1]", "I_log_0_wt[1]", "VE")) %>%
  mcmc_pairs()

spread_draws(fit$draws(), n_cases_sim_delta[LGA][t]) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = LGA_names[LGA],
         date = dates_delta[t]) %>% 
  ggplot() +
  geom_line(aes(x = date, y = n_cases_sim_delta, group = .draw),
            alpha = 0.2) +
  
  facet_wrap(~LGA, scales = "free_y") +
  
  geom_point(aes(x = date, y = n_cases), colour = "red",
             fit_data_delta, size = 0.6) +
  plot_theme

spread_draws(fit$draws(), n_cases_sim_wt[LGA][t]) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = model_data$LGA_names[LGA],
         date = model_data$dates_wt[t]) %>% 
  ggplot() +
  geom_line(aes(x = date, y = n_cases_sim_wt, group = .draw),
            alpha = 0.2) +
  
  facet_wrap(~LGA) +
  
  geom_point(aes(x = date, y = n_cases), colour = "red",
             fit_data_wt, size = 0.6) +
  plot_theme

spread_draws(fit$draws(), log_infections_t_delta[LGA][t]) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = LGA_names[LGA],
         date = dates_delta[t]) %>% 
  ggplot() +
  geom_line(aes(x = date, y = log_infections_t_delta, group = .draw),
            alpha = 0.2) +
  
  facet_wrap(~LGA) +
  
  geom_point(aes(x = date, y = log(n_cases)), colour = "red",
             fit_data_delta, size = 0.4) +
  plot_theme

spread_draws(fit$draws(), log_infections_t_wt[LGA][t]) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = LGA_names[LGA],
         date = dates_wt[t]) %>% 
  ggplot() +
  geom_line(aes(x = date, y = log_infections_t_wt, group = .draw),
            alpha = 0.2) +
  
  facet_wrap(~LGA) +
  
  geom_point(aes(x = date, y = log(n_cases)), colour = "red",
             fit_data_wt, size = 0.4) +
  plot_theme



spread_draws(fit$draws(), c(b)[LGA]) %>%
  filter(.draw %% 4 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>%
  
  ggplot() +
  geom_point(aes(y = LGA, x = b),
             position = position_jitter(height = 0.3),
             size = 0.3) +
  
  plot_theme




spread_draws(fit$draws(), c(b)[LGA], beta_wt, VE, gamma) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>%
  
  expand_grid(m = seq(0.5, 1.5, by = 0.01)) %>%
  
  left_join(
    fit_data_wt %>%
      group_by(LGA) %>%
      summarise(min_mobility = min(mobility),
                max_mobility = max(mobility)),
    
    by = "LGA"
  ) %>% 
  filter(m >= min_mobility, m <= max_mobility) %>% 
  
  mutate(R = beta_wt * (1 - b * m) / gamma) %>%
  
  ggplot() +
  
  geom_line(aes(x = m, y = R, group = interaction(LGA, .draw), colour = LGA)) +
  
  geom_hline(yintercept = 1) +
  plot_theme


spread_draws(fit$draws(), c(b)[LGA], mu_delta, VE, gamma) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>%
  
  expand_grid(m = seq(0.5, 1.5, by = 0.01)) %>%
  
  left_join(
    fit_data_delta %>%
      group_by(LGA) %>%
      summarise(min_mobility = min(mobility),
                max_mobility = max(mobility)),
    
    by = "LGA"
  ) %>% 
  filter(m >= min_mobility, m <= max_mobility) %>% 
  
  mutate(g = mu_delta * (1 - VE * 0.5) * (1 - b * m) - gamma) %>%
  
  ggplot() +
  
  geom_line(aes(x = m, y = g, group = interaction(LGA, .draw), colour = LGA)) +
  
  #facet_wrap(~LGA) +
  
  geom_hline(yintercept = 0) +
  plot_theme



spread_draws(fit$draws(), mu_wt, gamma, c(b)[LGA]) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>%
  
  right_join(mobility_data %>% filter(metric == "residential") %>% select(LGA, date, change)) %>%
  filter(wday(date) != 1, wday(date) != 7) %>% 
  
  mutate(m = (change + 100) / 100,
         beta_0 = mu_wt * (1 - b * m)) %>%

  
  mutate(R_0 = beta_0 / gamma) %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = R_0, group = .draw),
            alpha = 0.1,
            size = 0.3) +
  
  facet_wrap(~LGA) +
  
  plot_theme

spread_draws(fit$draws(), mu_delta, gamma, c(b)[LGA]) %>%
  filter(.draw == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>%
  
  right_join(mobility_data %>% filter(metric == "residential") %>% select(LGA, date, change)) %>%
  filter(wday(date) != 1, wday(date) != 7) %>%
  
  mutate(m = (change + 100) / 100,
         beta_0 = mu_delta * (1 - b * m)) %>%
  
  
  mutate(R_0 = beta_0 / gamma) %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = R_0),
             size = 0.3) +
  
  facet_wrap(~LGA) +
  
  plot_theme


spread_draws(fit$draws(), mu_wt, gamma, c(b)[LGA]) %>%
  filter(.draw %% 10 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>%
  
  right_join(mobility_data %>% filter(metric == "residential") %>% select(LGA, date, change)) %>%
  #filter(wday(date) != 1, wday(date) != 7) %>% 
  
  mutate(m = (change + 100) / 100,
         beta_0 = mu_wt * (1 - b * m)) %>%
  
  
  mutate(R_0 = beta_0 / gamma) %>%
  
  filter(date == min(date)) %>%
  
  ggplot() +
  
  geom_jitter(aes(x = R_0, y = LGA),
              height = 0.1, size = 0.4) +
  
  plot_theme



spread_draws(fit$draws(), mu_wt, gamma, c(b)[LGA]) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = model_data$LGA_names[LGA]) %>%
  
  right_join(model_data$fit_data_tbl_wt %>% select(LGA, date, mobility), by = c("LGA")) %>%
  filter(wday(date) != 1, wday(date) != 7) %>% 
  filter(date >= model_data$dates_wt[1], date <= last(model_data$dates_wt)) %>% 
  
  mutate(x = mu_wt * (1 - b * mobility) - gamma) %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = x, group = .draw),
            alpha = 0.1,
            size = 0.3) +
  
  facet_wrap(~LGA) +
  
  plot_theme

spread_draws(fit$draws(), b[LGA]) %>%
  #filter(.draw %% 10 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>%
  
  ggplot() +
  
  geom_jitter(aes(x = b, y = LGA),
              alpha = 0.5,
              height = 0.2, size = 0.2) +
  
  plot_theme
