

source("R/population.R")

date_period <- c(ymd("2020-05-25"), ymd("2020-10-15"))

source("R/smooth_case_data.R")

plot_data_growth <- modelling_data_cases %>%
  group_by(LGA) %>%
  arrange(date) %>% 
  mutate(growth_rate = pred_log_cases - lag(pred_log_cases))


ggplot(plot_data_growth) +
  geom_line(aes(x = date, y = growth_rate, colour = LGA)) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  xlab("Date") + ylab("Growth rate") +
  
  plot_theme

source("R/smooth_mobility_data.R")

model_data_mobility <- pred_data_mobility %>%
  select(LGA, date, mobility = pred_change) %>%
  mutate(mobility = as.vector(mobility),
         mobility = (mobility + 100) / 100 )



fit_data <- case_counts %>%
  select(LGA, date, n_cases) %>%
  left_join(population_LGAs, by = "LGA") %>% 
  left_join(model_data_mobility, by = c("LGA", "date")) %>%
  
  mutate(t = as.numeric(date - date_period[1]) + 1) %>%
  
  arrange(LGA, t)

fit_data  %>% 
  mutate(
    a = case_when(
      LGA == "Melton (C)" ~ 0.9,
      LGA == "Wyndham (C)" ~ 1.3,
      TRUE ~ 1
    ),
    b = case_when(
      LGA == "Wyndham (C)" ~ 0.8,
      TRUE ~ 0.5
    )
  ) %>% 
  
  ggplot() +
  #geom_line(aes(x = date, y = 1.5 * (a - b * mobility) - 0.5 )) +
  
  geom_line(aes(x = date, y = growth_rate),
            colour = "blue",
            plot_data_growth) +
  
  facet_wrap(~LGA) +
  plot_theme

mod <- cmdstan_model("src/growth_rate.stan")


fit_data_n_cases <- fit_data %>%
  select(date, LGA, n_cases) %>%
  pivot_wider(names_from = LGA, values_from = n_cases) %>%
  select(-date) %>%
  as.matrix() %>%
  t()

fit_data_mobility <- fit_data %>%
  select(date, LGA, mobility) %>%
  pivot_wider(names_from = LGA, values_from = mobility) %>%
  select(-date) %>%
  as.matrix() %>%
  t()

stopifnot(rownames(fit_data_n_cases) == rownames(fit_data_mobility))

LGA_names <- rownames(fit_data_n_cases)

data_list <- list(
  t_max = ncol(fit_data_n_cases),
  n_pops = nrow(fit_data_n_cases),
  
  n_cases = fit_data_n_cases,
  mobility = fit_data_mobility,
  
  nb_theta = 70
)


# Fix max tree depth
fit <- mod$sample(
  data = data_list, 
  seed = 5, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 100,
  iter_warmup = 1000,
  iter_sampling = 1000
)


fit <- mod$optimize(
  data = data_list,
  seed = 2,
)

fit$draws(c("a[1]", "b[1]", "I_log_0[1]", "n_cases_sim[1,75]")) %>%
  mcmc_pairs()
fit$draws(c("a[1]", "b", "I_log_0[1]", "n_cases_sim[1,75]")) %>%
  mcmc_pairs()

fit$draws(c("a[1]", "a[2]", "a[3]", "b[1]", "b[2]", "b[3]")) %>%
  mcmc_pairs()

fit$draws(c("a[1]", "b[1]")) %>%
  mcmc_hist()


spread_draws(fit$draws(), n_cases_sim[LGA][t]) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>% 
  ggplot() +
  geom_line(aes(x = t, y = n_cases_sim, group = .draw),
            alpha = 0.2) +
  
  facet_wrap(~LGA) +
  
  geom_point(aes(x = t, y = n_cases), colour = "red",
             fit_data) +
  plot_theme


spread_draws(fit$draws(), growth_rate[LGA][t]) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>% 
  ggplot() +
  geom_line(aes(x = t, y = growth_rate, group = interaction(.draw, LGA), colour = LGA)) +
  
  # geom_line(aes(x = t, y = growth_rate), colour = "red",
  #           fit_data) +
  
  coord_cartesian(ylim = c(-0.3, 0.3)) +
  plot_theme


spread_draws(fit$draws(), log_infections_t[LGA][t]) %>%
  filter(.draw %% 100 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>% 
  ggplot() +
  
  facet_wrap(~LGA) +
  geom_line(aes(x = t, y = log_infections_t, group = .draw)) +
  
  geom_point(aes(x = t, y = log(n_cases)), fit_data, colour = "red")  +
  plot_theme +
  
  coord_cartesian(ylim = c(-1, NA))

spread_draws(fit$draws(), n_cases_sim[LGA][t]) %>%
  filter(.draw %% 250 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>% 
  ggplot() +
  geom_line(aes(x = t, y = n_cases_sim, group = .draw),
            alpha = 0.2) +
  
  facet_wrap(~LGA) +

  geom_point(aes(x = t, y = n_cases), colour = "red",
             fit_data) +
  
  scale_y_log10() +
  plot_theme


spread_draws(fit$draws(), c(a)[LGA]) %>%
  filter(.draw %% 4 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>%
  
  ggplot() +
  geom_point(aes(y = LGA, x = a),
             position = position_jitter(height = 0.3),
             size = 0.3) +
  
  plot_theme

spread_draws(fit$draws(), c(a, b)[LGA]) %>%
  filter(.draw %% 4 == 1) %>% 
  mutate(LGA = LGA_names[LGA]) %>%
  
  ggplot() +
  geom_point(aes(x = a, y = b, colour = LGA),
             size = 0.3) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  plot_theme

