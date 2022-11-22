

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





i_LGA <- "Wyndham (C)"

fit_data <- modelling_data_cases %>%
  select(LGA, date, pred_cases) %>%
  left_join(population_LGAs, by = "LGA") %>% 
  left_join(model_data_mobility, by = c("LGA", "date")) %>%
  filter(LGA == i_LGA)

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
  geom_line(aes(x = date, y = 1.5 * (a - b * mobility) - 0.5 )) +
  
  geom_line(aes(x = date, y = growth_rate),
            colour = "blue",
            plot_data_growth) +
  
  facet_wrap(~LGA) +
  plot_theme

mod <- cmdstan_model("src/growth_rate.stan")


data_list <- list(
  N = nrow(fit_data),
  
  n_cases = fit_data$pred_cases,
  mobility = fit_data$mobility,
  
  theta = 70,
  sigma_R_start = 0.3,
  R_0_center = 1.5
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


# fit <- mod$optimize(
#   data = data_list, 
#   seed = 2, 
# )

fit$draws(c("a", "b", "gamma", "I_log_0", "n_cases_sim[75]")) %>%
  mcmc_pairs()

fit$draws(c("a", "b", "gamma")) %>%
  mcmc_hist()


spread_draws(fit$draws(), n_cases_sim[t]) %>%
  filter(.draw %% 100 == 1) %>% 
  ggplot() +
  geom_line(aes(x = t, y = n_cases_sim, group = .draw),
            alpha = 0.2) +
  
  geom_line(aes(x = 1:nrow(fit_data), y = n_cases), colour = "red",
            modelling_data_cases %>% filter(LGA == i_LGA)) +
  plot_theme



spread_draws(fit$draws(), growth_rate[t]) %>%
  filter(.draw %% 100 == 1) %>% 
  ggplot() +
  geom_line(aes(x = t, y = growth_rate, group = .draw)) +
  
  geom_line(aes(x = 1:nrow(fit_data), y = growth_rate), colour = "red",
            plot_data_growth %>% filter(LGA == i_LGA)) +
  
  coord_cartesian(ylim = c(-0.5, 1)) +
  plot_theme


spread_draws(fit$draws(), log_infections_t[t]) %>%
  filter(.draw %% 100 == 1) %>% 
  ggplot() +
  geom_line(aes(x = t, y = log_infections_t, group = .draw)) +
  
  geom_point(aes(x = 1:nrow(fit_data), y = log(n_cases) ), colour = "red",
            modelling_data_cases %>% filter(LGA == i_LGA))  +
  plot_theme

spread_draws(fit$draws(), n_cases_sim[t]) %>%
  filter(.draw %% 100 == 1) %>% 
  ggplot() +
  geom_line(aes(x = t, y = n_cases_sim, group = .draw),
            alpha = 0.2) +
  
  geom_point(aes(x = 1:nrow(fit_data), y = n_cases), colour = "red",
             modelling_data_cases %>% filter(LGA == i_LGA)) +
  
  scale_y_log10() +
  plot_theme

