

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





i_LGA <- "Brimbank (C)"

fit_data <- modelling_data_cases %>%
  select(LGA, date, pred_cases) %>%
  left_join(population_LGAs, by = "LGA") %>% 
  left_join(model_data_mobility, by = c("LGA", "date")) %>%
  filter(LGA == i_LGA)


fit_data  %>% 
  
  group_by(LGA) %>% 
  
  mutate(inf = zoo::rollsum(pred_cases, k = 14, fill = 0, align = "r"),
         sus = pop - cumsum(pred_cases),
         x = (sus - inf) / pop) %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = x * (1 / mobility) - 0.8)) +
  
  geom_line(aes(x = date, y = growth_rate),
            colour = "blue",
            plot_data_growth) +
  
  facet_wrap(~LGA) +
  plot_theme

mod <- cmdstan_model("src/growth_rate.stan")


data_list <- list(
  N = nrow(fit_data),
  t = fit_data$pred_cases,
  
  n_cases = fit_data$pred_cases,
  mobility = fit_data$mobility,
  pop_size = population_LGAs %>% filter(LGA == "Melbourne (C)") %>% pull(pop),
  
  theta = 20,
  sigma_R_start = 0.1
)



fit <- mod$sample(
  data = data_list, 
  seed = 5, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 100
)


fit <- mod$optimize(
  data = data_list, 
  seed = 2, 
)

fit$draws(c("mu_mobility", "beta_0", "gamma", "n_cases_sim[75]")) %>%
  mcmc_pairs()

fit$draws(c("mu_mobility", "beta_0", "gamma")) %>%
  mcmc_hist()


spread_draws(fit$draws(), n_cases_sim[t]) %>%
  filter(.draw %% 100 == 1) %>% 
  ggplot() +
  geom_line(aes(x = t, y = n_cases_sim, group = .draw)) +
  
  geom_line(aes(x = 1:nrow(fit_data), y = n_cases), colour = "red",
            modelling_data_cases %>% filter(LGA == i_LGA)) +
  
  coord_cartesian(ylim = c(0, 100))



spread_draws(fit$draws(), growth_rate[t]) %>%
  filter(.draw %% 100 == 1) %>% 
  ggplot() +
  geom_line(aes(x = t, y = growth_rate, group = .draw)) +
  
  coord_cartesian(ylim = c(-0.5, 1))

