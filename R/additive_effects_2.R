
library(tidyverse)
library(lubridate)
library(targets)

source("R/plot_theme.R")
source("R/population.R")


case_data <- tar_read(case_data)
LGAs <- unique(case_data$LGA)

date_period <- c(ymd("2021-11-01"), ymd("2022-05-15"))

date_to_t <- function(date) { as.numeric(date - date_period[1]) }


case_counts <- case_data %>%
  filter(date_specimen >= date_period[1],
         date_specimen <= date_period[2]) %>% 
  
  mutate(date_symptom_onset = if_else(is.na(date_symptom_onset), date_specimen - days(4), date_symptom_onset)) %>%
  filter(date_symptom_onset >= date_period[1],
         date_symptom_onset <= date_period[2]) %>% 
  count(LGA, date = date_symptom_onset,
        name = "n_cases") %>%
  complete(
    LGA,
    date = seq(date_period[1], date_period[2], "days"),
    fill = list(n_cases = 0)
  ) %>% 
  
  mutate(t = as.numeric(date - date_period[1]),
         LGA = factor(LGA),
         dow = wday(date)) %>%
  
  mutate(
    n_cases = if_else(date >= ymd("2021-12-15") & date <= ymd("2022-01-20"),
                      as.integer(n_cases * 3),
                      n_cases)
  ) %>%
  
  filter(date <= date_period[2] - days(7)) %>%
  
  filter(LGA == "Melbourne (C)")



ggplot(case_counts) +
  geom_point(aes(x = date, y = n_cases),
             colour = ggokabeito::palette_okabe_ito(5),
             size = 0.6) +
  
  scale_y_log10(limits = c(1, NA)) +
  coord_cartesian(ylim = c(10, NA)) +
  
  facet_wrap(~LGA) +
  
  xlab("Date") + ylab("Count") +
  
  ggtitle("Case incidence") +
  
  plot_theme


fit_data <- case_counts %>%
  left_join(pred_data_mobility %>% select(LGA, date, mobility_change = pred_change), by = c("LGA", "date"))


mod <- cmdstan_model("src/additive_effect.stan")


data_list <- list(
  N = nrow(fit_data),
  
  n_cases = fit_data$n_cases,
  t = fit_data$t,
  mobility_raw = as.vector(fit_data$mobility_change),
  population_size = population_LGAs %>% filter(LGA == "Melbourne (C)") %>% pull(pop),
  
  theta = 25
)

fit <- mod$sample(
  data = data_list, 
  seed = 6, 
  chains = 4, 
  parallel_chains = 4,
  refresh = 100
)

fit <- mod$optimize(
  data = data_list,
  seed = 123
)

c("mu", "mu_BA1", "mu_BA2", "mu_mobility", "mu_immunity", "slope_immunity", "holiday_ascertainment",
  "duration_immunity", "t_BA1", "t_BA2") %>%
  fit$draws() %>%
  mcmc_pairs()

library(bayesplot)

mcmc_hist(fit$draws("n_cases_sim[150]"), binwidth = 10)
mcmc_hist(fit$draws("mu"), binwidth = 0.001)
mcmc_hist(fit$draws("mu_BA1"), binwidth = 0.01)
mcmc_hist(fit$draws("mu_BA2"), binwidth = 0.001)
mcmc_hist(fit$draws("mu_mobility"), binwidth = 0.001)
mcmc_hist(fit$draws("mu_immunity"), binwidth = 0.01)
mcmc_hist(fit$draws("duration_immunity") * 100 + 50, binwidth = 10)
mcmc_hist(fit$draws("slope_immunity"), binwidth = 0.01)
mcmc_hist(1 / (1 + fit$draws("holiday_ascertainment")), binwidth = 0.01)

mcmc_parcoord(fit$draws())


spread_draws(fit$draws(), n_cases_sim[t]) %>%
  filter(.draw < 100) %>% 
  ggplot() +
  geom_line(aes(x = t, y = n_cases_sim, group = .draw)) +
  
  geom_line(aes(x = t, y = n_cases), colour = "red",
            case_counts) +
  
  scale_y_log10() +
  
  coord_cartesian(ylim = c(10, 6000))



spread_draws(fit$draws(), growth_rate[t]) %>%
  filter(.draw %% 100 == 1) %>% 
  ggplot() +
  geom_line(aes(x = t, y = growth_rate, group = .draw)) +
  
  coord_cartesian(ylim = c(-0.5, 1))

