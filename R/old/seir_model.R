
library(tidyverse)
library(lubridate)
library(targets)

source("R/plot_theme.R")
source("R/population.R")


case_data <- tar_read(case_data)
LGAs <- unique(case_data$LGA)

date_period <- c(ymd("2021-11-01"), ymd("2022-03-15"))

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
  
  filter(date <= date_period[2] - days(7)) %>%
  
  filter(LGA == "Melbourne (C)")



source("R/smooth_mobility_data.R")

fit_data <- case_counts %>%
  left_join(pred_data_mobility %>% select(LGA, date, mobility_change = pred_change), by = c("LGA", "date")) %>%
  
  mutate(mobility_change = as.vector(mobility_change),
         mobility_change = mobility_change / 100,
         mobility_change = mobility_change - mobility_change[1],
         mobility_change = mobility_change / sd(mobility_change))

plot(fit_data$mobility_change)

library(cmdstanr)


mod <- cmdstan_model("src/seir.stan")


data_list <- list(
  n_days = nrow(fit_data),
  pop_size = 169860,
  steps_per_day = 10,
  sigma_R_start = 0.07,
  I_start = 130,
  
  mobility = fit_data$mobility_change,
  mu_mobility = -0.25,
  mu_beta_BA2 = 0.5,
  
  mu_beta_variant = 2,
  mu_gamma_variant = 1,
  mu_alpha_variant = 0.2
)

fit <- mod$sample(
  data = data_list,
  seed = 2,
  iter_sampling = 1000,
  chains = 1,
  parallel_chains = 1,
  fixed_param = TRUE
)


draws <- fit$draws()


spread_draws(draws, S_d[d]) %>%
  filter(.draw < 1000) %>% 
  group_by(.draw) %>%
  mutate(incidence =  lag(S_d) - S_d) %>%
  ggplot() +
  geom_line(aes(x = d, y = incidence, group = .draw),
            alpha = 0.1) +
  
  geom_line(aes(x = t, y = n_cases),
            colour = "red",
            case_counts) +
  
  geom_line(aes(x = t, y = n_cases * 4),
            colour = "red",
            case_counts) +
  
  scale_y_log10() +
  coord_cartesian(ylim = c(10, NA))


spread_draws(draws, I_d[d]) %>%
  filter(.draw < 100) %>% 
  ggplot() +
  geom_line(aes(x = d, y = I_d, group = .draw))


mcmc_hist(fit$draws("gamma"), binwidth = 0.01)
