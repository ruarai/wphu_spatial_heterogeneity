
library(tidyverse)
library(lubridate)
library(targets)

source("R/plot_theme.R")


case_data <- tar_read(case_data)
LGAs <- unique(case_data$LGA)

date_period <- c(first(model_data$dates_wt), last(model_data$dates_wt))


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
  
  mutate(t = as.numeric(date - ymd("2020-01-01")),
         LGA = factor(LGA),
         dow = wday(date))


library(mgcv)

theta <- 25

gam_fit_cases <- bam(
  n_cases ~ s(t, k = 20, by = LGA) + LGA,
  
  data = case_counts,
  family = nb(theta = 70),
  discrete = TRUE
)

summary(gam_fit_cases)

pred_data_cases <- expand_grid(
  LGA = LGAs,
  date = seq(date_period[1], date_period[2], "days"),
) %>%
  mutate(
    t = as.numeric(date - ymd("2020-01-01")),
    dow = 3.5,
    row = row_number()
  ) %>%
  left_join(
    gratia::fitted_samples(gam_fit_cases, n = 500, seed = 0, newdata = ., scale = "linear_predictor"),
    by = "row"
  ) %>%
  select(LGA, date, draw, pred_log_cases = fitted) %>% 
  group_by(draw, LGA) %>%
  arrange(date) %>%
  mutate(growth_rate = pred_log_cases - lag(pred_log_cases))


ggplot(pred_data_cases) +
  geom_line(aes(x = date, y = growth_rate, colour = LGA, group = interaction(LGA, draw)))




pred_data_cases %>%
  ungroup() %>%
  filter(draw == 1, LGA == "Maribyrnong (C)") %>%
  drop_na(growth_rate) %>%
  mutate(G = growth_rate + 0.369,
         logG = log(G)) %>%
  
  select(-draw) %>% 
  
  left_join(model_data$fit_data_tbl_wt, by = c("LGA", "date")) %>%
  
  mutate(log_m = log(mobility)) %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = logG)) +
  
  geom_line(aes(x = date, y = 0.83 - 8 * log_m))






