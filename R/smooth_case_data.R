
library(tidyverse)
library(lubridate)
library(targets)

source("R/plot_theme.R")


case_data <- tar_read(case_data)
LGAs <- unique(case_data$LGA)

#date_period <- c(ymd("2021-11-01"), ymd("2022-07-01"))



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
  n_cases ~ s(t, k = 20, by = LGA) + s(dow, k = 4) + LGA,
  
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
    #dow = wday(date)
    dow = 3.5
    
  ) %>%
  mutate(
    pred_log_cases = predict(gam_fit_cases, newdata = .),
    pred_cases = exp(pred_log_cases),
    
    pred_var = pred_cases + (pred_cases) ^ 2 / theta,
    
    p = (pred_var - pred_cases) / (pred_var),
    
    pred_cases_lower = qnbinom(0.05, size = theta, prob = 1 - p),
    pred_cases_upper = qnbinom(0.95, size = theta, prob = 1 - p)
  )



modelling_data_cases <- pred_data_cases %>%
  filter(date >= date_period[1] + days(14),
         date <= date_period[2] - days(14)) %>%
  
  left_join(
    case_counts %>% select(-date),
    
    by = c("LGA", "t")
  ) %>%
  
  mutate(
    diff = (n_cases - pred_cases) / pred_cases,
    outlier = n_cases > pred_cases_upper | n_cases < pred_cases_lower,
    
    p_cases = pnbinom(n_cases, size = theta, prob = 1 - p)
  )

ggplot(modelling_data_cases) +
  geom_line(aes(x = date, y = pred_cases)) +
  geom_line(aes(x = date, y = pred_cases_upper))  +
  geom_line(aes(x = date, y = pred_cases_lower)) +
  
  geom_point(aes(x = date, y = n_cases),
             colour = ggokabeito::palette_okabe_ito(5),
             size = 0.6) +
  
  scale_y_log10(limits = c(1, NA)) +
  
  facet_wrap(~LGA) +
  
  xlab("Date") + ylab("Count") +
  
  ggtitle("Case incidence") +
  
  plot_theme

