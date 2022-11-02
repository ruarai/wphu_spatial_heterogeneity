

source("R/plot_theme.R")

mobility_data <- tar_read(mobility_data)







date_period <- c(ymd("2021-07-01"), ymd("2022-07-01"))



mobility_fit_data <- mobility_data %>%
  filter(
    date >= date_period[1],
    date <= date_period[2],
    metric == "residential"
  ) %>%
  
  mutate(
    t = as.numeric(date - ymd("2020-01-01")),
    dow = wday(date),
    LGA = factor(LGA),
    weekend = dow == 1 | dow == 7
  )


gam_fit_mobility <- bam(
  change ~ s(t, k = 70, by = LGA) + s(dow, k = 4, by = LGA) + weekend + LGA,
  
  data = mobility_fit_data
)

pred_data_mobility <- expand_grid(
  LGA = LGAs,
  date = seq(date_period[1], date_period[2], "days"),
) %>%
  mutate(
    t = as.numeric(date - ymd("2020-01-01")),
    weekend = wday(date) == 1 | wday(date) == 7,
    dow = if_else(weekend, 1, 3.5)
  ) %>%
  mutate(
    pred_change = predict(gam_fit_mobility, newdata = .)
  ) %>%
  
  left_join(mobility_fit_data %>%
              select(date, LGA, change)) %>%
  filter(date >= date_period[1] + days(60),
         date <= date_period[2] - days(14))




ggplot(pred_data_mobility) +
  
  geom_line(aes(x = date, y = pred_change, group = weekend, colour = weekend)) +
  geom_point(aes(x = date, y = change, colour = weekend)) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  facet_wrap(~LGA) +
  
  plot_theme




ggplot(pred_data_mobility) +
  
  geom_point(aes(x = date, y = (change - pred_change))) +
  
  facet_wrap(~LGA) +
  
  plot_theme




joint_fit_data <- left_join(
  pred_data_mobility %>%
    select(LGA, date, pred_change, change, t),
  
  modelling_data_cases %>%
    select(LGA, date, pred_cases, n_cases, p_cases, diff)
) %>%
  
  mutate(
    diff_change = (change - pred_change),
    LGA = factor(LGA)
  ) %>%
    
  #select(-c(change, pred_change)) %>% 
  
  expand_grid(
    days_before = 0:7
  ) %>%
  group_by(LGA, days_before) %>%
  mutate(change_before = lag(diff_change, n = days_before[1])) %>%
  
  pivot_wider(names_from = days_before, values_from = change_before,
              names_prefix = "days_") %>%
  
  mutate(p_cases_logit = qlogis(p_cases),
         dow = wday(date))




gam_fit_joint <- bam(
  p_cases_logit ~ 
    s(days_0, k = 5) +
    s(days_1, k = 5) + 
    s(days_2, k = 5) + 
    s(days_3, k = 5) + 
    s(days_4, k = 5) + 
    s(days_5, k = 5) + 
    s(days_6, k = 5) + 
    s(days_7, k = 5) +
    LGA,
  
  data = joint_fit_data,
  discrete = TRUE
)

summary(gam_fit_joint)

gratia::draw(gam_fit_joint, rug = FALSE, residuals = TRUE)


joint_fit_data %>%
  ungroup() %>% 
  
  mutate(
    pred_p_cases = plogis(predict(gam_fit_joint, newdata = .))
  ) %>%
  
  ggplot() +
  
  #geom_point(aes(x = p_cases, y = pred_p_cases))
  
  geom_line(aes(x = date, y = pred_p_cases)) +
  geom_point(aes(x = date, y = p_cases)) +
  
  facet_wrap(~LGA, scales = "free_y")


