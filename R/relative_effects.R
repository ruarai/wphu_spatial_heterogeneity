


date_period <- c(ymd("2021-11-01"), ymd("2022-07-01"))

derivatives <- gratia::derivatives(
  gam_fit_cases,
  terms = "s(t)",
  newdata = expand_grid(dow = 3, LGA = LGAs, t = case_counts$t %>% unique()),
  partial_match = TRUE,
  n = 4000,
  ncores = 16
)

derivatives_mobility <- gratia::derivatives(
  gam_fit_mobility,
  terms = "s(t)",
  newdata = expand_grid(weekend = FALSE, dow = 3, LGA = LGAs, t = mobility_fit_data$t %>% unique()),
  partial_match = TRUE,
  n = 4000,
  ncores = 16
)


case_growth <- derivatives %>%
  mutate(LGA = str_remove(smooth, "s\\(t\\)\\:LGA"),
         date = ymd("2020-01-01") + days(floor(data))) %>%
  
  select(LGA, date, derivative) %>%
  
  filter(date >= date_period[1], date <= date_period[2])

relative_growth <- case_growth %>%
  
  select(LGA, date, derivative) %>%
  
  group_by(date = as_date(date)) %>%
  
  mutate(
    rel_growth = derivative - derivative[LGA == "Melbourne (C)"]
    #rel_growth = derivative - mean(derivative)
  ) %>%
  
  group_by(LGA) %>%
  
  mutate(cumulative_growth = cumsum(rel_growth))



p1 <- ggplot(relative_growth) +
  
  geom_line(aes(x = date, y = rel_growth, colour = LGA)) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  coord_cartesian(xlim = date_period) +
  
  plot_theme




relative_mobility <- derivatives_mobility %>%
  mutate(LGA = str_remove(smooth, "s\\(t\\)\\:LGA"),
         date = ymd("2020-01-01") + days(floor(data))) %>%
  
  select(LGA, date, derivative) %>%
  
  filter(date >= date_period[1], date <= date_period[2]) %>%
  
  select(LGA, date, derivative) %>%
  
  group_by(date = as_date(date)) %>%
  
  mutate(
    rel_change = derivative - derivative[LGA == "Melbourne (C)"]
    #rel_change = derivative - mean(derivative)
  )




# relative_mobility <- pred_data_mobility %>%
#   filter(!weekend) %>% 
#   select(LGA, date, pred_change)  %>%
#   
#   group_by(LGA) %>%
#   mutate(pred_change = pred_change - mean(pred_change)) %>% 
#   
#   group_by(date = as_date(date)) %>%
#   
#   mutate(#pred_change = log(pred_change / 100 + 1),
#          rel_change = pred_change - pred_change[LGA == "Melbourne (C)"])




p2 <- ggplot(relative_mobility) +
  
  geom_line(aes(x = date, y = -rel_change, colour = LGA)) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  coord_cartesian(xlim = date_period) +
  
  plot_theme

cowplot::plot_grid(
  p2, p1, ncol = 1
)



relative_mobility %>%
  select(-derivative) %>% 
  left_join(relative_growth) %>%
  filter(date >= ymd("2021-11-15")) %>% 
  
  group_by(LGA) %>%
  arrange(date) %>%
  mutate(rel_growth = lead(rel_growth, 6)) %>% 
  
  ggplot() +
  
  geom_point(aes(x = rel_change, y = rel_growth))
  
  geom_line(aes(x = date, y = rel_growth, colour = LGA)) +
  
  geom_line(aes(x = date, y = -rel_change / 5, colour = LGA),
            linetype = "dotted") +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  plot_theme

