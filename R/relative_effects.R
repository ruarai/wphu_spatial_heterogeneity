


#date_period <- c(ymd("2020-05-01"), ymd("2020-10-01"))

derivatives_cases <- gratia::derivatives(
  gam_fit_cases,
  order = 1,
  terms = "s(t)",
  newdata = expand_grid(dow = 3, LGA = LGAs, t = case_counts$t %>% unique()),
  partial_match = TRUE,
  n = 12000,
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


case_growth <- derivatives_cases %>%
  mutate(LGA = str_remove(smooth, "s\\(t\\)\\:LGA"),
         date = ymd("2020-01-01") + days(floor(data))) %>%
  
  select(LGA, date, derivative) %>%
  
  arrange(date) %>%
  group_by(LGA) %>%
  mutate(derivative = derivative - lag(derivative)) %>% 
  
  filter(date >= date_period[1], date <= date_period[2])


ggplot(case_growth) +
  
  geom_line(aes(x = date, y = derivative, colour = LGA)) +
  
  plot_theme +
  
  ggtitle("Case incidence derivative (growth rate)")+
  
  ggokabeito::scale_colour_okabe_ito()

relative_growth <- case_growth %>%
  
  select(LGA, date, derivative) %>%
  
  group_by(date = as_date(date)) %>%
  
  mutate(
    rel_growth = derivative - derivative[LGA == "Melbourne (C)"]
  ) %>%
  
  group_by(LGA) %>%
  
  mutate(cumulative_growth = cumsum(rel_growth))



p_case_rel_growth <- ggplot(relative_growth) +
  
  geom_line(aes(x = date + days(2), y = rel_growth, colour = LGA)) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  coord_cartesian(xlim = date_period) +
  
  plot_theme




relative_mobility <- derivatives_mobility %>%
  mutate(LGA = str_remove(smooth, "s\\(t\\)\\:LGA"),
         date = ymd("2020-01-01") + days(floor(data))) %>%
  
  select(LGA, date, derivative) %>%
  
  filter(date >= date_period[1], date <= date_period[2]) %>%
  
  group_by(date = as_date(date)) %>%
  
  mutate(
    rel_change = derivative - derivative[LGA == "Melbourne (C)"]
  )


ggplot(relative_mobility) +
  geom_line(aes(x = date, y = derivative, colour = LGA)) +
  
  plot_theme +
  
  ggtitle("Mobility derivative (change in time spent at home)") +
  
  ggokabeito::scale_colour_okabe_ito()

p_mobility_rel_change <- ggplot(relative_mobility) +
  
  geom_line(aes(x = date, y = -rel_change, colour = LGA)) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  coord_cartesian(xlim = date_period) +
  
  plot_theme





cowplot::plot_grid(
  p_mobility_rel_change,
  p_case_rel_growth,
  ncol = 1
)





relative_mobility %>%
  select(-derivative) %>% 
  left_join(relative_growth) %>%
  filter(LGA != "Melbourne (C)") %>%
  
  group_by(LGA) %>%
  arrange(date) %>%
  mutate(lead_rel_growth = lag(rel_growth, 2)) %>%
  
  ggplot(aes(x = rel_change, y = -lead_rel_growth)) +
  
  geom_point(size = 0.4) +
  geom_smooth(method = "lm") +
  
  plot_theme

relative_mobility %>%
  select(-derivative) %>% 
  left_join(relative_growth) %>%
  filter(LGA != "Melbourne (C)") %>%
  
  group_by(LGA) %>%
  arrange(date) %>%
  mutate(lead_rel_growth = lag(rel_growth, 2)) %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = rel_change / 6, group = LGA),
            colour = "green") +
  geom_line(aes(x = date, y = -lead_rel_growth, group = LGA)) +
  
  plot_theme
  
  
  

relative_mobility %>%
  mutate(date = as_date(date)) %>% 
  filter(LGA != "Melbourne (C)") %>% 
  select(-derivative) %>% 
  left_join(relative_growth %>% mutate(date = as_date(date))) %>%
  
  group_by(LGA) %>%
  arrange(date) %>%

  mutate(rel_growth = rel_growth < 0,
         rel_change = rel_change > 0) %>%
  
  ggplot() +
  geom_tile(aes(x = date + days(2), y = 0, fill = rel_growth)) +
  geom_tile(aes(x = date, y = 1, fill = rel_change)) +
  
  scale_fill_brewer(type = "qual", palette = 6) +
  
  facet_wrap(~LGA)
