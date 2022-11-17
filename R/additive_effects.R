
source("R/population.R")

date_to_t <- function(date) { as.numeric(date - ymd("2020-01-01")) }

derivatives_cases <- gratia::derivatives(
  gam_fit_cases,
  order = 1,
  terms = "s(t)",
  newdata = expand_grid(dow = 3, LGA = LGAs, t = case_counts$t %>% unique()),
  partial_match = TRUE,
  n = 12000,
  ncores = 16
)



case_growth <- derivatives_cases %>%
  mutate(LGA = str_remove(smooth, "s\\(t\\)\\:LGA"),
         date = ymd("2020-01-01") + days(floor(data))) %>%
  
  select(LGA, date, growth_rate = derivative) %>%
  
  filter(date >= date_period[1], date <= date_period[2] - days(14))



case_growth %>%
  
  ggplot() +
  geom_line(aes(x = date, y = growth_rate)) +
  
  facet_wrap(~LGA) +
  
  plot_theme



immunity_fn <- function(days_before) {
  plogis((2.2 * 100 - days_before) * 0.31)
}

tibble(days_before = 1:360) %>%
  mutate(relative_effect = immunity_fn(days_before)) %>% 
  ggplot() +
  geom_line(aes(x = days_before, y = relative_effect))

pred_immunity <- case_counts %>%
  select(LGA, date, n_cases) %>%
  
  left_join(population_LGAs, by = "LGA") %>%
  mutate(
    n_cases = n_cases / pop,
    n_cases = if_else(date >= ymd("2021-12-01") & date <= ymd("2022-02-01"), n_cases * 3, n_cases)
  ) %>% 
  
  expand_grid(
    days_before = 1:360
  ) %>%
  
  mutate(relative_effect = immunity_fn(days_before)) %>% 
  
  group_by(LGA, days_before) %>%
  mutate(n_cases_before = lag(n_cases, days_before[1])) %>%
  
  ungroup() %>%
  filter(date - days(days_before) >= date_period[1]) %>%
  
  group_by(LGA, date) %>%
  summarise(total_immunity = sum(n_cases_before * relative_effect))


ggplot(pred_immunity) +
  geom_line(aes(x = date, y = total_immunity)) +
  facet_wrap(~LGA)


date_BA1 <- ymd("2021-12-20")
date_BA2 <- ymd("2022-02-15")

pred_variants <- tibble(
  date = seq(date_period[1], date_period[2], "days")
) %>%
  mutate(
    t = date_to_t(date),
    p_BA1 = 1 - plogis( (date_to_t(date_BA1) - t) * 0.3),
    p_BA2 = 1 - plogis( (date_to_t(date_BA2) - t) * 0.1)
  ) %>%
  select(-t)

ggplot(pred_variants) +
  geom_line(aes(x = date, y = p_BA1)) +
  geom_line(aes(x = date, y = p_BA2))




fit_data <- case_growth %>%
  left_join(pred_immunity, by = c("date", "LGA")) %>%
  left_join(pred_variants, by = c("date")) %>%
  left_join(
    pred_data_mobility %>% select(LGA, date, pred_mobility = pred_change),
    by = c("date", "LGA")
  ) %>%
  
  drop_na(total_immunity, pred_mobility) %>%
  
  mutate(
    t = date_to_t(date),
    LGA = factor(LGA),
  )


gam_fit <- gamm(
  growth_rate ~ total_immunity + 
    p_BA1 + 
    p_BA2 +
    s(pred_mobility, k = 3),
  
  correlation = corExp(form = ~ t | LGA),
  
  data = fit_data
)


gratia::draw(gam_fit$gam)


fit_data %>%
  
  mutate(pred_growth = predict(gam_fit$gam, newdata = .)) %>%
  
  mutate(
    #total_immunity = 0
    pred_mobility = 0
  ) %>% 
  
  mutate(pred_growth_comp = predict(gam_fit$gam, newdata = .)) %>%
  
  ggplot() +
  
  geom_line(aes(x = date, y = pred_growth)) +
  
  geom_line(aes(x = date, y = pred_growth_comp),
            colour = ggokabeito::palette_okabe_ito(5)) +
  
  geom_point(aes(x = date, y = growth_rate),
             size = 0.2) +
  
  facet_wrap(~LGA) +
  
  plot_theme




