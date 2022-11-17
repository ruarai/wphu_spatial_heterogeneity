

source("R/plot_theme.R")

mobility_data <- tar_read(mobility_data)




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


require(mgcv)
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
    #weekend = wday(date) == 1 | wday(date) == 7,
    #dow = if_else(weekend, 1, 3.5)
    
    weekend = FALSE,
    dow = 3.5
  ) %>%
  mutate(
    pred_change = predict(gam_fit_mobility, newdata = .)
  ) %>%
  
  left_join(mobility_fit_data %>%
              select(date, LGA, change))




ggplot() +
  geom_point(aes(x = date, y = change, colour = weekend),
             pred_data_mobility %>% mutate(weekend = wday(date) == 1 | wday(date) == 7)) +
  
  geom_line(aes(x = date, y = pred_change, group = weekend),
            pred_data_mobility) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  facet_wrap(~LGA) +
  
  xlab("Date") + ylab("Count") +
  
  plot_theme +
  
  theme(legend.position = "none")


