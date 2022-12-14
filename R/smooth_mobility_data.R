


smooth_mobility_data <- function(mobility_data, date_period) {
  
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
      
      weekend = FALSE,
      dow = 3.5
    ) %>%
    mutate(
      mobility = predict(gam_fit_mobility, newdata = .),
      mobility = (mobility + 100) / 100
    ) %>%
    
    left_join(mobility_fit_data %>%
                select(date, LGA, change))
  
  pred_data_mobility
}



