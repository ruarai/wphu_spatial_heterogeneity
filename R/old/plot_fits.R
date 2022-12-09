library(targets)
library(tidyverse)
library(lubridate)

case_data <- tar_read(case_data)
mobility_data <- tar_read(mobility_data)


LGAs <- unique(case_data$LGA)



reff_fit <- tar_read(reff_fit)



all_summ <- map_dfr(
  1:length(reff_fit),
  function(i) {
    reff_fit[[i]]$summarised %>%
      mutate(LGA = LGAs[[i]])
  }
)




date_period <- c(ymd("2020-05-01"), ymd("2020-10-01"))

date_period <- c(ymd("2021-08-01"), ymd("2021-12-15"))

date_period <- c(ymd("2021-12-15"), ymd("2022-02-01"))

date_period <- c(ymd("2022-02-01"), ymd("2022-05-01"))

p1 <- all_summ %>%
  filter(variable == "infections") %>%
  
  left_join(population_LGAs, by = "LGA") %>%
  
  filter(date >= date_period[1], date <= date_period[2]) %>% 
  
  ggplot() +
  
  geom_line(aes(x = date, y = median / pop, colour = LGA)) +
  geom_ribbon(aes(x = date, ymin = lower_90 / pop, ymax = upper_90 / pop, fill = LGA),
              alpha = 0.2) +
  
  coord_cartesian(xlim = c(date_period[1], date_period[2])) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%B", "%d"), sep = "\n")) +
  
  
  
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  
  ggtitle("Incidence") +
  xlab(NULL) +
  ylab(NULL) +
  
  plot_theme

p2 <- all_summ %>%
  filter(variable == "infections") %>%
  
  left_join(population_LGAs, by = "LGA") %>%
  
  filter(date >= date_period[1], date <= date_period[2]) %>% 
  
  group_by(LGA) %>%
  mutate(median = cumsum(median)) %>% 
  
  ggplot() +
  
  geom_line(aes(x = date, y = median / pop, colour = LGA)) +
  
  coord_cartesian(xlim = c(date_period[1], date_period[2])) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%B", "%d"), sep = "\n")) +
  
  
  
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  
  ggtitle("Cumulative incidence") +
  xlab(NULL) +
  ylab(NULL) +
  
  plot_theme



p3 <- mobility_data %>%
  filter(metric %in% c("residential"),
         wday(date) != 1, wday(date) != 7,
         date <= ymd("2022-07-01")) %>%
  
  filter(date >= date_period[1], date <= date_period[2]) %>% 
  # 
  #   group_by(LGA, date = floor_date(date, "week")) %>%
  #   summarise(change = mean(change, na.rm = TRUE)) %>% 
  
  
  ggplot() +
  
  # geom_ribbon(aes(x = date, ymin = lower_90, ymax = upper_90, fill = LGA),
  #             
  #             alpha = 0.2) +
  
  geom_line(aes(x = date, y = change, colour = LGA)) +
  
  coord_cartesian(xlim = c(date_period[1], date_period[2])) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%B", "%d"), sep = "\n")) +
  
  
  
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  
  ggtitle("Time at residential locations") +
  xlab(NULL) +
  ylab(NULL) +
  
  plot_theme



cowplot::plot_grid(
  p2, p3, ncol = 1,
  align = "v", axis = "lr"
)



p4 <- all_summ %>%
  filter(variable == "R") %>%
  
  
  ggplot() +
  
  geom_line(aes(x = date, y = median, colour = LGA)) +
  
  coord_cartesian(xlim = c(date_period[1], date_period[2]),
                  ylim = c(0.5, 2)) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%B", "%d"), sep = "\n")) +
  
  
  
  ggokabeito::scale_colour_okabe_ito() +
  ggokabeito::scale_fill_okabe_ito() +
  
  ggtitle("Effective reproduction number") +
  xlab(NULL) +
  ylab(NULL) +
  
  plot_theme


cowplot::plot_grid(
  p1, p2, p3, p4, ncol = 1,
  align = "v", axis = "lr"
)
