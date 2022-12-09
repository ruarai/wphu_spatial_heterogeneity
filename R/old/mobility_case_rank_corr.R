



source("R/plot_theme.R")
source("R/population.R")

mobility_data <- tar_read(mobility_data)
case_data <- tar_read(case_data)


period <- c(ymd("2021-12-01"), ymd("2022-07-01"))


mobility_plot_data <- mobility_data %>%
  filter(metric == "residential",
         date >= period[1], date <= period[2],
         wday(date) != 1, wday(date) != 7)

p_mobility <- ggplot(mobility_plot_data) +
  geom_line(aes(x = date, y = change, colour = LGA)) +
  
  plot_theme +
  
  xlab(NULL) + ylab(NULL) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  ggtitle("Change in time spent at home") +
  
  theme(legend.position = "none")



case_plot_data <- case_data %>%
  count(date = date_specimen, LGA) %>%
  filter(date >= period[1], date <= period[2]) %>%
  
  complete(date = seq(period[1], period[2], "days"),
           LGA,
           fill = list(n = 0)) %>%
  
  group_by(LGA) %>%
  arrange(date) %>% 
  mutate(n = cumsum(n)) %>%
  
  left_join(population_LGAs, by = "LGA") %>%
  mutate(n = n / pop)

p_case <- ggplot(case_plot_data) +
  geom_line(aes(x = date, y = n, colour = LGA)) +
  
  plot_theme +
  
  xlab(NULL) + ylab(NULL) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  ggtitle("Cumulative case incidence (proportion of population)") +
  
  theme(legend.position = "none")

p_legend <- cowplot::get_legend(
  p_case + theme(legend.position = "bottom")
) 


cowplot::plot_grid(
  p_mobility, p_case, p_legend,
  rel_heights = c(3, 3, 1),
  ncol = 1, align = "v", axis = "lr"
)


case_cumulative_incidence <- case_plot_data %>%
  ungroup() %>% 
  filter(date == max(date))


mean_mobility_change <- mobility_plot_data %>% 
  group_by(LGA) %>% 
  summarise(mean_change = mean(change, na.rm = TRUE))


plot_data_cor <- case_cumulative_incidence %>%
  left_join(mean_mobility_change, by = "LGA")

ggplot(plot_data_cor) +
  geom_point(aes(x = mean_change, y = n, colour = LGA),
             size = 2) +
  
  xlab("Average change in time at home (% increase)") +
  ylab("Cumulative case incidence") +
  
  ggokabeito::scale_colour_okabe_ito()  +
  
  coord_cartesian(ylim = c(0, NA), xlim = c(5, NA)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.3))) +
  
  theme_minimal() +
  theme(legend.position = "bottom")


cor.test(
  case_cumulative_incidence$n,
  mean_mobility_change$mean_change,
  method = "pearson"
)


cor.test(
  case_cumulative_incidence$n,
  mean_mobility_change$mean_change,
  method = "spearman"
)









