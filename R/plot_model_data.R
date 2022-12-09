

source("R/plot_theme.R")

model_data <- tar_read(model_data)


plot_mobility <- function(fit_data) {
  ggplot(fit_data) +
    
    geom_step(aes(x = date, y = (change + 100) / 100),
              size = 0.6) +
    
    geom_line(aes(x = date, y = mobility),
              
              size = 1,
              colour = ggokabeito::palette_okabe_ito(5)) +
    
    facet_wrap(~LGA, ncol = 3) +
    
    coord_cartesian(ylim = c(0.95, 1.4)) +
    
    scale_x_date(breaks = seq(ymd("2020-01-01"), ymd("2022-07-01"), by = "1 months"),
                 labels = scales::label_date_short(format = c("%Y", "%b", "%d"), sep = "\n")) +
    
    xlab(NULL) + 
    ylab("Change") +
    
    plot_theme
}
  

model_data$fit_data_tbl_wt %>%
  plot_mobility() +
  ggtitle("Increase in time spent in residence \u2012 Second wave epidemic")


model_data$fit_data_tbl_delta %>%
  plot_mobility() +
  ggtitle("Increase in time spent in residence \u2012 Delta epidemic")


plot_cases <- function(fit_data) {
  fit_data %>%
    
    ggplot() +
    
    geom_point(aes(x = date, y = n_cases),
               size = 0.6) +
    
    scale_y_log10(labels = scales::label_comma()) +
    
    xlab(NULL) + ylab("Count") +
    
    facet_wrap(~LGA, ncol = 3) +
    
    scale_x_date(breaks = seq(ymd("2020-01-01"), ymd("2022-07-01"), by = "1 months"),
                 labels = scales::label_date_short(format = c("%Y", "%b", "%d"), sep = "\n")) +
    
    coord_cartesian(ylim = c(0.5, NA)) +
    
    plot_theme
}


model_data$fit_data_tbl_wt %>%
  plot_cases() +
  ggtitle("Case incidence by symptom onset \u2012 Second wave epidemic")

model_data$fit_data_tbl_delta %>%
  plot_cases() +
  ggtitle("Case incidence by symptom onset \u2012 Delta epidemic")



model_data$fit_data_tbl_delta %>%
  ggplot() +
  
  geom_line(aes(x = date, y = p_second_dose, colour = LGA),
            
            size = 1) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  geom_vline(xintercept = ymd("2021-08-27"), linetype = "dashed") +
  geom_vline(xintercept = ymd("2021-08-01"), linetype = "dashed") +
  
  scale_x_date(breaks = seq(ymd("2020-01-01"), ymd("2022-07-01"), by = "1 months"),
               labels = scales::label_date_short(format = c("%Y", "%b", "%d"), sep = "\n")) +
  
  xlab(NULL) + 
  ylab("Proportion vaccinated") +
  ggtitle("Proportion of individuals in LGA with second dose of vaccine \u2012 Delta epidemic") +
  
  plot_theme
  

