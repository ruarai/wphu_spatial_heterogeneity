


plot_case_data <- function(case_data) {
  
  case_data %>%
    count(LGA, date_specimen,
          name = "n_cases") %>%
    
    ggplot() +
    
    geom_rect(aes(xmin = date_start, xmax = date_end, ymin = 0, ymax = Inf),
              periods %>% filter(alternate_coding == 1),
              alpha = 0.4,
              fill = "lightblue") +
    
    geom_linerange(aes(x = date_specimen, ymin = 0, ymax = n_cases)) +
    
    scale_y_log10(labels = scales::label_comma()) +
    
    xlab("Date (specimen)") + ylab("Count") +
    
    facet_wrap(~LGA, ncol = 1) +
    
    scale_x_date(breaks = seq(ymd("2020-01-01"), ymd("2022-07-01"), by = "3 months"),
                 labels = scales::label_date_short(format = c("%Y", "%B", "%d"), sep = "\n")) +
    
    plot_theme
    
  
  ggsave(
    "results/case_counts_by_LGA.png",
    width = 10,
    height = 8,
    bg = "white"
  )
    
  
}