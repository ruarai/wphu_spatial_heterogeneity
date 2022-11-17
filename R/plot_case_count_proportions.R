

source("R/plot_theme.R")

source("R/population.R")

case_data <- tar_read(case_data)
LGAs <- unique(case_data$LGA)

date_period <- c(ymd("2021-11-01"), ymd("2022-06-25"))



case_counts <- case_data %>%
  filter(date_specimen >= date_period[1],
         date_specimen <= date_period[2]) %>% 
  
  mutate(date_symptom_onset = if_else(is.na(date_symptom_onset), date_specimen - days(4), date_symptom_onset)) %>%
  filter(date_symptom_onset >= date_period[1],
         date_symptom_onset <= date_period[2]) %>% 
  count(LGA, date = date_symptom_onset,
        name = "n_cases") %>%
  complete(
    LGA,
    date = seq(date_period[1], date_period[2], "days"),
    fill = list(n_cases = 0)
  )




case_counts %>%
  left_join(population_LGAs, by = "LGA") %>%
  group_by(date) %>%
  mutate(total_pop = sum(pop),
         p_pop = pop / total_pop,
         n_cases = n_cases / p_pop,
         total_cases = sum(n_cases),
         p_cases = n_cases / total_cases,
         logit_p_cases = qlogis(p_cases))  %>%
  
  ggplot() +
  
  geom_hline(yintercept = (1/8), linetype = "dashed") +
  
  geom_line(aes(x = date, y = p_cases, colour = LGA)) +
  
  ggokabeito::scale_colour_okabe_ito() +
  
  facet_wrap(~LGA) +
  
  plot_theme

