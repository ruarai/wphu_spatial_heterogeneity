
fit_reff <- function(case_data) {
  LGAs <- unique(case_data$LGA)
  
  require(EpiNow2)
  
  
  fits_LGAs <- map(
    LGAs,
    function(i_LGA) {
      
      case_data_LGA <- case_data %>%
        filter(LGA == i_LGA)
      
      
      
      
      reporting_delay <- list(
        mean = convert_to_logmean(2, 1), mean_sd = 0.1,
        sd = convert_to_logsd(2, 1), sd_sd = 0.1,
        max = 10
      )
      
      generation_time <- get_generation_time(
        disease = "SARS-CoV-2", source = "ganyani"
      )
      incubation_period <- get_incubation_period(
        disease = "SARS-CoV-2", source = "lauer"
      )
      
      
      reported_cases <- case_data_LGA %>%
        mutate(
          date_symptom_onset = if_else(
            is.na(date_symptom_onset),
            date_specimen - days(2),
            date_symptom_onset
          )
        ) %>%
        
        count(date = date_symptom_onset, name = "confirm") %>%
        
        filter(date > ymd("2020-01-01"),
               date < ymd("2022-07-01"))
      
      
      infections <- estimate_infections(
        reported_cases = reported_cases,
        generation_time = generation_time,
        delays = delay_opts(incubation_period),
        backcalc = backcalc_opts("reports", prior_window = 7, rt_window = 3),
        stan = stan_opts(cores = 4),
        rt = NULL,
        
        horizon = 0,
      )
    }
  )
}



