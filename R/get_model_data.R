source("R/model_data_functions.R")
source("R/population.R")

get_model_data <- function(
  case_data, 
  mobility_data,
  vaccination_data
) {
  
  
  date_period_wt <- c(ymd("2020-05-25"), ymd("2020-10-15"))
  date_period_delta <- c(ymd("2021-08-07"), ymd("2021-11-15"))
  
  
  
  LGAs <- unique(case_data$LGA)
  LGAs <- LGAs[order(LGAs)]
  
  
  
  
  case_counts_wt <- get_case_counts(case_data, date_period_wt)
  case_counts_delta <- get_case_counts(case_data, date_period_delta)
  
  mobility_smooth_wt <- smooth_mobility_data(mobility_data, date_period_wt)
  mobility_smooth_delta <- smooth_mobility_data(mobility_data, date_period_delta)
  
  
  
  ## TODO justify
  # mobility_smooth_delta <- mobility_smooth_delta %>%
  #   mutate(mobility = if_else(date <= ymd("2021-08-15"), NA_real_, mobility)) %>%
  #   group_by(LGA) %>%
  #   fill(mobility, .direction = "up")
  
  
  fit_data_wt <- case_counts_wt %>%
    select(LGA, date, n_cases) %>%
    left_join(population_LGAs, by = "LGA") %>% 
    left_join(mobility_smooth_wt, by = c("LGA", "date")) %>%
    
    mutate(t = as.numeric(date - date_period_wt[1]) + 1) %>%
    
    arrange(LGA, t)
  
  fit_data_delta <- case_counts_delta %>%
    select(LGA, date, n_cases) %>%
    left_join(population_LGAs, by = "LGA") %>% 
    left_join(mobility_smooth_delta, by = c("LGA", "date")) %>%
    left_join(vaccination_data, by = c("LGA", "date")) %>% 
    
    mutate(t = as.numeric(date - date_period_delta[1]) + 1) %>%
    
    arrange(LGA, t)
  
  
  fit_data_to_matrix <- function(x, variable) {
    col <- deparse(substitute(variable))
    
    x %>%
      select(date, LGA, all_of(col)) %>%
      arrange(LGA) %>%
      mutate(LGA = factor(LGA, unique(LGA))) %>% 
      pivot_wider(names_from = LGA, values_from = all_of(col)) %>%
      select(-date) %>%
      as.matrix()
  }
  
  
  dates_wt <- fit_data_wt$date %>% unique()
  dates_delta <- fit_data_delta$date %>% unique()
  
  data_list <- list(
    t_max_wt = length(dates_wt),
    t_max_delta = length(dates_delta),
    
    n_pops = length(LGAs),
    
    n_cases_wt = fit_data_to_matrix(fit_data_wt, n_cases),
    n_cases_delta = fit_data_to_matrix(fit_data_delta, n_cases),
    
    mobility_wt = fit_data_to_matrix(fit_data_wt, mobility),
    mobility_delta = fit_data_to_matrix(fit_data_delta, mobility),
    
    p_vacc = fit_data_to_matrix(fit_data_delta, p_second_dose),
    
    LGA_names = LGAs,
    dates_wt = fit_data_wt$date %>% unique(),
    dates_delta = fit_data_delta$date %>% unique(),
    
    t_ix_wt = fit_data_wt$t %>% unique(),
    t_ix_delta = fit_data_delta$t %>% unique(),
    
    fit_data_tbl_wt = fit_data_wt,
    fit_data_tbl_delta = fit_data_delta,
    
    nb_theta = 60,
    
    
    rho = 7,
    alpha = 0.005
  )
  
  
  data_list
  
  
  
  
  
  
  
  
  
  
}


