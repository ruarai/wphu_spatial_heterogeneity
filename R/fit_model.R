

fit_model <- function(model, model_data) {
  
  require(cmdstanr)
  
  fit <- model$sample(
    data = model_data[c("n_pops", "t_max_wt", "t_max_delta", "n_cases_wt", "n_cases_delta", "mobility_wt", "mobility_delta", "p_second_dose", "nb_theta")], 
    seed = 5, 
    chains = 4, 
    parallel_chains = 4,
    refresh = 100,
    iter_warmup = 1000,
    iter_sampling = 1000
  )
  
  file_out <- "data/cmdstanr_fit.rds"
  
  fit$save_object(file_out)
  
  return(file_out)
}