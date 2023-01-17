

fit_model <- function(model, model_data) {
  
  require(cmdstanr)
  
  model_data_stan <- model_data[c("t_max_wt", "n_pops", "n_cases_wt", "mobility_wt", "nb_theta",
                                  "t_max_delta", "n_cases_delta", "mobility_delta", "p_vacc",
                                  "t_ix_wt", "t_ix_delta", "rho", "alpha")]
  
  fit <- model$sample(
    data = model_data_stan, 
    seed = 4, 
    chains = 4, 
    parallel_chains = 4,
    refresh = 100,
    iter_warmup = 1000,
    iter_sampling = 4000
  )
  
  file_out <- "data/cmdstanr_fit.rds"
  
  fit$save_object(file_out)
  
  return(file_out)
}