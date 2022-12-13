data {
  int<lower=0> t_max_wt;
  
  array[t_max_wt] int<lower=0> n_cases_wt; 
  
  array[t_max_wt] real mobility_wt;
  
  real<lower=0> rho;
  real<lower=0> alpha;
  
  real t_ix[t_max_wt];
  
  real nb_theta;
}

transformed data {
  
  matrix[t_max_wt, t_max_wt] cov = cov_exp_quad(t_ix, alpha, rho)
                     + diag_matrix(rep_vector(1e-10, t_max_wt));
  matrix[t_max_wt, t_max_wt] L_cov = cholesky_decompose(cov);
}

parameters {
  real b;
  
  real<lower=0> gamma;
  
  real<lower=0> mu_wt;
  
  real I_log_0_wt;
  
  vector[t_max_wt] f_tilde;
}

transformed parameters {
  vector[t_max_wt] log_f_predict = L_cov * f_tilde;
}


model {
  b ~ normal(0, 1);
  
  gamma ~ lognormal(0, 1);
  mu_wt ~ lognormal(0, 1);
  
  I_log_0_wt ~ normal(0, 1);
  
  f_tilde ~ normal(0, 0.05);
  
  
  array[t_max_wt] real log_infections_t_wt;
  
  log_infections_t_wt[1] = I_log_0_wt;
  
  for(t in 1:(t_max_wt - 1)) {
    real log_infections = log_infections_t_wt[t];
    
    real total_growth = mu_wt * (1 - b * mobility_wt[t]) - gamma + log_f_predict[t];
    
    real log_infections_next = log_infections + total_growth;
    
    log_infections_t_wt[t + 1] = log_infections_next;
    
    n_cases_wt[t + 1] ~ neg_binomial_2_log(log_infections_next, nb_theta);
  }
}

generated quantities {
  array[t_max_wt] real n_cases_sim_wt;
  array[t_max_wt] real log_infections_t_wt;
  array[t_max_wt] real growth_rate_wt;
  
  log_infections_t_wt[1] = I_log_0_wt;
  
  for(t in 1:(t_max_wt - 1)) {
    real log_infections = log_infections_t_wt[t];
    
    real total_growth = mu_wt * (1 - b * mobility_wt[t]) - gamma + log_f_predict[t];
    
    real log_infections_next = log_infections + total_growth;
    
    log_infections_t_wt[t + 1] = log_infections_next;
    
    n_cases_sim_wt[t + 1] = neg_binomial_2_log_rng(log_infections_next, nb_theta);
    growth_rate_wt[t] = total_growth;
  }
}

