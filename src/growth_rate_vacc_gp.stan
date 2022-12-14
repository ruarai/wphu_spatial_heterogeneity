data {
  int<lower=0> t_max_wt;
  int<lower=0> t_max_delta;
  
  int<lower=1> n_pops;
  
  array[t_max_wt, n_pops] int<lower=0> n_cases_wt; 
  array[t_max_delta, n_pops] int<lower=0> n_cases_delta; 

  array[t_max_wt, n_pops] real mobility_wt;
  array[t_max_delta, n_pops] real mobility_delta;
  
  array[t_max_delta, n_pops] real p_vacc;
  
  real<lower=0> rho;
  real<lower=0> alpha;
  
  array[t_max_wt] real t_ix_wt;
  array[t_max_delta] real t_ix_delta;
  
  real nb_theta;
}

transformed data {
  matrix[t_max_wt, t_max_wt] cov_wt = gp_exp_quad_cov(t_ix_wt, alpha, rho) +
  diag_matrix(rep_vector(1e-10, t_max_wt));
  matrix[t_max_wt, t_max_wt] L_cov_wt = cholesky_decompose(cov_wt);
  
  matrix[t_max_delta, t_max_delta] cov_delta = gp_exp_quad_cov(t_ix_delta, alpha, rho) +
  diag_matrix(rep_vector(1e-10, t_max_delta));
  matrix[t_max_delta, t_max_delta] L_cov_delta = cholesky_decompose(cov_delta);
}

parameters {
  vector[n_pops] b;
  
  real logit_VE;
  
  real<lower=0> gamma;
  
  real<lower=0> mu_wt;
  real<lower=0> mu_delta;
  
  vector[n_pops] I_log_0_wt;
  vector[n_pops] I_log_0_delta;
  
  matrix[t_max_wt, n_pops] f_wt;
  matrix[t_max_delta, n_pops] f_delta;
}

transformed parameters {
  matrix[t_max_wt, n_pops] log_f_pred_wt;
  matrix[t_max_delta, n_pops] log_f_pred_delta;
  
  for(p in 1:n_pops) {
     log_f_pred_wt[, p] = L_cov_wt * f_wt[, p];
     log_f_pred_delta[, p] = L_cov_delta * f_delta[, p];
  }
  
  real VE = inv_logit(logit_VE);
}


model {
  b ~ normal(0, 1);
  
  logit_VE ~ normal(0, 1);
  
  gamma ~ lognormal(0, 1);
  
  mu_wt ~ lognormal(0, 1);
  mu_delta ~ lognormal(0, 1);
  
  I_log_0_wt ~ normal(0, 1);
  I_log_0_delta ~ normal(0, 1);
  
  for(p in 1:n_pops) {
    f_wt[, p] ~ normal(0, 1);
    f_delta[, p] ~ normal(0, 1);
  }
  
  
  array[t_max_wt, n_pops] real log_infections_t_wt;
  
  for(p in 1:n_pops) {
    log_infections_t_wt[1, p] = I_log_0_wt[p];
    
    for(t in 1:(t_max_wt - 1)) {
      real log_infections = log_infections_t_wt[t, p];
      
      real total_growth = mu_wt * (1 - b[p] * mobility_wt[t, p]) - gamma + log_f_pred_wt[t, p];
      
      real log_infections_next = log_infections + total_growth;
      
      log_infections_t_wt[t + 1, p] = log_infections_next;
      
      n_cases_wt[t + 1, p] ~ neg_binomial_2_log(log_infections_next, nb_theta);
    }
  }
  
  
  array[t_max_delta, n_pops] real log_infections_t_delta;
  
  for(p in 1:n_pops) {
    log_infections_t_delta[1, p] = I_log_0_delta[p];
    
    for(t in 1:(t_max_delta - 1)) {
      real log_infections = log_infections_t_delta[t, p];
      
      real total_growth = mu_delta * (1 - b[p] * mobility_delta[t, p]) * (1 - p_vacc[t, p] * VE) - 
        gamma + 
        log_f_pred_delta[t, p];
      
      real log_infections_next = log_infections + total_growth;
      
      log_infections_t_delta[t + 1, p] = log_infections_next;
      
      n_cases_delta[t + 1, p] ~ neg_binomial_2_log(log_infections_next, nb_theta);
    }
  }
  
}

generated quantities {
  array[t_max_wt, n_pops] real n_cases_sim_wt;
  array[t_max_wt, n_pops] real log_infections_t_wt;
  
  for(p in 1:n_pops) {
    log_infections_t_wt[1, p] = I_log_0_wt[p];
    
    for(t in 1:(t_max_wt - 1)) {
      real log_infections = log_infections_t_wt[t, p];
      
      real total_growth = mu_wt * (1 - b[p] * mobility_wt[t, p]) - gamma + log_f_pred_wt[t, p];
      
      real log_infections_next = log_infections + total_growth;
      
      log_infections_t_wt[t + 1, p] = log_infections_next;
      
      n_cases_sim_wt[t + 1, p] = neg_binomial_2_log_rng(log_infections_next, nb_theta);
    }
  }
  
  array[t_max_delta, n_pops] real n_cases_sim_delta;
  array[t_max_delta, n_pops] real log_infections_t_delta;
  
  for(p in 1:n_pops) {
    log_infections_t_delta[1, p] = I_log_0_delta[p];
    
    for(t in 1:(t_max_delta - 1)) {
      real log_infections = log_infections_t_delta[t, p];
      
      real total_growth = mu_delta * (1 - b[p] * mobility_delta[t, p]) * (1 - p_vacc[t, p] * VE) - 
        gamma + 
        log_f_pred_delta[t, p];
      
      real log_infections_next = log_infections + total_growth;
      
      log_infections_t_delta[t + 1, p] = log_infections_next;
      
      n_cases_sim_delta[t + 1, p] = neg_binomial_2_log_rng(log_infections_next, nb_theta);
    }
  }
  
}

