data {
  int<lower=0> n_pops;
  
  int<lower=0> t_max_wt;
  int<lower=0> t_max_delta;
  
  array[n_pops, t_max_wt] int<lower=0> n_cases_wt; 
  array[n_pops, t_max_delta] int<lower=0> n_cases_delta; 
  
  array[n_pops, t_max_wt] real mobility_wt;
  array[n_pops, t_max_delta] real mobility_delta;
  
  array[n_pops, t_max_delta] real p_second_dose;
  
  
  real nb_theta;
}

parameters {
  vector[n_pops] b;
  
  real logit_VE;
  
  real<lower=0> gamma;
  
  real<lower=0> mu_wt;
  real<lower=0> mu_delta;
  
  vector[n_pops] I_log_0_wt;
  vector[n_pops] I_log_0_delta;
}


model {
  b ~ normal(0, 1);
  
  logit_VE ~ normal(0, 1);
  
  gamma ~ lognormal(0, 1);
  mu_wt ~ lognormal(0, 1);
  mu_delta ~ lognormal(0, 1);
  
  I_log_0_wt ~ normal(0, 1);
  
  
  array[n_pops, t_max_wt] real log_infections_t_wt;
  
  for(p in 1:n_pops) {
    log_infections_t_wt[p, 1] = I_log_0_wt[p];
    
    for(t in 1:(t_max_wt - 1)) {
      real log_infections = log_infections_t_wt[p, t];
      
      real total_growth = mu_wt * (1 - b[p] * mobility_wt[p][t]) - gamma;
      
      real log_infections_next = log_infections + total_growth;
      
      log_infections_t_wt[p, t + 1] = log_infections_next;
      
      n_cases_wt[p][t + 1] ~ neg_binomial_2_log(log_infections_next, nb_theta);
    }
  }
  
  
  
  I_log_0_delta ~ normal(0, 1);
  
  
  
  array[n_pops, t_max_delta] real log_infections_t_delta;
  
  for(p in 1:n_pops) {
    log_infections_t_delta[p, 1] = I_log_0_delta[p];
    
    for(t in 1:(t_max_delta - 1)) {
      real log_infections = log_infections_t_delta[p, t];
      
      real vaccine_effect = 1 - p_second_dose[p, t] * inv_logit(logit_VE);
      
      real total_growth = mu_delta * vaccine_effect * (1 - b[p] * mobility_delta[p][t]) - gamma;
      
      real log_infections_next = log_infections + total_growth;
      
      log_infections_t_delta[p, t + 1] = log_infections_next;
      
      n_cases_delta[p][t + 1] ~ neg_binomial_2_log(log_infections_next, nb_theta);
    }
  }
}

generated quantities {
  array[n_pops, t_max_wt] real n_cases_sim_wt;
  array[n_pops, t_max_wt] real log_infections_t_wt;
  array[n_pops, t_max_wt] real growth_rate_wt;
  
  for(p in 1:n_pops) {
    log_infections_t_wt[p, 1] = I_log_0_wt[p];
    
    for(t in 1:(t_max_wt - 1)) {
      real log_infections = log_infections_t_wt[p, t];
      
      real total_growth = mu_wt * (1 - b[p] * mobility_wt[p][t]) - gamma;
      
      real log_infections_next = log_infections + total_growth;
      
      log_infections_t_wt[p, t + 1] = log_infections_next;
      
      n_cases_sim_wt[p, t + 1] = neg_binomial_2_log_rng(log_infections_next, nb_theta);
      growth_rate_wt[p, t] = total_growth;
    }
  }
  
  
  array[n_pops, t_max_delta] real n_cases_sim_delta;
  array[n_pops, t_max_delta] real log_infections_t_delta;
  array[n_pops, t_max_delta] real growth_rate_delta;
  
  for(p in 1:n_pops) {
    log_infections_t_delta[p, 1] = I_log_0_delta[p];
    
    for(t in 1:(t_max_delta - 1)) {
      real log_infections = log_infections_t_delta[p, t];
      
      real vaccine_effect = 1 - p_second_dose[p, t] * inv_logit(logit_VE);
      
      real total_growth = mu_delta * vaccine_effect * (1 - b[p] * mobility_delta[p][t]) - gamma;
      
      real log_infections_next = log_infections + total_growth;
      
      log_infections_t_delta[p, t + 1] = log_infections_next;
      
      n_cases_sim_delta[p, t + 1] = neg_binomial_2_log_rng(log_infections_next, nb_theta);
      growth_rate_delta[p, t] = total_growth;
    }
  }
  
  real VE = inv_logit(logit_VE);
}

