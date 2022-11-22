data {
  int<lower=0> N;
  
  array[N] int<lower=0> n_cases; 
  
  
  vector[N] mobility;
  
  real theta;
  real sigma_R_start;
  real R_0_center;
}

parameters {
  real<lower=0> gamma;
  
  real a;
  real b;
  
  real I_log_0;
}


model {
  gamma ~ lognormal(0, 1);
  
  a ~ normal(0, 0.5);
  b ~ normal(0, 0.5);
  I_log_0 ~ normal(0, 1);
  
  vector[N] log_infections_t;
  log_infections_t[1] = I_log_0;
  
  for(i in 1:(N - 1)) {
    real log_infections = log_infections_t[i];
    
    real total_growth = a - b * mobility[i] - gamma;
    
    real log_infections_next = log_infections + total_growth;
    
    log_infections_t[i + 1] = log_infections_next;
    
    n_cases[i + 1] ~ neg_binomial_2_log(log_infections_next, theta);
  }
}

generated quantities {
  vector[N] n_cases_sim;
  vector[N] growth_rate;
  n_cases_sim[1] = 1;
  
  vector[N] log_infections_t;
  log_infections_t[1] = I_log_0;
  
  for(i in 1:(N - 1)) {
    real log_infections = log_infections_t[i];
    
    real total_growth = a - b * mobility[i] - gamma;
    
    real log_infections_next = log_infections + total_growth;
    
    log_infections_t[i + 1] = log_infections_next;
    
    n_cases_sim[i + 1] = neg_binomial_2_log_rng(log_infections_next, theta);
    growth_rate[i] = total_growth;
  }
}
