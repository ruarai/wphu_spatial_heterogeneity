data {
  int<lower=0> N;
  
  array[N] int<lower=0> n_cases; 
  array[N] int t;
  
  
  vector[N] mobility;
  
  real pop_size;
  real theta;
  real sigma_R_start;
  real R_0_center;
}

parameters {
  real R_0_log;
  real<lower=0> gamma;
  real<lower=0> mu_mobility;
}


model {
  gamma ~ lognormal(0, 1);
  mu_mobility ~ lognormal(0, 1);
  
  
  R_0_log ~ normal(0, sigma_R_start);
  
  real R_0 = exp(R_0_log) + R_0_center;
  
  real beta_0 = R_0 / gamma;
  
  real cumulative_infections = 0;
  
  vector[N] log_infections_t;
  log_infections_t[1] = log(1);
  
  for(i in 1:(N - 1)) {
    real log_infections = log_infections_t[i];
    
    real sus = pop_size - cumulative_infections;
    real inf = 0;
    for(j in max(i - 7, 1):(i - 1)) {
      inf += exp(log_infections_t[j]);
    }
    
    real total_growth = beta_0 * ((sus - inf) / pop_size) * (1 / mobility[i]) - gamma;
    
    
    cumulative_infections += exp(log_infections);
    
    real log_infections_next = log_infections + total_growth;
    
    log_infections_next = fmin(log_infections_next, 12);
    log_infections_next = fmax(log_infections_next, 0);
    
    log_infections_t[i + 1] = log_infections_next;
    
    n_cases[i + 1] ~ neg_binomial_2_log(log_infections_next, theta);
  }
}

generated quantities {
  vector[N] n_cases_sim;
  vector[N] growth_rate;
  n_cases_sim[1] = 1;
  
  real cumulative_infections = 0;
  real R_0 = exp(R_0_log) + R_0_center;
  
  real beta_0 = R_0 / gamma;
  
  vector[N] log_infections_t;
  log_infections_t[1] = log(1);
  
  for(i in 1:(N - 1)) {
    real log_infections = log_infections_t[i];
    
    real sus = pop_size - cumulative_infections;
    real inf = 0;
    for(j in max(i - 7, 1):(i - 1)) {
      inf += exp(log_infections_t[j]);
    }
    
    real total_growth = beta_0 * ((sus - inf) / pop_size) * (1 / mobility[i]) - gamma;
    
    
    cumulative_infections += exp(log_infections);
    
    real log_infections_next = log_infections + total_growth;
    
    log_infections_next = fmin(log_infections_next, 12);
    log_infections_next = fmax(log_infections_next, 0);
    
    log_infections_t[i + 1] = log_infections_next;
    
    n_cases_sim[i + 1] = neg_binomial_2_log_rng(log_infections_next, theta);
    growth_rate[i] = total_growth;
  }
}
