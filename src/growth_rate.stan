data {
  int<lower=0> t_max;
  int<lower=0> n_pops;
  
  array[n_pops, t_max] int<lower=0> n_cases; 
  array[n_pops, t_max] real mobility;
  
  
  real nb_theta;
}

parameters {
  vector[n_pops] a;
  vector[n_pops] b;
  
  vector[n_pops] I_log_0;
}


model {
  
  a ~ normal(0, 1);
  b ~ normal(0, 1);
  
  I_log_0 ~ normal(0, 1);
  
  array[n_pops, t_max] real log_infections_t;
  
  for(p in 1:n_pops) {
    log_infections_t[p, 1] = I_log_0[p];
    
    for(t in 1:(t_max - 1)) {
      real log_infections = log_infections_t[p, t];
      
      real total_growth = a[p] - b[p] * mobility[p][t];
      
      real log_infections_next = log_infections + total_growth;
      
      log_infections_t[p, t + 1] = log_infections_next;
      
      n_cases[p][t + 1] ~ neg_binomial_2_log(log_infections_next, nb_theta);
    }
  }
  
  
}

generated quantities {
  array[n_pops, t_max] real n_cases_sim;
  array[n_pops, t_max] real growth_rate;
  
  array[n_pops, t_max] real log_infections_t;
  
  
  for(p in 1:n_pops) {
    log_infections_t[p, 1] = I_log_0[p];
    
    n_cases_sim[p, 1] = neg_binomial_2_log_rng(log_infections_t[p, 1], nb_theta);
    
    for(t in 1:(t_max - 1)) {
      real log_infections = log_infections_t[p, t];
      
      real total_growth = a[p] - b[p] * mobility[p][t];
      
      real log_infections_next = log_infections + total_growth;
      
      log_infections_t[p, t + 1] = log_infections_next;
      
      n_cases_sim[p, t + 1] = neg_binomial_2_log_rng(log_infections_next, nb_theta);
      growth_rate[p, t] = total_growth;
    }
  }
}
