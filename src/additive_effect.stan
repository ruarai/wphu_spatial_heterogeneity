
data {
  int<lower=0> N;
  
  array[N] int<lower=0> n_cases; 
  array[N] int t;
  
  vector[N] mobility_raw;
  
  real theta;
  real population_size;
}
transformed data {
  vector[N] mobility;
  mobility = (mobility_raw - mean(mobility_raw)) / sd(mobility_raw);
}


parameters {
  real<lower=0> mu;
  real<lower=0> mu_BA1;
  real<lower=0> mu_BA2;
  real<lower=0> mu_mobility;
  
  real<lower=0> gamma;
  
  
  real<lower=0.3, upper=0.6> t_BA1;
  real<lower=0.8, upper=1.4> t_BA2;
}
model {
  mu ~ lognormal(0, 0.5);
  mu_mobility ~ lognormal(0, 0.5);
  
  mu_BA1 ~ lognormal(0, 0.5);
  mu_BA2 ~ lognormal(0, 0.5);
  gamma ~ lognormal(0, 0.5);
  
  
  t_BA1 ~ uniform(0.3, 0.6);
  t_BA2 ~ uniform(0.8, 1.4);
  
  real holiday_ascertainment = 2;
  real t_holiday_start = 50;
  real t_holiday_end = 85;
  
  real slope_holiday_start = 0.7;
  real slope_holiday_end = 0.3;
  
  
  real log_infections_next = -1;
  real cumulative_infections = 0;
  
  for(i in 1:(N - 1)) {
    real asc_mult = 1 + holiday_ascertainment * (inv_logit((t_holiday_end - i) * slope_holiday_start) - 
                                                 inv_logit((t_holiday_start - i) * slope_holiday_end));
    
    real log_infections;
    if(log_infections_next == -1) {
       log_infections = log(n_cases[i]) + log(asc_mult);
    } else {
      log_infections = log_infections_next;
    }
    
    cumulative_infections += exp(log_infections);
    
    real effect_BA1 = (1 - inv_logit((t_BA1 * 100 - i) * 0.5)) * mu_BA1;
    real effect_BA2 = (1 - inv_logit((t_BA2 * 100 - i) * 0.15)) * mu_BA2;
    real effect_mobility = mobility[i] * mu_mobility;
    
    
    real total_S = population_size - cumulative_infections;
    real growth_mult = (total_S / population_size);

    
    real total_growth = growth_mult * (mu + effect_mobility + effect_BA1 + effect_BA2) - gamma;
    
    
    log_infections_next = log_infections + total_growth;
    
    log_infections_next = fmin(log_infections_next, 12);
    log_infections_next = fmax(log_infections_next, 0);
    
    real log_cases_next = log_infections_next - log(asc_mult);
    
    n_cases[i + 1] ~ neg_binomial_2_log(log_cases_next, theta);
  }
}

generated quantities {
  array[N] int n_cases_sim;
  vector[N] growth_rate;
  n_cases_sim[1] = n_cases[1];
  
  real log_infections_next = -1;
  real cumulative_infections = 0;
  
  real holiday_ascertainment = 2;
  real t_holiday_start = 50;
  real t_holiday_end = 85;
  
  real slope_holiday_start = 0.7;
  real slope_holiday_end = 0.3;
  
  
  for(i in 1:(N - 1)) {
    real asc_mult = 1 + holiday_ascertainment * (inv_logit((t_holiday_end - i) * slope_holiday_start) - 
                                                 inv_logit((t_holiday_start - i) * slope_holiday_end));
    
    real log_infections;
    if(log_infections_next == -1) {
       log_infections = log(n_cases[i]) + log(asc_mult);
    } else {
      log_infections = log_infections_next;
    }
    
    cumulative_infections += exp(log_infections);
    
    real effect_BA1 = (1 - inv_logit((t_BA1 * 100 - i) * 0.5)) * mu_BA1;
    real effect_BA2 = (1 - inv_logit((t_BA2 * 100 - i) * 0.15)) * mu_BA2;
    real effect_mobility = mobility[i] * mu_mobility;
    
    
    real total_S = population_size - cumulative_infections;
    real growth_mult = (total_S / population_size);

    
    real total_growth = growth_mult * (mu + effect_mobility + effect_BA1 + effect_BA2) - gamma;
    
    
    log_infections_next = log_infections + total_growth;
    
    log_infections_next = fmin(log_infections_next, 12);
    log_infections_next = fmax(log_infections_next, 0);
    
    real log_cases_next = log_infections_next - log(asc_mult);
    
    n_cases_sim[i + 1] = neg_binomial_2_log_rng(log_cases_next, theta);
    growth_rate[i] = total_growth;
  }
}

