
functions {
  vector immunity_individual(vector days_before, real duration_immunity, real slope_immunity) {
    return inv_logit(((duration_immunity+0.5) * 100 - days_before) * slope_immunity);
  }
  
  // Calculate immunity up to (not including) time t according to cases_sim
  real immunity_total(
    int t, array[] int n_cases, real population_size, real duration_immunity, real slope_immunity, real holiday_ascertainment
  ) {
    vector[t] n_cases_sub = to_vector(n_cases[:t]);
    vector[t] days_before;
    for(i in 1:(t)) { days_before[i] = t - 1; }
    
    if(t >= 44) {
      int top_ix = min(t, 85);
      for(i in 44:top_ix) {
        real asc_mult = 1 + holiday_ascertainment * (inv_logit((85 - i) * 0.3) - inv_logit((44 - i) * 0.3));
        n_cases_sub[i] *= asc_mult;
      }
    }
    
    return sum(immunity_individual(days_before, duration_immunity, slope_immunity) .* n_cases_sub ./ population_size);
  }
}

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
  real<lower=0> mu_immunity;
  
  //real<lower=0> duration_immunity;
  //real<lower=0> slope_immunity;
  
  real<lower=0> holiday_ascertainment;
  
  
  real<lower=0.3, upper=0.6> t_BA1;
  real<lower=0.8, upper=1.4> t_BA2;
}
model {
  mu ~ lognormal(0, 0.5);
  mu_mobility ~ lognormal(0, 0.5);
  
  mu_BA1 ~ lognormal(0, 0.5);
  mu_BA2 ~ lognormal(0, 0.5);
  mu_immunity ~ lognormal(0, 0.5);
  
  //duration_immunity ~ lognormal(0, 0.5);
  //slope_immunity ~ lognormal(-1, 0.4);
  real duration_immunity = 1.5;
  real slope_immunity = 0.3;
  
  
  holiday_ascertainment ~ lognormal(0, 0.5);
  
  t_BA1 ~ uniform(0.3, 0.6);
  t_BA2 ~ uniform(0.8, 1.4);
  
  real log_infections_next = -1;
  
  for(i in 1:(N - 1)) {
    real log_infections;
    if(log_infections_next == -1) {
       log_infections = log(n_cases[i]);
    } else {
      log_infections = log_infections_next;
    }
    
    real asc_mult = 1 + holiday_ascertainment * (inv_logit((85 - i) * 0.3) - inv_logit((44 - i) * 0.3));
    
    real effect_BA1 = (1 - inv_logit((t_BA1 * 100 - i) * 0.5)) * mu_BA1;
    real effect_BA2 = (1 - inv_logit((t_BA2 * 100 - i) * 0.15)) * mu_BA2;
    real effect_mobility = mobility[i] * mu_mobility;
    
    real total_immunity = immunity_total(i, n_cases, population_size, duration_immunity, slope_immunity, holiday_ascertainment);
    real effect_immunity = total_immunity * mu_immunity;

    
    real total_growth = mu - effect_mobility - effect_immunity + effect_BA1 + effect_BA2;
    
    
    log_infections_next = log_infections + total_growth;
    
    log_infections_next = fmin(log_infections_next, 12);
    
    real log_cases_next = log_infections_next - log(asc_mult);
    
    n_cases[i + 1] ~ neg_binomial_2_log(log_cases_next, theta);
  }
}

generated quantities {
  array[N] int n_cases_sim;
  vector[N] growth_rate;
  n_cases_sim[1] = n_cases[1];
  
  real log_infections_next = -1;
  real duration_immunity = 1.5;
  real slope_immunity = 0.3;
  
  
  for(i in 1:(N - 1)) {
    real log_infections;
    if(log_infections_next == -1) {
       log_infections = log(n_cases[i]);
    } else {
      log_infections = log_infections_next;
    }
    
    real asc_mult = 1 + holiday_ascertainment * (inv_logit((95 - i) * 0.3) - inv_logit((44 - i) * 0.3));
    
    real effect_BA1 = (1 - inv_logit((t_BA1 * 100 - i) * 0.5)) * mu_BA1;
    real effect_BA2 = (1 - inv_logit((t_BA2 * 100 - i) * 0.15)) * mu_BA2;
    real effect_mobility = mobility[i] * mu_mobility;
    
    real total_immunity = immunity_total(i, n_cases, population_size, duration_immunity, slope_immunity, holiday_ascertainment);
    real effect_immunity = total_immunity * mu_immunity;

    
    real total_growth = mu - effect_mobility - effect_immunity + effect_BA1 + effect_BA2;
    
    
    log_infections_next = log_infections + total_growth;
    
    log_infections_next = fmin(log_infections_next, 12);
    
    real log_cases_next = log_infections_next - log(asc_mult);
    
    n_cases_sim[i + 1] = neg_binomial_2_log_rng(log_cases_next, theta);
    growth_rate[i] = total_growth;
  }
}

