
data {
  int n_days;
  int steps_per_day;
  int pop_size;
  int I_start;
  
  vector[n_days] mobility;
  real mu_mobility;
  real mu_beta_BA2;
  real mu_beta_variant;
  real mu_gamma_variant;
  real mu_alpha_variant;
  
  real sigma_R_start;
}

transformed data {
  int t_steps = steps_per_day * n_days;
}

generated quantities {
  real dt = 1.0 / (1.0 * steps_per_day);
  
  real R_start = exp(normal_rng(0, sigma_R_start));
  real gamma_0 = lognormal_rng(-1.5, 0.2);
  real alpha_0 = lognormal_rng(-1, 0.5);
  
  
  real beta_0 = gamma_0 / R_start;
  
  
  
  vector[t_steps] S;
  vector[t_steps] E;
  vector[t_steps] I;
  vector[t_steps] R;
  
  S[1] = pop_size - I_start;
  E[1] = 0;
  I[1] = I_start;
  R[1] = 0;
  
  for(t in 1:(t_steps - 1)) {
    real S_t = S[t];
    real E_t = E[t];
    real I_t = I[t];
    real R_t = R[t];
    
    int d = t %/% steps_per_day + 1;
    
    real BA1_proportion = (1 - inv_logit((50 - d) * 0.6));
    real BA2_proportion = (1 - inv_logit((100 - d) * 0.2));
    
    real effect_beta_omicron = 1 + BA1_proportion * mu_beta_variant;
    real effect_gamma_omicron = 1 + BA1_proportion * mu_gamma_variant;
    real effect_alpha_omicron = 1 + BA1_proportion * mu_alpha_variant;
    
    real effect_beta_BA2 = 1 + BA2_proportion * mu_beta_BA2;
    
    real effect_beta_mobility = exp(mu_mobility * mobility[d]);
    
    real beta_t = beta_0 * effect_beta_omicron * effect_beta_mobility * effect_beta_BA2;
    real gamma_t = gamma_0 * effect_gamma_omicron;
    real alpha_t = alpha_0 / effect_alpha_omicron;
  
    real StoE = S_t * I_t / pop_size * beta_t * dt;
    real EtoI = E_t * alpha_t * dt;
    real ItoR = I_t * gamma_t * dt;


    real S_new = S_t - StoE;
    real E_new = E_t + StoE - EtoI;
    real I_new = I_t + EtoI - ItoR;
    real R_new = R_t + ItoR;
  
    S[t + 1] = S_new;
    E[t + 1] = E_new;
    I[t + 1] = I_new;
    R[t + 1] = R_new;
  }
  
  vector[n_days] S_d;
  vector[n_days] E_d;
  vector[n_days] I_d;
  vector[n_days] R_d;
  

  for(d in 1:n_days) {
    S_d[d] = S[(d - 1) * steps_per_day + 1];
    E_d[d] = E[(d - 1) * steps_per_day + 1];
    I_d[d] = I[(d - 1) * steps_per_day + 1];
    R_d[d] = R[(d - 1) * steps_per_day + 1];
  }
}

