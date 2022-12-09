data{
  real eta;
  real phi;
}

generated quantities {
  vector[10000] x;
  
  for(i in 1:10000) {
    x[i] = neg_binomial_2_log_rng(log(30), 70);
  }
}