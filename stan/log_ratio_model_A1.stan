// Log-ratio Model A1 — Global slope beta
// with transformed posterior (% change)

data {
  int<lower=1> N;
  vector[N] r;
  vector[N] d;
}

parameters {
  real beta;                 
  real<lower=0> sigma;       
}

model {
  beta ~ normal(0, 1);
  sigma ~ normal(0, 1);

  r ~ normal(beta .* d, sigma);
}

generated quantities {
  vector[N] mu;
  vector[N] r_rep;
  vector[N] log_lik;
  vector[N] r_prior_pred;

  // transformed (% change)
  real alpha_global_exp_m1;

  // posterior predictive
  for (i in 1:N) {
    mu[i] = beta * d[i];
    r_rep[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(r[i] | mu[i], sigma);
  }

  // transformation
  alpha_global_exp_m1 = exp(beta) - 1;

  // prior predictive
  real beta_sim = normal_rng(0, 1);
  real sigma_sim = fabs(normal_rng(0, 1));

  for (i in 1:N) {
    real mu_prior = beta_sim * d[i];
    r_prior_pred[i] = normal_rng(mu_prior, sigma_sim);
  }
}
