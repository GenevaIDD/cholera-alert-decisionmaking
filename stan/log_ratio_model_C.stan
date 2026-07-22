// Log-ratio Model C — location-level hierarchical slopes
// with transformed posterior (% change)

data {
  int<lower=1> N;
  int<lower=1> E;
  int<lower=1, upper=E> loc[N];
  vector[N] r;
  vector[N] d;
}

parameters {
  real alpha0;
  vector[E] alpha_loc;
  real<lower=0> tau_loc;
  real<lower=0> sigma;
}

transformed parameters {
  vector[E] alpha_loc_eff;
  for (e in 1:E)
    alpha_loc_eff[e] = alpha0 + alpha_loc[e];
}

model {
  alpha0 ~ normal(0, 1);
  alpha_loc ~ normal(0, tau_loc);
  tau_loc ~ normal(0, 1);
  sigma ~ normal(0, 1);

  for (i in 1:N)
    r[i] ~ normal(alpha_loc_eff[loc[i]] * d[i], sigma);
}

generated quantities {
  vector[N] mu;
  vector[N] r_rep;
  vector[N] log_lik;
  vector[N] r_prior_pred;

  // transformed (% change)
  real alpha_global_exp_m1;
  vector[E] alpha_loc_eff_exp_m1;

  // posterior predictive
  for (i in 1:N) {
    mu[i] = alpha_loc_eff[loc[i]] * d[i];
    r_rep[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(r[i] | mu[i], sigma);
  }

  // raw effective slopes
  vector[E] alpha_loc_eff_out;
  for (e in 1:E)
    alpha_loc_eff_out[e] = alpha_loc_eff[e];

  // transformations
  alpha_global_exp_m1 = exp(alpha0) - 1;

  for (e in 1:E)
    alpha_loc_eff_exp_m1[e] = exp(alpha_loc_eff[e]) - 1;

  // prior predictive
  real alpha0_sim = normal_rng(0, 1);
  real tau_loc_sim = fabs(normal_rng(0, 1));
  real sigma_sim = fabs(normal_rng(0, 1));

  vector[E] alpha_loc_sim;
  for (e in 1:E)
    alpha_loc_sim[e] = normal_rng(0, tau_loc_sim);

  for (i in 1:N) {
    real mu_prior = (alpha0_sim + alpha_loc_sim[loc[i]]) * d[i];
    r_prior_pred[i] = normal_rng(mu_prior, sigma_sim);
  }
}
