// Log-ratio Model B — global + country-level hierarchical slopes
// with transformed posterior (% change)

data {
  int<lower=1> N;
  int<lower=1> C;
  int<lower=1,upper=C> country[N];
  vector[N] r;
  vector[N] d;
}

parameters {
  real alpha0;
  vector[C] alpha_country;
  real<lower=0> tau_country;
  real<lower=0> sigma;
}

model {
  alpha0 ~ normal(0, 1);
  alpha_country ~ normal(0, tau_country);
  tau_country ~ normal(0, 1);
  sigma ~ normal(0, 1);

  for (i in 1:N)
    r[i] ~ normal((alpha0 + alpha_country[country[i]]) * d[i], sigma);
}

generated quantities {
  vector[N] mu;
  vector[N] r_rep;
  vector[N] log_lik;

  // effective slopes
  vector[C] alpha_country_eff;

  // Transformed (% change)
  real alpha_global_exp_m1;
  vector[C] alpha_country_eff_exp_m1;

  // effective slopes
  for (c in 1:C)
    alpha_country_eff[c] = alpha0 + alpha_country[c];

  // transformations
  alpha_global_exp_m1 = exp(alpha0) - 1;

  for (c in 1:C)
    alpha_country_eff_exp_m1[c] = exp(alpha_country_eff[c]) - 1;

  // posterior predictive
  for (i in 1:N) {
    mu[i] = alpha_country_eff[country[i]] * d[i];
    r_rep[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(r[i] | mu[i], sigma);
  }

  // prior predictive
  vector[N] mu_prior;
  vector[N] r_prior_pred;

  real alpha0_sim = normal_rng(0, 1);
  real tau_country_sim = fabs(normal_rng(0, 1));
  real sigma_sim = fabs(normal_rng(0, 1));

  vector[C] alpha_country_sim;
  vector[C] alpha_country_eff_sim;

  for (c in 1:C)
    alpha_country_sim[c] = normal_rng(0, tau_country_sim);

  for (c in 1:C)
    alpha_country_eff_sim[c] = alpha0_sim + alpha_country_sim[c];

  for (i in 1:N) {
    mu_prior[i] = alpha_country_eff_sim[country[i]] * d[i];
    r_prior_pred[i] = normal_rng(mu_prior[i], sigma_sim);
  }
}
