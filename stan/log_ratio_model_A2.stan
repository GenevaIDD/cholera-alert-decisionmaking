// Log-ratio Model A2 — global slope + alert-specific deviation
// with transformed posterior (% change)

data {
  int<lower=1> N;
  int<lower=1> J;
  int<lower=1,upper=J> alert[N];
  vector[N] r;
  vector[N] d;
}

parameters {
  real alpha0;
  vector[J] alpha_alert;
  real<lower=0> tau_alert;
  real<lower=0> sigma;
}

transformed parameters {
  vector[J] alpha_alert_eff;
  for (j in 1:J)
    alpha_alert_eff[j] = alpha0 + alpha_alert[j];
}

model {
  alpha0 ~ normal(0, 1);
  alpha_alert ~ normal(0, tau_alert);
  tau_alert ~ normal(0, 0.5);
  sigma ~ normal(0, 1);

  for (i in 1:N)
    r[i] ~ normal(alpha_alert_eff[alert[i]] * d[i], sigma);
}

generated quantities {
  // posterior predictive
  vector[N] mu;
  vector[N] r_rep;
  vector[N] log_lik;

  // Transformed (% change)
  real alpha_global_exp_m1;
  vector[J] alpha_alert_eff_exp_m1;

  // posterior predictive
  for (i in 1:N) {
    mu[i] = alpha_alert_eff[alert[i]] * d[i];
    r_rep[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(r[i] | mu[i], sigma);
  }

  // keep original effective slopes for completeness
  vector[J] alpha_alert_eff_out;
  for (j in 1:J)
    alpha_alert_eff_out[j] = alpha_alert_eff[j];

  // transformations
  alpha_global_exp_m1 = exp(alpha0) - 1;

  for (j in 1:J)
    alpha_alert_eff_exp_m1[j] = exp(alpha_alert_eff[j]) - 1;

  // prior predictive
  real alpha0_sim = normal_rng(0, 1);
  real tau_alert_sim = fabs(normal_rng(0, 0.5));
  real sigma_sim = fabs(normal_rng(0, 1));

  vector[J] alpha_alert_sim;
  for (j in 1:J)
    alpha_alert_sim[j] = normal_rng(0, tau_alert_sim);

  vector[J] alpha_alert_eff_sim;
  for (j in 1:J)
    alpha_alert_eff_sim[j] = alpha0_sim + alpha_alert_sim[j];

  vector[N] mu_prior;
  vector[N] r_prior_pred;

  for (i in 1:N) {
    mu_prior[i] = alpha_alert_eff_sim[alert[i]] * d[i];
    r_prior_pred[i] = normal_rng(mu_prior[i], sigma_sim);
  }
}
