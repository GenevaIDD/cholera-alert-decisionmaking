// Log-ratio Model D: global + admin1 + alert-level deviations with transformed posteriors

data {
  int<lower=1> N;                  // number of observations
  int<lower=1> U;                  // number of admin1 units
  int<lower=1> J;                  // total number of alerts
  int<lower=1,upper=U> admin1[N];  // admin1 index for each observation
  int<lower=1,upper=J> alert[N];   // alert index for each observation
  int<lower=1,upper=U> admin1_alert[J]; // admin1 index for each alert
  vector[N] r;                     // observed log-ratios
  vector[N] d;                     // delays
}

parameters {
  real alpha0;                     // global mean slope
  vector[U] alpha_admin1;          // admin1 deviations
  real<lower=0> tau_admin1;        // sd of admin1 deviations

  vector[J] alpha_alert;           // alert deviations
  real<lower=0> tau_alert;         // sd of alert deviations

  real<lower=0> sigma;             // measurement variability
}

model {
  alpha0 ~ normal(0, 1);
  alpha_admin1 ~ normal(0, tau_admin1);
  tau_admin1 ~ normal(0, 0.5);

  alpha_alert ~ normal(0, tau_alert);
  tau_alert ~ normal(0, 0.25);

  sigma ~ normal(0, 1);

  for (i in 1:N)
    r[i] ~ normal((alpha0 + alpha_admin1[admin1[i]] + alpha_alert[alert[i]]) * d[i], sigma);
}

generated quantities {
  vector[N] mu;
  vector[N] r_rep;
  vector[N] log_lik;
  vector[N] r_prior_pred;

  vector[U] alpha_admin1_eff;
  vector[J] alpha_alert_eff;

  // transformed posteriors
  real alpha_global_exp_m1;
  vector[U] alpha_admin1_eff_exp_m1;
  vector[J] alpha_alert_eff_exp_m1;

  // effective slopes
  for (u in 1:U)
    alpha_admin1_eff[u] = alpha0 + alpha_admin1[u];

  for (j in 1:J) {
    int u = admin1_alert[j];
    alpha_alert_eff[j] = alpha0 + alpha_admin1[u] + alpha_alert[j];
  }

  // transformed slopes
  alpha_global_exp_m1 = exp(alpha0) - 1;

  for (u in 1:U)
    alpha_admin1_eff_exp_m1[u] = exp(alpha_admin1_eff[u]) - 1;

  for (j in 1:J)
    alpha_alert_eff_exp_m1[j] = exp(alpha_alert_eff[j]) - 1;

  // posterior predictive
  for (i in 1:N) {
    mu[i] = (alpha0 + alpha_admin1[admin1[i]] + alpha_alert[alert[i]]) * d[i];
    r_rep[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(r[i] | mu[i], sigma);
  }

  // prior predictive
  vector[N] mu_prior;
  real alpha0_sim = normal_rng(0, 1);
  real tau_admin1_sim = fabs(normal_rng(0, 0.5));
  real tau_alert_sim = fabs(normal_rng(0, 0.25));
  real sigma_sim = fabs(normal_rng(0, 1));

  vector[U] alpha_admin1_sim;
  vector[J] alpha_alert_sim;
  vector[J] alpha_alert_eff_sim;

  for (u in 1:U)
    alpha_admin1_sim[u] = normal_rng(0, tau_admin1_sim);

  for (j in 1:J) {
    int u = admin1_alert[j];
    alpha_alert_sim[j] = normal_rng(0, tau_alert_sim);
    alpha_alert_eff_sim[j] = alpha0_sim + alpha_admin1_sim[u] + alpha_alert_sim[j];
  }

  for (i in 1:N) {
    int j = alert[i];
    mu_prior[i] = alpha_alert_eff_sim[j] * d[i];
    r_prior_pred[i] = normal_rng(mu_prior[i], sigma_sim);
  }
}
