// Log-ratio Model B2 : global average slope + country-level hierarchical slopes + alert-specific deviation with transformation


data {
  int<lower=1> N;                  // number of observations
  int<lower=1> C;                  // number of countries
  int<lower=1> J;                  // total number of alerts across all countries

  int<lower=1,upper=C> country[N]; // country index for each observation
  int<lower=1,upper=J> alert[N];   // alert index for each observation
  vector[N] r;                     // observed log-ratios
  vector[N] d;                     // delays

  // associating each alert to its country
  int<lower=1,upper=C> country_alert[J];  // alert to country
}

parameters {
  real alpha0;                     // global mean slope
  vector[C] alpha_country;         // country deviations
  real<lower=0> tau_country;       // sd of country deviations

  vector[J] alpha_alert;           // alert deviations
  real<lower=0> tau_alert;         // sd of alert deviations

  real<lower=0> sigma;             // measurement variability in the ratio
}

model {
  // Priors
  alpha0 ~ normal(0, 1); 
  alpha_country ~ normal(0, tau_country);
  tau_country ~ normal(0, 0.5);   

  alpha_alert ~ normal(0, tau_alert);
  tau_alert ~ normal(0, 0.25);

  sigma ~ normal(0, 1);

  // Likelihood
  for (i in 1:N)
    r[i] ~ normal((alpha0
                   + alpha_country[country[i]]
                   + alpha_alert[alert[i]]) * d[i], sigma);
}

generated quantities {

  // Posterior predictive
  vector[N] mu;
  vector[N] r_rep;
  vector[N] log_lik;

  // transformed posterior predictive
  vector[N] mu_exp_m1;
  vector[N] r_rep_exp_m1;

  for (i in 1:N) {
    mu[i] = (alpha0
             + alpha_country[country[i]]
             + alpha_alert[alert[i]]) * d[i];

    r_rep[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(r[i] | mu[i], sigma);

    // Transformations
    mu_exp_m1[i]    = exp(mu[i]) - 1;
    r_rep_exp_m1[i] = exp(r_rep[i]) - 1;
  }


  // Effective slopes

  // Global
  real alpha_global = alpha0;
  real alpha_global_exp_m1 = exp(alpha_global) - 1;

  // Country level
  vector[C] alpha_country_eff;
  vector[C] alpha_country_eff_exp_m1;

  for (c in 1:C) {
    alpha_country_eff[c] = alpha0 + alpha_country[c];
    alpha_country_eff_exp_m1[c] = exp(alpha_country_eff[c]) - 1;
  }

  // Alert level
  vector[J] alpha_alert_eff;
  vector[J] alpha_alert_eff_exp_m1;

  for (j in 1:J) {
    int c = country_alert[j];
    alpha_alert_eff[j] = alpha0 + alpha_country[c] + alpha_alert[j];
    alpha_alert_eff_exp_m1[j] = exp(alpha_alert_eff[j]) - 1;
  }


  // prior predictive

  vector[N] mu_prior;
  vector[N] r_prior_pred;

  // simulated parameters
  real alpha0_sim = normal_rng(0, 1);
  real tau_country_sim = fabs(normal_rng(0, 0.5));
  real tau_alert_sim   = fabs(normal_rng(0, 0.25));
  real sigma_sim       = fabs(normal_rng(0, 1));

  vector[C] alpha_country_sim;
  vector[J] alpha_alert_sim;

  for (c in 1:C)
    alpha_country_sim[c] = normal_rng(0, tau_country_sim);

  for (j in 1:J)
    alpha_alert_sim[j] = normal_rng(0, tau_alert_sim);

  for (i in 1:N) {
    mu_prior[i] = (alpha0_sim
                   + alpha_country_sim[country[i]]
                   + alpha_alert_sim[alert[i]]) * d[i];

    r_prior_pred[i] = normal_rng(mu_prior[i], sigma_sim);
  }
}
