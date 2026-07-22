// Log-ratio Model B3: global + country + admin1 + alert deviations
// with transformed posterior (% change)

data {
  int<lower=1> N;
  int<lower=1> C;
  int<lower=1> U;
  int<lower=1> J;

  int<lower=1,upper=C> country[N];
  int<lower=1,upper=U> admin1[N];
  int<lower=1,upper=J> alert[N];

  vector[N] r;
  vector[N] d;

  int<lower=1,upper=C> country_admin1[U];
  int<lower=1,upper=U> admin1_alert[J];
}

parameters {
  real alpha0;

  vector[C] alpha_country;
  real<lower=0> tau_country;

  vector[U] alpha_admin1_std;
  real<lower=0> tau_admin1;

  vector[J] alpha_alert;
  real<lower=0> tau_alert;

  real<lower=0> sigma;
}

transformed parameters {
  vector[U] alpha_admin1 = alpha_admin1_std * tau_admin1;
}

model {
  alpha0 ~ normal(0, 1);

  alpha_country ~ normal(0, tau_country);
  tau_country ~ normal(0, 0.5);

  alpha_admin1_std ~ normal(0, 1);
  tau_admin1 ~ normal(0, 0.25);

  alpha_alert ~ normal(0, tau_alert);
  tau_alert ~ normal(0, 0.25);

  sigma ~ normal(0, 1);

  for (i in 1:N)
    r[i] ~ normal((alpha0 + alpha_country[country[i]] 
                        + alpha_admin1[admin1[i]] 
                        + alpha_alert[alert[i]]) * d[i], sigma);
}

generated quantities {
  vector[N] mu;
  vector[N] r_rep;
  vector[N] log_lik;

  // effective slopes
  real alpha_global = alpha0;
  vector[C] alpha_country_eff;
  vector[U] alpha_admin1_eff;
  vector[J] alpha_alert_eff;

  // transformed (% change)
  real alpha_global_exp_m1;
  vector[C] alpha_country_eff_exp_m1;
  vector[U] alpha_admin1_eff_exp_m1;
  vector[J] alpha_alert_eff_exp_m1;

  // posterior predictive
  for (i in 1:N) {
    mu[i] = (alpha0 
             + alpha_country[country[i]] 
             + alpha_admin1[admin1[i]] 
             + alpha_alert[alert[i]]) * d[i];

    r_rep[i] = normal_rng(mu[i], sigma);
    log_lik[i] = normal_lpdf(r[i] | mu[i], sigma);
  }

  // effective slopes
  for (c in 1:C)
    alpha_country_eff[c] = alpha0 + alpha_country[c];

  for (u in 1:U) {
    int c = country_admin1[u];
    alpha_admin1_eff[u] = alpha0 + alpha_country[c] + alpha_admin1[u];
  }

  for (j in 1:J) {
    int u = admin1_alert[j];
    int c = country_admin1[u];
    alpha_alert_eff[j] = alpha0 + alpha_country[c] + alpha_admin1[u] + alpha_alert[j];
  }

  // transformations
  alpha_global_exp_m1 = exp(alpha0) - 1;

  for (c in 1:C)
    alpha_country_eff_exp_m1[c] = exp(alpha_country_eff[c]) - 1;

  for (u in 1:U)
    alpha_admin1_eff_exp_m1[u] = exp(alpha_admin1_eff[u]) - 1;

  for (j in 1:J)
    alpha_alert_eff_exp_m1[j] = exp(alpha_alert_eff[j]) - 1;

  // prior predictive
  vector[N] mu_prior;
  vector[N] r_prior_pred;

  real alpha0_sim = normal_rng(0, 1);
  real tau_country_sim = fabs(normal_rng(0, 0.5));
  real tau_admin1_sim  = fabs(normal_rng(0, 0.25));
  real tau_alert_sim   = fabs(normal_rng(0, 0.25));
  real sigma_sim       = fabs(normal_rng(0, 1));

  vector[C] alpha_country_sim;
  vector[U] alpha_admin1_sim;
  vector[J] alpha_alert_sim;

  for (c in 1:C)
    alpha_country_sim[c] = normal_rng(0, tau_country_sim);

  for (u in 1:U)
    alpha_admin1_sim[u] = normal_rng(0, tau_admin1_sim);

  for (j in 1:J)
    alpha_alert_sim[j] = normal_rng(0, tau_alert_sim);

  for (i in 1:N) {
    mu_prior[i] = (alpha0_sim
                   + alpha_country_sim[country[i]]
                   + alpha_admin1_sim[admin1[i]]
                   + alpha_alert_sim[alert[i]]) * d[i];

    r_prior_pred[i] = normal_rng(mu_prior[i], sigma_sim);
  }
}
