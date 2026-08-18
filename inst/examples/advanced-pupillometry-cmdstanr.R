# Full gp3bayes 0.5 cmdstanr example (compiles Stan)
library(gp3bayes)

sim <- simulate_advanced_pupil_timecourse(
  n_participants = 16,
  trials_per_participant = 4,
  time_points = 31,
  seed = 2026
)

spec <- specify_advanced_pupil_timecourse_model(
  sim$data,
  temporal_structure = "gaussian_process",
  family = "gaussian",
  residual_scale = "condition_time",
  gp_spec = create_pupil_gp_spec("matern32", "approximate", 25),
  autocorrelation = "none",
  covariates = c("baseline_pupil", "luminance"),
  predictive_target = "future_segment"
)

prior <- check_advanced_pupil_prior_predictive(
  spec,
  backend = "cmdstanr",
  chains = 2,
  iter = 800,
  warmup = 400,
  cores = 2
)
print(prior)

fit <- fit_advanced_pupil_model_backend(
  spec,
  backend = "cmdstanr",
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 2,
  seed = 2026
)

print(diagnose_advanced_pupil_fit(fit))
trajectory <- predict_advanced_pupil_trajectory(fit)
plot_advanced_pupil_trajectory(trajectory)

sigma <- estimate_pupil_residual_scale(fit)
plot_pupil_residual_scale(sigma)

hyper <- pupil_gp_hyperparameters(fit)
print(hyper)
plot_pupil_gp_hyperparameters(hyper)

spectrum <- pupil_residual_spectrum(fit)
plot_pupil_residual_spectrum(spectrum)
