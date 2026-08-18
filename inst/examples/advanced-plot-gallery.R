# gp3bayes 0.5 backend-free plot gallery
library(gp3bayes)

sim <- simulate_advanced_pupil_timecourse(
  n_participants = 12,
  trials_per_participant = 4,
  time_points = 37,
  family = "student",
  residual_scale = 0.085,
  heteroskedastic_strength = 0.55,
  ar = c(0.45, -0.10),
  missing_fraction = 0.06,
  measurement_error_sd = 0.02,
  seed = 2054
)

plot_advanced_pupil_simulation(sim)

temporal <- audit_pupil_temporal_dependence(sim$data, max_lag = 12)
plot_pupil_temporal_dependence(temporal)

measurement <- create_pupil_measurement_model(
  baseline_error = "baseline_se",
  luminance_error = "luminance_se",
  response_error = "pupil_se"
)
missingness <- create_pupil_missingness_spec(response = "model")

spec <- specify_advanced_pupil_timecourse_model(
  sim$data,
  temporal_structure = "gaussian_process",
  gp_spec = create_pupil_gp_spec(kernel = "matern32", basis = "approximate", k = 20),
  family = "student",
  residual_scale = "condition_time",
  autocorrelation = "none",
  covariates = c("baseline_pupil", "luminance"),
  measurement_model = measurement,
  missingness_model = missingness,
  predictive_target = "future_segment",
  allow_high_complexity = TRUE
)

plot_pupil_model_complexity(spec)
plot_pupil_identifiability_audit(audit_advanced_pupil_identifiability(spec))
plot_pupil_measurement_uncertainty(audit_pupil_measurement_model(spec))
plot_pupil_missingness(audit_pupil_missingness(spec))

bin <- simulate_binocular_pupil_timecourse(
  n_participants = 10,
  trials_per_participant = 4,
  time_points = 31,
  missing_fraction = 0.03,
  seed = 2055
)
prepared_bin <- prepare_binocular_pupil_timecourse(bin$data)
print(audit_binocular_pupil_readiness(prepared_bin))
