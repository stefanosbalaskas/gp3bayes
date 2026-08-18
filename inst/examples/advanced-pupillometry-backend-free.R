# gp3bayes 0.5 backend-free example gallery
library(gp3bayes)

sim <- simulate_advanced_pupil_timecourse(
  n_participants = 12,
  trials_per_participant = 4,
  time_points = 31,
  heteroskedastic_strength = 0.6,
  ar = 0.5,
  missing_fraction = 0.05,
  seed = 2026
)

plot_advanced_pupil_simulation(sim)
plot_pupil_temporal_dependence(audit_pupil_temporal_dependence(sim$data))

measurement <- create_pupil_measurement_model(
  baseline_error = "baseline_se",
  luminance_error = "luminance_se",
  response_error = "pupil_se"
)
missingness <- create_pupil_missingness_spec(response = "model")

spec <- specify_advanced_pupil_timecourse_model(
  sim$data,
  temporal_structure = "gaussian_process",
  gp_spec = create_pupil_gp_spec("matern32", "approximate", 25),
  residual_scale = "condition_time",
  autocorrelation = "none",
  covariates = c("baseline_pupil", "luminance"),
  measurement_model = measurement,
  missingness_model = missingness,
  predictive_target = "future_segment"
)

print(spec)
plot_pupil_model_complexity(spec)
plot_pupil_measurement_uncertainty(audit_pupil_measurement_model(spec))
plot_pupil_missingness(audit_pupil_missingness(spec))
print(pupil_model_card(spec))
