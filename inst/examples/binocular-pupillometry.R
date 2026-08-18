# Joint binocular pupil model example
library(gp3bayes)

sim <- simulate_binocular_pupil_timecourse(
  n_participants = 16,
  trials_per_participant = 4,
  time_points = 31,
  residual_correlation = 0.7,
  eye_bias = 0.02,
  seed = 2027
)
prep <- prepare_binocular_pupil_timecourse(sim$data)
print(audit_binocular_pupil_readiness(prep))

spec <- specify_binocular_pupil_model(
  prep,
  temporal_structure = "smooth",
  family = "gaussian",
  residual_correlation = TRUE
)

# Requires Stan backend:
# fit <- fit_binocular_pupil_model(spec, backend = "cmdstanr", cores = 2)
# trajectory <- estimate_binocular_pupil_trajectory(fit)
# plot_binocular_pupil_trajectory(trajectory)
# pupil_binocular_correlation(fit)
# pupil_binocular_agreement_table(trajectory, tolerance = 0.10)
