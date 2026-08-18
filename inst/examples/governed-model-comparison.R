# Predictive comparison example; fitting lines require Stan.
library(gp3bayes)

sim <- simulate_advanced_pupil_timecourse(
  n_participants = 14,
  trials_per_participant = 4,
  time_points = 31,
  seed = 2028
)

base <- specify_advanced_pupil_timecourse_model(sim$data, family = "gaussian")
suite <- create_pupil_advanced_sensitivity_suite(base)
print(suite)

# robust <- materialize_pupil_advanced_sensitivity_scenario(suite, "likelihood_student")
# fit_base <- fit_advanced_pupil_model_backend(base, backend = "cmdstanr")
# fit_robust <- fit_advanced_pupil_model_backend(robust, backend = "cmdstanr")
# models <- create_pupil_model_set(gaussian = fit_base, student = fit_robust,
#                                  predictive_target = "new_trial_known_participant")
# comparison <- compare_pupil_models(models, criterion = "loo")
# print(comparison)
# plot_pupil_model_comparison(comparison)
# pupil_model_weights(comparison, method = "stacking")
