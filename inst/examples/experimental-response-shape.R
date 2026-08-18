# Experimental nonlinear response-shape example
library(gp3bayes)

sim <- simulate_pupil_response_shape(
  n_participants = 14,
  trials_per_participant = 4,
  time_points = 41,
  seed = 2029
)

spec <- specify_pupil_response_shape_model(sim$data)
print(spec)

# Requires Stan backend:
# fit <- fit_pupil_response_shape_model(spec, backend = "cmdstanr", cores = 2)
# pars <- estimate_pupil_response_parameters(fit)
# print(pars)
# plot_pupil_response_parameters(pars)
