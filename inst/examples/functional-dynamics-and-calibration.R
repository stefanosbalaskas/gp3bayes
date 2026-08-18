# gp3bayes 0.5 backend-free functional dynamics and calibration example
library(gp3bayes)

sim <- simulate_advanced_pupil_timecourse(
  n_participants = 10,
  trials_per_participant = 4,
  time_points = 41,
  time_range = c(-200, 1800),
  residual_scale = 0.08,
  ar = 0.35,
  missing_fraction = 0,
  seed = 2051
)

spec <- specify_advanced_pupil_timecourse_model(
  sim$data,
  temporal_structure = "smooth",
  family = "gaussian",
  residual_scale = "condition_time",
  autocorrelation = "ar1",
  predictive_target = "future_segment"
)

# Construct a deterministic synthetic posterior trajectory for demonstrating
# functional estimands without fitting Stan. Real analyses should replace this
# object with predict_advanced_pupil_trajectory(fit, ...).
times <- sort(unique(sim$data$time_ms))
conditions <- levels(sim$data$condition)
grid <- expand.grid(
  time_ms = times,
  condition = conditions,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
grid$condition <- factor(grid$condition, levels = conditions)

set.seed(2052)
S <- 400L
mu <- with(
  grid,
  3.2 +
    0.42 * plogis((time_ms - 300) / 170) * plogis((1250 - time_ms) / 320) +
    ifelse(condition == conditions[[2L]], 0.18 * plogis((time_ms - 420) / 150), 0)
)
draws <- matrix(
  stats::rnorm(S * nrow(grid), rep(mu, each = S), 0.035),
  nrow = S,
  ncol = nrow(grid)
)
trajectory <- structure(
  list(
    grid = grid,
    draws = draws,
    type = "expected",
    population_only = TRUE,
    specification = spec
  ),
  class = c("gp3bayes_pupil_advanced_trajectory", "gp3bayes_pupil_trajectory")
)

plot_advanced_pupil_trajectory(trajectory)

derivative <- estimate_pupil_trajectory_derivative(trajectory, order = 1)
plot_pupil_trajectory_derivative(derivative)

contrast <- estimate_pupil_dynamic_contrast(
  trajectory,
  contrast = c(conditions[[2L]], conditions[[1L]]),
  threshold = 0.05
)
plot_pupil_dynamic_contrast(contrast)
print(estimate_pupil_threshold_duration(contrast, direction = "above"))

# Predictive calibration can be demonstrated directly from posterior predictive
# draws. In real validation, these draws must come from genuinely held-out data.
set.seed(2053)
y <- stats::rnorm(80, 3.5, 0.12)
yrep <- matrix(stats::rnorm(500 * 80, rep(y, each = 500), 0.10), nrow = 500)
score <- score_pupil_predictions(y, yrep, probability = 0.90)
print(score)
plot_pupil_predictive_calibration(score)
