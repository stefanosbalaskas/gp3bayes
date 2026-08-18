# Simulate advanced dynamic pupil time courses

Generates deterministic hierarchical traces with optional Student-t
contamination, heteroskedasticity, ARMA dependence, measurement error,
and missingness. The simulator is intended for examples, recovery
studies, and failure-path validation; it does not claim physiological
realism.

## Usage

``` r
simulate_advanced_pupil_timecourse(
  n_participants = 24L,
  trials_per_participant = 6L,
  time_points = 41L,
  time_range = c(-500, 2500),
  conditions = c("control", "treatment"),
  family = c("gaussian", "student"),
  residual_scale = 0.08,
  heteroskedastic_strength = 0.35,
  ar = 0.45,
  ma = numeric(),
  participant_sd = 0.12,
  amplitude_condition = 0.22,
  latency_condition = 120,
  outlier_fraction = 0.01,
  missing_fraction = 0.03,
  measurement_error_sd = 0.015,
  student_df = 5,
  seed = 2026
)
```

## Arguments

- n_participants:

  Number of participants.

- trials_per_participant:

  Trials per participant.

- time_points:

  Number of samples per trial.

- time_range:

  Numeric length-two time range in milliseconds.

- conditions:

  Character condition labels.

- family:

  `"gaussian"` or `"student"`.

- residual_scale:

  Baseline residual SD.

- heteroskedastic_strength:

  Multiplicative time-varying noise strength.

- ar:

  Numeric AR coefficients, length at most 3.

- ma:

  Numeric MA coefficients, length at most 2.

- participant_sd:

  Participant random-intercept SD.

- amplitude_condition:

  Difference in response amplitude for condition 2.

- latency_condition:

  Difference in peak latency for condition 2.

- outlier_fraction:

  Fraction of observations receiving extra contamination.

- missing_fraction:

  Fraction of pupil observations set missing.

- measurement_error_sd:

  Known response-measurement SD; zero disables.

- student_df:

  Degrees of freedom when `family = "student"`.

- seed:

  Random seed.

## Value

A `gp3bayes_pupil_advanced_simulation` object with data and stored
truth.
