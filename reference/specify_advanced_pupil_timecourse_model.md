# Specify an advanced governed pupil time-course model

Builds the 0.5 advanced model contract without compiling or fitting
Stan. The function is additive to the frozen 0.4 API: it consumes the
same prepared pupil data but returns a distinct advanced specification.

## Usage

``` r
specify_advanced_pupil_timecourse_model(
  prepared,
  temporal_structure = c("smooth", "linear", "gaussian_process"),
  family = c("gaussian", "student"),
  residual_scale = c("constant", "condition", "time", "condition_time"),
  distribution = NULL,
  smooth_basis_dimension = 10L,
  gp_spec = create_pupil_gp_spec(),
  condition_trajectory = NULL,
  autocorrelation = c("none", "ar1", "ar2", "arma11"),
  participant_trajectory = c("none", "factor_smooth"),
  item_effects = NULL,
  covariates = character(),
  measurement_model = NULL,
  missingness_model = NULL,
  prior_scales = NULL,
  predictive_target = c("new_trial_known_participant", "new_participant",
    "future_segment", "new_sample_known_trial"),
  allow_high_complexity = FALSE
)
```

## Arguments

- prepared:

  A prepared 0.4 pupil object or compatible data frame.

- temporal_structure:

  `"smooth"`, `"linear"`, or `"gaussian_process"`.

- family:

  `"gaussian"` or robust `"student"`.

- residual_scale:

  Residual-scale model: constant, condition, time, or condition-by-time.

- distribution:

  Optional object from
  [`specify_pupil_distribution()`](https://stefanosbalaskas.github.io/gp3bayes/reference/specify_pupil_distribution.md).
  When supplied, its family and residual-scale declarations override the
  corresponding scalar arguments.

- smooth_basis_dimension:

  Basis dimension for smooth mean trajectories.

- gp_spec:

  A GP configuration from
  [`create_pupil_gp_spec()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_pupil_gp_spec.md).

- condition_trajectory:

  Whether condition-specific trajectories are included. Defaults to TRUE
  when a condition column exists.

- autocorrelation:

  One of `"none"`, `"ar1"`, `"ar2"`, `"arma11"`, or a bounded object
  from
  [`create_pupil_arma_spec()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_pupil_arma_spec.md).

- participant_trajectory:

  `"none"` or `"factor_smooth"`.

- item_effects:

  Include a random item intercept when an item column exists.

- covariates:

  Additional declared covariates.

- measurement_model:

  Optional known-uncertainty declaration.

- missingness_model:

  Optional MAR-oriented missingness declaration.

- prior_scales:

  Optional named numeric prior-scale overrides.

- predictive_target:

  Declared prediction target inherited from the 0.4 validation
  vocabulary.

- allow_high_complexity:

  Permit specifications flagged by the complexity audit. This is an
  explicit opt-in, not automatic model approval.

## Value

A `gp3bayes_pupil_advanced_specification` object.
