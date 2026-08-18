# Specify the experimental nonlinear pupil response-shape model

Uses a smooth asymmetric gated response: baseline + exp(log_amplitude)
\* logistic((time-onset)/exp(log_rise)) \*
logistic((onset+exp(log_duration)-time)/exp(log_decay)).

## Usage

``` r
specify_pupil_response_shape_model(
  prepared,
  family = c("gaussian", "student"),
  condition_effects = c("amplitude", "onset", "duration"),
  participant_effects = c("baseline", "amplitude"),
  covariates = character(),
  prior_scales = NULL
)
```

## Arguments

- prepared:

  A prepared pupil object or compatible data frame.

- family:

  Gaussian or Student-t.

- condition_effects:

  Character subset of `"amplitude"`, `"onset"`, and `"duration"`.

- participant_effects:

  Character subset of `"baseline"` and `"amplitude"`.

- covariates:

  Additional covariates for baseline only.

- prior_scales:

  Optional named positive numeric prior-scale overrides.

## Value

A `gp3bayes_pupil_response_shape_specification` object.

## Details

Condition effects can enter log-amplitude, onset, and log-duration. This
is a deliberately single, inspectable response family rather than an
arbitrary nonlinear-formula interface.
