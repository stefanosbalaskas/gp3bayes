# Create a Numeric Prediction Profile

Create a Numeric Prediction Profile

## Usage

``` r
create_prediction_profile(
  fit,
  variable,
  values = NULL,
  n = 50L,
  at = list(),
  type = c("expected", "predictive", "linear", "median"),
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL,
  probs = c(0.025, 0.5, 0.975),
  seed = 1L
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- variable:

  Numeric predictor to vary.

- values:

  Optional explicit predictor values.

- n:

  Number of values when `values` is omitted.

- at:

  Named values holding other predictors fixed.

- type:

  Prediction quantity.

- include_group_effects:

  Whether group effects are included.

- allow_new_levels:

  Whether new grouping levels are permitted.

- ndraws:

  Optional posterior draws.

- probs:

  Three interval probabilities.

- seed:

  Predictive simulation seed where applicable.

## Value

A `gp3bayes_prediction_profile`.
