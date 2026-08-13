# Create a Two-Dimensional Prediction Surface

Create a Two-Dimensional Prediction Surface

## Usage

``` r
create_prediction_surface(
  fit,
  x,
  y,
  x_values = NULL,
  y_values = NULL,
  n = 30L,
  at = list(),
  type = c("expected", "predictive", "linear", "median"),
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL,
  probs = c(0.025, 0.5, 0.975),
  seed = 1L,
  max_rows = 2500L
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- x, y:

  Numeric predictors.

- x_values, y_values:

  Optional explicit predictor values.

- n:

  Values per predictor when explicit values are omitted.

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

  Predictive simulation seed.

- max_rows:

  Maximum grid rows.

## Value

A `gp3bayes_prediction_surface`.
