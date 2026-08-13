# Extract Linear-Predictor Draws

Extract Linear-Predictor Draws

## Usage

``` r
extract_linear_predictions(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- newdata:

  Optional data frame. `NULL` uses the fitted prepared data.

- include_group_effects:

  Whether fitted group-level effects are included.

- allow_new_levels:

  Whether new grouping levels are permitted by brms.

- ndraws:

  Optional number of posterior draws.

## Value

A numeric matrix on the model linear-predictor scale.
