# Extract Expected Posterior Predictions

Extract Expected Posterior Predictions

## Usage

``` r
extract_expected_predictions(
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

A numeric matrix of conditional expected-response draws.
