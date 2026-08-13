# Decompose Prediction Uncertainty Descriptively

Separates variability in conditional expected-response draws from total
posterior predictive variability. The difference is a descriptive Monte
Carlo decomposition and is not a causal variance decomposition.

## Usage

``` r
prediction_uncertainty_decomposition(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = 1000L,
  seed = 1L
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

- seed:

  Non-negative seed used for posterior predictive simulation.

## Value

A `gp3bayes_prediction_uncertainty` object.
