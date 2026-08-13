# Extract Posterior Predictive Draws

Extract Posterior Predictive Draws

## Usage

``` r
extract_posterior_predictions(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL,
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

A numeric matrix of new-outcome posterior predictive draws.
