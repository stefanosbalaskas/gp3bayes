# Duration Predictions

Duration Predictions

## Usage

``` r
predict_duration(
  fit,
  newdata = NULL,
  type = c("median", "expected", "predictive"),
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

- newdata:

  Optional data frame. `NULL` uses the fitted prepared data.

- type:

  One of `"median"`, `"expected"`, or `"predictive"`.

- include_group_effects:

  Whether fitted group-level effects are included.

- allow_new_levels:

  Whether new grouping levels are permitted by brms.

- ndraws:

  Optional number of posterior draws.

- probs:

  Three probabilities used to summarise predictions.

- seed:

  Non-negative seed used for posterior predictive simulation.

## Value

A `gp3bayes_prediction` on the recorded duration scale.
