# Posterior Prediction for Approved gp3bayes Models

Distinguishes conditional expectations, new-outcome posterior
predictions, linear-predictor draws, and the conditional median for
lognormal duration models.

## Usage

``` r
predict_model(
  fit,
  newdata = NULL,
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

- newdata:

  Optional data frame. `NULL` uses the fitted prepared data.

- type:

  Prediction quantity: `"expected"`, `"predictive"`, `"linear"`, or
  `"median"`.

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

A `gp3bayes_prediction` containing draws, summaries, prediction data,
and interpretation metadata.
