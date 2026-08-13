# Binary Event-Probability Predictions

Binary Event-Probability Predictions

## Usage

``` r
predict_binary_probability(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL,
  probs = c(0.025, 0.5, 0.975)
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

- probs:

  Three probabilities used to summarise predictions.

## Value

A `gp3bayes_prediction` of event probabilities.
