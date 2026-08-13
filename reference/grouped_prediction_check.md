# Grouped Posterior Predictive Check

Grouped Posterior Predictive Check

## Usage

``` r
grouped_prediction_check(
  fit,
  group,
  ndraws = 1000L,
  probs = c(0.025, 0.5, 0.975),
  seed = 1L
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- group:

  Name of a grouping column in the prepared model data.

- ndraws:

  Number of posterior predictive draws.

- probs:

  Posterior interval probabilities.

- seed:

  Non-negative seed.

## Value

A `gp3bayes_group_prediction_check`.
