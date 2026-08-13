# Posterior-Predictive Quantile Envelope

Posterior-Predictive Quantile Envelope

## Usage

``` r
predictive_quantile_envelope(
  x,
  probabilities = seq(0.05, 0.95, by = 0.05),
  probs = c(0.025, 0.5, 0.975),
  ndraws = 500L,
  include_group_effects = TRUE,
  seed = 1L
)
```

## Arguments

- x:

  A fitted model or predictive atlas.

- probabilities:

  Outcome quantile probabilities.

- probs:

  Posterior interval probabilities for each replicated quantile.

- ndraws:

  Predictive draws when `x` is a fit.

- include_group_effects:

  Whether fitted group effects are included.

- seed:

  Predictive seed.

## Value

A quantile-envelope table.
