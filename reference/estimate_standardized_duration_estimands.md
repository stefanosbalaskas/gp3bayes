# Estimate Design-Standardised Duration Estimands

Produces posterior draws of average conditional medians, their
difference and ratio, the average log-duration contrast, and a posterior
predictive upper quantile under each focal-condition level.

## Usage

``` r
estimate_standardized_duration_estimands(
  fit,
  target_data = NULL,
  target_scale = c("prepared", "raw"),
  predictive_quantile = 0.9,
  ndraws = NULL,
  include_group_effects = FALSE,
  seed = 1L
)
```

## Arguments

- fit:

  An approved gp3bayes duration fit.

- target_data:

  Optional target covariate distribution.

- target_scale:

  Whether supplied target data are raw or prepared.

- predictive_quantile:

  Predictive quantile probability.

- ndraws:

  Optional number of posterior draws.

- include_group_effects:

  Whether group-level effects are included.

- seed:

  Seed used for posterior predictive draws.

## Value

A `gp3bayes_estimand`.
