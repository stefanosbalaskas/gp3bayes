# Estimate the residual-scale trajectory from a distributional model

Estimate the residual-scale trajectory from a distributional model

## Usage

``` r
estimate_pupil_residual_scale(
  fit,
  newdata = NULL,
  ndraws = 500L,
  probability = 0.95
)
```

## Arguments

- fit:

  An advanced fit.

- newdata:

  Optional prediction data.

- ndraws:

  Number of posterior draws.

- probability:

  Central interval probability.

## Value

A `gp3bayes_pupil_residual_scale` object.
