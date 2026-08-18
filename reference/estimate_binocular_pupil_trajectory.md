# Estimate joint binocular posterior trajectories

Estimate joint binocular posterior trajectories

## Usage

``` r
estimate_binocular_pupil_trajectory(
  fit,
  newdata = NULL,
  ndraws = 500L,
  probability = 0.95
)
```

## Arguments

- fit:

  A binocular fit.

- newdata:

  Optional prediction grid.

- ndraws:

  Posterior draws.

- probability:

  Central interval probability.

## Value

A `gp3bayes_binocular_pupil_trajectory` object.
