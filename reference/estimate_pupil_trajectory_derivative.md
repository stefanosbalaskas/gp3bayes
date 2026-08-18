# Estimate posterior temporal derivatives of a pupil trajectory

Computes finite-difference posterior derivatives on the prediction grid.
This is a descriptive functional estimand: it does not automatically
define physiological onset, changepoints, or cognitively meaningful
phases.

## Usage

``` r
estimate_pupil_trajectory_derivative(
  prediction,
  order = 1L,
  probability = 0.95
)
```

## Arguments

- prediction:

  A `gp3bayes_pupil_advanced_trajectory` object.

- order:

  Derivative order, 1 (velocity/slope) or 2 (acceleration/curvature).

- probability:

  Central posterior interval probability.

## Value

A `gp3bayes_pupil_trajectory_derivative` object.
