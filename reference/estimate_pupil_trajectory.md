# Estimate posterior pupil trajectories

Summarises a finite, declared prediction grid with pointwise or
grid-wise simultaneous posterior bands.

## Usage

``` r
estimate_pupil_trajectory(
  prediction,
  probability = 0.95,
  interval = c("pointwise", "simultaneous")
)
```

## Arguments

- prediction:

  A `gp3bayes_pupil_prediction`.

- probability:

  Credible probability.

- interval:

  `"pointwise"` or `"simultaneous"`.

## Value

A `gp3bayes_pupil_trajectory`.

## Uncertainty

`"simultaneous"` constructs a grid-wise band from the empirical
posterior maximum standardized deviation over the supplied finite grid.
It is not a universal continuous-time confidence band.

## Examples

``` r
grid <- data.frame(.event_time = seq(0, 1, length.out = 5))
draws <- matrix(rnorm(500), nrow = 100)
prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
estimate_pupil_trajectory(prediction)
#> <gp3bayes_pupil_estimand>
#>   Rows: 5
#>   Automatic significance decision: FALSE
```
