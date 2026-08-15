# Estimate a declared-window mean pupil response

Estimate a declared-window mean pupil response

## Usage

``` r
estimate_pupil_window(prediction, window, probability = 0.95)
```

## Arguments

- prediction:

  A pupil prediction object.

- window:

  Prespecified event-relative time window.

- probability:

  Credible probability.

## Value

A `gp3bayes_pupil_estimand`.

## Examples

``` r
grid <- data.frame(.event_time = seq(0, 1, length.out = 5))
draws <- matrix(rnorm(500), nrow = 100)
prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
estimate_pupil_window(prediction, c(0.2, 0.8))
#> <gp3bayes_pupil_estimand>
#>   Estimand: window_mean
#>   Rows: 1
#>   Declared window: 0.2 to 0.8
#>   Automatic significance decision: FALSE
```
