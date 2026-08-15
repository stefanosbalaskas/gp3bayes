# Estimate posterior peak pupil response inside a declared window

Estimate posterior peak pupil response inside a declared window

## Usage

``` r
estimate_pupil_peak(prediction, window, probability = 0.95)
```

## Arguments

- prediction:

  A pupil prediction object.

- window:

  Prespecified event-relative time window.

- probability:

  Credible probability.

## Value

A pupil estimand with posterior uncertainty in the peak.

## Examples

``` r
grid <- data.frame(.event_time = seq(0, 1, length.out = 5))
draws <- matrix(rnorm(500), nrow = 100)
prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
estimate_pupil_peak(prediction, c(0.2, 0.8))
#> <gp3bayes_pupil_estimand>
#>   Estimand: peak
#>   Rows: 1
#>   Declared window: 0.2 to 0.8
#>   Automatic significance decision: FALSE
```
