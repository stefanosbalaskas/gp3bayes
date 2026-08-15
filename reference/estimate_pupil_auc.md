# Estimate area under a declared pupil-response window

Estimate area under a declared pupil-response window

## Usage

``` r
estimate_pupil_auc(prediction, window, probability = 0.95)
```

## Arguments

- prediction:

  A pupil prediction object.

- window:

  Prespecified event-relative time window.

- probability:

  Credible probability.

## Value

A pupil estimand. AUC units are pupil-unit times time-unit.

## Examples

``` r
grid <- data.frame(.event_time = seq(0, 1, length.out = 5))
draws <- matrix(rnorm(500), nrow = 100)
prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
estimate_pupil_auc(prediction, c(0.2, 0.8))
#> <gp3bayes_pupil_estimand>
#>   Estimand: auc
#>   Rows: 1
#>   Declared window: 0.2 to 0.8
#>   Automatic significance decision: FALSE
```
