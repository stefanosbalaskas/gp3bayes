# Duration Quantile Calibration

Duration Quantile Calibration

## Usage

``` r
duration_quantile_calibration(x, quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9))
```

## Arguments

- x:

  A duration posterior predictive `gp3bayes_prediction`.

- quantiles:

  Predictive quantiles to assess.

## Value

A table comparing nominal predictive quantiles with empirical coverage
below those quantiles.
