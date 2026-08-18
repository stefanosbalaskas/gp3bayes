# Audit empirical temporal dependence before model fitting

Computes descriptive within-series autocorrelation and spacing
diagnostics. The audit is diagnostic only and does not select an ARMA
order.

## Usage

``` r
audit_pupil_temporal_dependence(x, max_lag = 10L)
```

## Arguments

- x:

  A prepared pupil object, data frame, or advanced specification.

- max_lag:

  Maximum lag for descriptive ACF summaries.

## Value

A `gp3bayes_pupil_temporal_dependence_audit` object.
