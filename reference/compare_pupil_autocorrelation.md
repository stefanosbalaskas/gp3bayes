# Compare residual autocorrelation across fitted advanced models

This function does not choose a winner. It summarises lag-specific
residual dependence after subtracting posterior expected means.

## Usage

``` r
compare_pupil_autocorrelation(..., max_lag = 10L, ndraws = 300L)
```

## Arguments

- ...:

  Two or more named advanced fits, or one named list.

- max_lag:

  Maximum residual ACF lag.

- ndraws:

  Draws used for posterior expected means.

## Value

A `gp3bayes_pupil_autocorrelation_comparison` object.
