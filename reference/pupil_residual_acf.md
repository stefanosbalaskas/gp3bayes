# Summarise residual autocorrelation for a pupil fit

Summarise residual autocorrelation for a pupil fit

## Usage

``` r
pupil_residual_acf(x, max_lag = 10L, ndraws = 200L)
```

## Arguments

- x:

  A pupil fit or pupil diagnostics object.

- max_lag:

  Maximum lag when `x` is a fit.

- ndraws:

  Draws for fit-based expected residuals.

## Value

A data frame of mean within-series residual autocorrelations.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```
