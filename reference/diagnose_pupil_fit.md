# Diagnose temporal and sampling behaviour of a pupil fit

Reports posterior R-hat/ESS summaries, NUTS sampler evidence when
available, residual temporal drift, and residual autocorrelation.
Thresholds are numerical review gates, not adequacy certification.

## Usage

``` r
diagnose_pupil_fit(fit, ndraws = 200L, max_lag = 10L, max_cells = 3000000L)
```

## Arguments

- fit:

  Fitted pupil model.

- ndraws:

  Draws used for expected-value residual summaries.

- max_lag:

  Maximum residual ACF lag.

- max_cells:

  Maximum draw-by-observation cells.

## Value

A `gp3bayes_pupil_diagnostics`.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```
