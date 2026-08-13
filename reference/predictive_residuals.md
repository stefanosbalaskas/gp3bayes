# Posterior Predictive Residuals

Posterior Predictive Residuals

## Usage

``` r
predictive_residuals(fit, type = NULL, ndraws = 1000L)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- type:

  Residual type. Binary models support `"raw"` and `"pearson"`; duration
  models support `"raw"`, `"log"`, and `"relative"`.

- ndraws:

  Optional number of expected-response draws.

## Value

A data frame of observed values, posterior expected values, and
descriptive residuals.
