# Summarise a fitted pupil posterior

Returns posterior location, uncertainty, R-hat, and ESS evidence for
model parameters without converting them into psychological constructs.

## Usage

``` r
summarise_pupil_posterior(fit, probability = 0.95)
```

## Arguments

- fit:

  A fitted pupil model.

- probability:

  Credible interval probability.

## Value

A `gp3bayes_pupil_posterior_summary`.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```
