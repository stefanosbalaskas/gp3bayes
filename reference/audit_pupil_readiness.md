# Audit pupil-timecourse readiness

Produces observable evidence about hierarchy, sampling, missingness,
baseline support, measurement flags, gaze/PFE context, luminance, and
preprocessing provenance. Review signals are not exclusion decisions.

## Usage

``` r
audit_pupil_readiness(x, contract = NULL)
```

## Arguments

- x:

  A `gp3bayes_pupil_prepared` object or raw data with `contract`.

- contract:

  Required only when `x` is raw data.

## Value

A `gp3bayes_pupil_readiness` object with summary and stratified tables.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```
