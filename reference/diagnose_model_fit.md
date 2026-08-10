# Diagnose an Approved gp3bayes Fit

Family-neutral wrapper around
[`diagnose_binary_fit()`](https://stefanosbalaskas.github.io/gp3bayes/reference/diagnose_binary_fit.md)
and
[`diagnose_duration_fit()`](https://stefanosbalaskas.github.io/gp3bayes/reference/diagnose_duration_fit.md).

## Usage

``` r
diagnose_model_fit(fit, ...)
```

## Arguments

- fit:

  A `gp3bayes_fit`.

- ...:

  Family-specific diagnostic arguments.

## Value

A family-specific `gp3bayes_sampling_diagnostics` object.
