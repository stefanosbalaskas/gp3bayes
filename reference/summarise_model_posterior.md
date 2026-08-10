# Summarise an Approved gp3bayes Posterior

Family-neutral wrapper around
[`summarise_binary_posterior()`](https://stefanosbalaskas.github.io/gp3bayes/reference/summarise_binary_posterior.md)
and
[`summarise_duration_posterior()`](https://stefanosbalaskas.github.io/gp3bayes/reference/summarise_duration_posterior.md).

## Usage

``` r
summarise_model_posterior(fit, ...)
```

## Arguments

- fit:

  A `gp3bayes_fit`.

- ...:

  Family-specific diagnostic arguments.

## Value

A family-specific gp3bayes posterior summary.
