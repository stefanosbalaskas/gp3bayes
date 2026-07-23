# Summarise a Duration Posterior

Reports posterior location, uncertainty, diagnostics, and multiplicative
duration-ratio transforms for population-level coefficients.

## Usage

``` r
summarise_duration_posterior(fit, probability = 0.95, variables = NULL)
```

## Arguments

- fit:

  A `gp3bayes_duration_fit`.

- probability:

  Central posterior interval probability.

- variables:

  Optional supported posterior variable names.

## Value

A `gp3bayes_duration_posterior_summary`.

## Details

Exponentiating a population-level coefficient gives its conditional
multiplicative effect on the median duration under the approved
lognormal model. This is not automatically a causal effect.
