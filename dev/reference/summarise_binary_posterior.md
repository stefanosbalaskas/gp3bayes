# Summarise a Binary Posterior

Reports posterior location, uncertainty intervals, R-hat, effective
sample sizes, probability of a positive coefficient, and odds-ratio
transforms for population-level coefficients.

## Usage

``` r
summarise_binary_posterior(fit, probability = 0.95, variables = NULL)
```

## Arguments

- fit:

  A `gp3bayes_binary_fit`.

- probability:

  Central posterior interval probability.

- variables:

  Optional supported posterior variable names.

## Value

A `gp3bayes_binary_posterior_summary`.

## Details

Probability-positive values and intervals are descriptive posterior
summaries. They are not frequentist significance tests and do not
establish causal or substantive validity.
