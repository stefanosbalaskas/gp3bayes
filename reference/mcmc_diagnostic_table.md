# Modern MCMC Diagnostic Table

Computes rank-normalised R-hat, bulk ESS, tail ESS, and Monte Carlo
standard error summaries through the `posterior` package.

## Usage

``` r
mcmc_diagnostic_table(fit, variables = NULL, regex = NULL)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- variables:

  Optional exact posterior variable names.

- regex:

  Optional posterior-variable regular expression.

## Value

A data frame with posterior diagnostics by variable.
