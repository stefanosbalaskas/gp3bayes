# Rank-Diagnostic Plot

Rank-Diagnostic Plot

## Usage

``` r
plot_rank_diagnostics(fit, variables = NULL, regex = "^b_")
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- variables:

  Optional posterior variables.

- regex:

  Optional posterior-variable regular expression.

## Value

A `ggplot` rank-overlay diagnostic.
