# Posterior Pairs Plot

Posterior Pairs Plot

## Usage

``` r
plot_posterior_pairs(fit, variables = NULL, regex = "^b_", max_variables = 8L)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- variables:

  Optional variables.

- regex:

  Optional variable regular expression.

- max_variables:

  Maximum number of variables displayed.

## Value

A bayesplot pairs object.
