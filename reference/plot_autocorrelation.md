# Autocorrelation Diagnostic Plot

Autocorrelation Diagnostic Plot

## Usage

``` r
plot_autocorrelation(fit, variables = NULL, regex = "^b_", lags = 20L)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- variables:

  Optional posterior variables.

- regex:

  Optional posterior-variable regular expression.

- lags:

  Maximum autocorrelation lag.

## Value

A `ggplot` object.
