# Variance-Component Posterior Table

Variance-Component Posterior Table

## Usage

``` r
variance_component_table(fit, probs = c(0.025, 0.5, 0.975))
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- probs:

  Three posterior interval probabilities.

## Value

A posterior summary table for group SDs, correlations, and residual
scale where applicable.
