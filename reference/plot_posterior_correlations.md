# Posterior Correlation Plot

Posterior Correlation Plot

## Usage

``` r
plot_posterior_correlations(
  x,
  variables = NULL,
  regex = NULL,
  method = c("pearson", "spearman")
)
```

## Arguments

- x:

  A gp3bayes fit or posterior draws.

- variables, regex:

  Posterior variable selectors.

- method:

  Correlation method.

## Value

A `ggplot` heatmap of posterior-draw correlations.
