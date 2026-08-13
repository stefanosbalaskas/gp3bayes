# Posterior Correlation Table

Posterior Correlation Table

## Usage

``` r
posterior_correlation_table(
  x,
  variables = NULL,
  regex = NULL,
  method = c("pearson", "spearman")
)
```

## Arguments

- x:

  A gp3bayes fit or posterior draws accepted by
  [`posterior_interval_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/posterior_interval_table.md).

- variables, regex:

  Posterior variable selectors.

- method:

  Correlation method.

## Value

A long data frame of unique posterior-draw correlations.

## Examples

``` r
x <- cbind(a = rnorm(100), b = rnorm(100), c = rnorm(100))
posterior_correlation_table(x)
#>   variable_1 variable_2 correlation  method
#> 1          b          a  0.03816461 pearson
#> 2          c          a -0.15600779 pearson
#> 3          c          b -0.08591344 pearson
```
