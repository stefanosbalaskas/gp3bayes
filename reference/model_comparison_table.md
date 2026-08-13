# LOO Model-Comparison Table

LOO Model-Comparison Table

## Usage

``` r
model_comparison_table(x)
```

## Arguments

- x:

  A `gp3bayes_loo_comparison` or matrix returned by
  [`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html).

## Value

A data frame retaining ELPD differences and their standard errors.
