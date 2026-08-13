# Posterior Area Plot

Posterior Area Plot

## Usage

``` r
plot_posterior_areas(
  x,
  variables = NULL,
  regex = NULL,
  prob = 0.5,
  prob_outer = 0.95
)
```

## Arguments

- x:

  A gp3bayes fit or posterior draws accepted by
  [`posterior_interval_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/posterior_interval_table.md).

- variables, regex:

  Posterior variable selectors.

- prob:

  Inner interval probability.

- prob_outer:

  Outer interval probability.

## Value

A `ggplot` object.
