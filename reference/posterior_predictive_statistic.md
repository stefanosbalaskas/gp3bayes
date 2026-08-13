# Posterior Predictive Statistic Check

Computes one scalar discrepancy statistic for every posterior predictive
draw and compares that distribution with the same statistic in the
observed data. The returned probability is descriptive and is not an
automatic model verdict.

## Usage

``` r
posterior_predictive_statistic(
  x,
  statistic = c("mean", "sd", "median", "q90", "q95", "max", "tail_rate"),
  threshold = NULL
)
```

## Arguments

- x:

  A posterior predictive `gp3bayes_prediction`.

- statistic:

  Built-in statistic: `"mean"`, `"sd"`, `"median"`, `"q90"`, `"q95"`,
  `"max"`, or `"tail_rate"`.

- threshold:

  Required for `"tail_rate"`.

## Value

A `gp3bayes_ppc_statistic` object.
