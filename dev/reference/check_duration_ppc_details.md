# Run Detailed Duration Posterior Predictive Checks

Adds raw/log-scale distributions, median and upper-quantile summaries,
tail exceedance, group medians, and within-participant focal-condition
median ratios.

## Usage

``` r
check_duration_ppc_details(
  fit,
  draws = 300L,
  seed = 1L,
  quantiles = c(0.5, 0.9, 0.95),
  tail_threshold = NULL
)
```

## Arguments

- fit:

  Approved duration fit.

- draws:

  Number of posterior predictive draws.

- seed:

  Random seed.

- quantiles:

  Predictive quantiles to report.

- tail_threshold:

  Optional substantive tail threshold in the analysis unit. If omitted,
  the observed 95th percentile is used descriptively.

## Value

A `gp3bayes_duration_ppc_detail`.
