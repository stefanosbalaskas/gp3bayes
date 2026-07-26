# Identify Influential PSIS-LOO Observations

Identify Influential PSIS-LOO Observations

## Usage

``` r
identify_loo_influential_observations(x, threshold = NULL, data = NULL)
```

## Arguments

- x:

  A `gp3bayes_psis_loo` or raw loo object.

- threshold:

  Optional explicit Pareto-k threshold.

- data:

  Optional observation-level data to append.

## Value

A data frame of flagged observations.
