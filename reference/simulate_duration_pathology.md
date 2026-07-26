# Simulate a Dedicated Duration Pathology Scenario

Simulate a Dedicated Duration Pathology Scenario

## Usage

``` r
simulate_duration_pathology(
  scenario = c("null_ratio", "high_group_heterogeneity", "weak_information",
    "severe_imbalance", "heavy_tailed_contamination", "mixture", "censoring",
    "incorrect_unit", "zero_duration", "negative_duration"),
  seed = 1L
)
```

## Arguments

- scenario:

  Pathological or stress-test duration scenario.

- seed:

  Non-negative integer seed.

## Value

A `gp3bayes_pathological_simulation`.
