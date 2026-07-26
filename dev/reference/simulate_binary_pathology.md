# Simulate a Dedicated Binary Pathology Scenario

Simulate a Dedicated Binary Pathology Scenario

## Usage

``` r
simulate_binary_pathology(
  scenario = c("null_contrast", "weak_information", "severe_imbalance",
    "near_separation", "omitted_random_slope", "sparse_item_structure",
    "all_zero_participants", "rank_deficiency", "missing_outcomes"),
  seed = 1L
)
```

## Arguments

- scenario:

  Pathological or stress-test scenario.

- seed:

  Non-negative integer seed.

## Value

A `gp3bayes_pathological_simulation`.
