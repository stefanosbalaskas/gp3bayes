# Summarise Overall Condition Balance

Computes the observed proportion of each focal-condition level and
applies explicit review and failure thresholds. The thresholds are
workflow thresholds, not universal statistical laws.

## Usage

``` r
summarise_condition_balance(
  data,
  contract,
  warning_fraction = 0.1,
  failure_fraction = 0.02
)
```

## Arguments

- data:

  A data frame.

- contract:

  An approved model contract.

- warning_fraction:

  Minimum condition fraction below which review is requested.

- failure_fraction:

  Minimum condition fraction below which the strict readiness gate
  fails.

## Value

A `gp3bayes_condition_balance` object.
