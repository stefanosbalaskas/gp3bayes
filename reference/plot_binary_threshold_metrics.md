# Binary Threshold-Metric Plot

Binary Threshold-Metric Plot

## Usage

``` r
plot_binary_threshold_metrics(
  x,
  observed = NULL,
  thresholds = seq(0.1, 0.9, by = 0.05)
)
```

## Arguments

- x:

  Threshold-metric table, binary expected prediction, or numeric
  probabilities.

- observed:

  Optional observed outcomes for numeric predictions.

- thresholds:

  Thresholds to evaluate when `x` is not already a table.

## Value

A `ggplot` object.
