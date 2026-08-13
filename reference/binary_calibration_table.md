# Binary Calibration Table

Binary Calibration Table

## Usage

``` r
binary_calibration_table(x, bins = 10L, probs = c(0.025, 0.5, 0.975))
```

## Arguments

- x:

  A binary expected-response `gp3bayes_prediction`.

- bins:

  Number of equal-frequency calibration bins.

- probs:

  Posterior interval probabilities.

## Value

A data frame comparing observed event rates with posterior mean event
probabilities by bin.
