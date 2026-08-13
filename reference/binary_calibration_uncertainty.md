# Binary Calibration Uncertainty

Equal-width bins are defined from posterior-mean predicted
probabilities.

## Usage

``` r
binary_calibration_uncertainty(
  fit,
  newdata = NULL,
  bins = 10L,
  include_group_effects = FALSE,
  ndraws = 1000L,
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- fit:

  A fitted binary `gp3bayes_fit`.

- newdata:

  Optional data containing observed outcomes.

- bins:

  Number of equal-width probability bins.

- include_group_effects:

  Whether fitted group effects are included.

- ndraws:

  Expected-probability posterior draws.

- probs:

  Three interval probabilities.

## Value

A `gp3bayes_binary_calibration_uncertainty`.
