# Posterior Exceedance Probabilities

Posterior Exceedance Probabilities

## Usage

``` r
prediction_exceedance_probability(
  x,
  threshold,
  direction = c("above", "below")
)
```

## Arguments

- x:

  A `gp3bayes_prediction`.

- threshold:

  Finite response-scale threshold.

- direction:

  Whether to evaluate values above or below the threshold.

## Value

Observation-level posterior exceedance probabilities.
