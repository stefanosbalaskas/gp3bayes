# Compute explicit predictive model weights

Compute explicit predictive model weights

## Usage

``` r
pupil_model_weights(x, method = c("stacking", "pseudobma"), BB = TRUE)
```

## Arguments

- x:

  A LOO-based model comparison or a model set.

- method:

  `"stacking"` or `"pseudobma"`.

- BB:

  Bayesian bootstrap for pseudo-BMA where applicable.

## Value

A data frame of weights. Weights are not used automatically for
prediction or model selection.
