# Compute LOO Model-Averaging Weights

Compute LOO Model-Averaging Weights

## Usage

``` r
compute_loo_model_weights(x, method = c("stacking", "pseudobma"), cores = 1L)
```

## Arguments

- x:

  A `gp3bayes_loo_comparison` or named list of PSIS-LOO objects.

- method:

  Either stacking or pseudo-BMA.

- cores:

  Number of cores.

## Value

A `gp3bayes_loo_weights` object.
