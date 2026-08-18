# Estimate a dynamic posterior contrast between two pupil conditions

The contrast is evaluated pointwise on an explicitly supplied posterior
trajectory (or its derivative). No favorable time window is selected.

## Usage

``` r
estimate_pupil_dynamic_contrast(
  prediction,
  contrast,
  threshold = 0,
  probability = 0.95
)
```

## Arguments

- prediction:

  An advanced trajectory or derivative object.

- contrast:

  Character vector of exactly two condition levels: first minus second.

- threshold:

  Prespecified scientifically meaningful contrast threshold.

- probability:

  Central posterior interval probability.

## Value

A `gp3bayes_pupil_dynamic_contrast` object.
