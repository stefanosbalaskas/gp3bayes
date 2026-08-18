# Declare an advanced pupil observation distribution

Declare an advanced pupil observation distribution

## Usage

``` r
specify_pupil_distribution(
  family = c("gaussian", "student"),
  residual_scale = c("constant", "condition", "time", "condition_time")
)
```

## Arguments

- family:

  Gaussian or Student-t.

- residual_scale:

  Constant, condition-, time-, or condition-by-time scale.

## Value

A `gp3bayes_pupil_distribution_spec` object.
