# Create a Gaussian-process configuration for pupil trajectories

Create a Gaussian-process configuration for pupil trajectories

## Usage

``` r
create_pupil_gp_spec(
  kernel = c("matern32", "matern52", "exp_quad"),
  basis = c("approximate", "exact"),
  k = 30L,
  scale = TRUE
)
```

## Arguments

- kernel:

  GP covariance kernel.

- basis:

  `"exact"` or `"approximate"`.

- k:

  Number of Hilbert-space basis functions when `basis = "approximate"`.

- scale:

  Whether brms should internally scale GP predictors.

## Value

A `gp3bayes_pupil_gp_spec` object.
