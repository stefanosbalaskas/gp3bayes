# Specify a joint binocular pupil model

Specify a joint binocular pupil model

## Usage

``` r
specify_binocular_pupil_model(
  prepared,
  temporal_structure = c("smooth", "linear", "gaussian_process"),
  family = c("gaussian", "student"),
  smooth_basis_dimension = 10L,
  gp_spec = create_pupil_gp_spec(),
  residual_correlation = TRUE,
  item_effects = NULL,
  prior_scales = NULL,
  allow_high_complexity = FALSE
)
```

## Arguments

- prepared:

  A binocular prepared object.

- temporal_structure:

  Mean trajectory type.

- family:

  Gaussian or Student-t for both eyes.

- smooth_basis_dimension:

  Requested smooth basis dimension; when omitted, an unsupported default
  is conservatively reduced to observed temporal support, while
  explicitly unsupported values are rejected.

- gp_spec:

  GP configuration when requested.

- residual_correlation:

  Whether to estimate left/right residual correlation.

- item_effects:

  Include item random intercepts when available.

- prior_scales:

  Optional prior-scale overrides.

- allow_high_complexity:

  Explicit computational opt-in for exact GP on large time grids.

## Value

A `gp3bayes_binocular_pupil_specification` object.
