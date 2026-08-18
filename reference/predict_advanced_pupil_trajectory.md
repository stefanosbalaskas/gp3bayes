# Predict an advanced pupil trajectory

Predict an advanced pupil trajectory

## Usage

``` r
predict_advanced_pupil_trajectory(
  fit,
  newdata = NULL,
  type = c("expected", "posterior_predictive", "linear"),
  ndraws = 500L,
  population_only = TRUE,
  allow_new_levels = FALSE,
  max_grid = 5000L
)
```

## Arguments

- fit:

  A fitted advanced pupil model.

- newdata:

  Optional prediction data. If omitted, a population-level
  time-by-condition grid is generated with covariates held at reference
  values.

- type:

  `"expected"`, `"posterior_predictive"`, or `"linear"`.

- ndraws:

  Number of posterior draws.

- population_only:

  Exclude group-level effects when TRUE.

- allow_new_levels:

  Passed to brms prediction methods.

- max_grid:

  Maximum generated grid size.

## Value

A `gp3bayes_pupil_advanced_trajectory` object.
