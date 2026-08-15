# Predict governed pupil trajectories

Obtains expected, posterior-predictive, or linear-predictor draws from
an approved pupil fit with explicit draw and grid-size guards.

## Usage

``` r
predict_pupil_trajectory(
  fit,
  newdata = NULL,
  type = c("expected", "posterior_predictive", "linear"),
  ndraws = 500L,
  population_only = TRUE,
  allow_new_levels = FALSE,
  max_grid = 5000L,
  max_cells = 5000000L
)
```

## Arguments

- fit:

  A `gp3bayes_pupil_fit`.

- newdata:

  Optional prepared prediction grid. When omitted, a compact population
  grid is built from observed event times and conditions.
  Participant-conditioned prediction requires explicit `newdata`.

- type:

  `"expected"`, `"posterior_predictive"`, or `"linear"`.

- ndraws:

  Maximum posterior draws to retain.

- population_only:

  If `TRUE`, group-level effects are excluded from the prediction via
  `re_formula = NA`.

- allow_new_levels:

  Passed conservatively to brms prediction methods.

- max_grid:

  Maximum grid rows.

- max_cells:

  Maximum draw-by-grid cells.

## Value

A `gp3bayes_pupil_prediction`.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```
