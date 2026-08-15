# Create a lightweight pupil prediction object from frozen draws

Wraps already-computed posterior prediction draws for examples,
reporting, and reproducible post-fit analysis without pretending that
fitting occurred in the current session.

## Usage

``` r
as_pupil_prediction_draws(
  draws,
  grid,
  unit,
  type = c("expected", "posterior_predictive", "linear"),
  max_cells = 5000000L
)
```

## Arguments

- draws:

  Numeric matrix with posterior draws in rows and grid points in
  columns.

- grid:

  Data frame with one row per draw column. It should contain
  `.event_time` and may contain `.condition`, `.participant`, and
  `.item`.

- unit:

  Declared pupil unit.

- type:

  Prediction type label.

- max_cells:

  Maximum draw-by-grid cells.

## Value

A `gp3bayes_pupil_prediction`.

## Examples

``` r
grid <- expand.grid(
  .event_time = seq(0, 1, length.out = 5),
  .condition = factor(c("control", "treatment"))
)
draws <- matrix(rnorm(1000), nrow = 100, ncol = nrow(grid))
prediction <- as_pupil_prediction_draws(
  draws, grid, unit = "millimetres"
)
```
