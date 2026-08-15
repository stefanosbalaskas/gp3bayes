# Estimate a posterior condition-difference trajectory

Estimate a posterior condition-difference trajectory

## Usage

``` r
pupil_condition_contrast(
  prediction,
  contrast,
  threshold = 0,
  probability = 0.95
)
```

## Arguments

- prediction:

  Pupil prediction with a `.condition` column.

- contrast:

  Character vector `c(level_a, level_b)` defining `a - b`.

- threshold:

  Scientifically declared threshold on the pupil scale.

- probability:

  Credible probability.

## Value

A pupil trajectory/contrast object with pointwise probabilities that the
declared contrast exceeds `threshold`.

## Examples

``` r
grid <- expand.grid(
  .event_time = seq(0, 1, length.out = 5),
  .condition = factor(c("control", "treatment"))
)
draws <- matrix(rnorm(1000), nrow = 100, ncol = nrow(grid))
prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
pupil_condition_contrast(
  prediction, contrast = c("treatment", "control"), threshold = 0.1
)
#> <gp3bayes_pupil_estimand>
#>   Rows: 5
#>   Automatic significance decision: FALSE
```
