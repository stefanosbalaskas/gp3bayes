# Create a Governed Prediction Grid

Creates a Cartesian prediction grid from declared model predictors.
Numeric covariates are held at an observed typical value unless values
are supplied explicitly through `at`.

## Usage

``` r
create_prediction_grid(
  x,
  variables = NULL,
  at = list(),
  numeric_at = c("median", "mean"),
  max_rows = 5000L
)
```

## Arguments

- x:

  A gp3bayes fit or approved model specification.

- variables:

  Optional variables to vary. By default the declared condition and
  non-identifier predictors are considered.

- at:

  Named list of explicit values for selected variables.

- numeric_at:

  One of `"median"` or `"mean"` for numeric covariates not supplied in
  `at`.

- max_rows:

  Maximum permitted grid size.

## Value

A data frame suitable for
[`predict_model()`](https://stefanosbalaskas.github.io/gp3bayes/reference/predict_model.md).
