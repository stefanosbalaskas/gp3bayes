# Create a governed missingness specification

Create a governed missingness specification

## Usage

``` r
create_pupil_missingness_spec(
  response = c("exclude", "model"),
  predictors = character(),
  assumptions = "MAR",
  auxiliary_predictors = character()
)
```

## Arguments

- response:

  Either `"exclude"` or `"model"`. `"model"` uses brms missing-response
  syntax and is interpreted under an explicitly declared MAR-oriented
  modelling assumption, not as proof that MAR holds.

- predictors:

  Character vector of predictor columns whose missing values should be
  modelled jointly.

- assumptions:

  Currently only `"MAR"` is accepted for modelling.

- auxiliary_predictors:

  Optional fully observed columns to use in the missing-predictor
  submodels.

## Value

A `gp3bayes_pupil_missingness_spec` object.
