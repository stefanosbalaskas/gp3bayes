# Audit Prediction Support

Compares requested prediction rows with the observed model-building
support. It reports extrapolation and novel levels but never removes
prediction rows.

## Usage

``` r
audit_prediction_support(fit, newdata)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- newdata:

  Data to audit.

## Value

A `gp3bayes_prediction_support` object.
