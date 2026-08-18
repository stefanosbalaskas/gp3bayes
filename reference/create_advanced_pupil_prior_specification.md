# Create an advanced pupil prior specification

Constructs the governed default prior specification used when
translating an advanced pupil time-course model to `brms`. This function
defines prior scales only; it does not compile or fit a model and does
not establish model adequacy.

## Usage

``` r
create_advanced_pupil_prior_specification(specification)
```

## Arguments

- specification:

  A `gp3bayes_pupil_advanced_specification` created by
  [`specify_advanced_pupil_timecourse_model()`](https://stefanosbalaskas.github.io/gp3bayes/reference/specify_advanced_pupil_timecourse_model.md).

## Value

A `gp3bayes_pupil_advanced_prior_specification` object.
