# Simulate joint binocular pupil traces

Simulate joint binocular pupil traces

## Usage

``` r
simulate_binocular_pupil_timecourse(
  ...,
  residual_correlation = 0.65,
  eye_bias = 0.015,
  eye_specific_sd = 0.035
)
```

## Arguments

- ...:

  Arguments passed to
  [`simulate_advanced_pupil_timecourse()`](https://stefanosbalaskas.github.io/gp3bayes/reference/simulate_advanced_pupil_timecourse.md).

- residual_correlation:

  Approximate left/right innovation correlation.

- eye_bias:

  Mean right-minus-left difference.

- eye_specific_sd:

  Eye-specific noise SD.

## Value

A `gp3bayes_binocular_pupil_simulation` object.
