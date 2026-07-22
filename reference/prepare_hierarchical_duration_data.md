# Prepare Hierarchical Duration Data

Validates strictly positive finite uncensored durations, applies
explicit unit conversion and recorded scaling, and runs the duration
readiness gate.

## Usage

``` r
prepare_hierarchical_duration_data(
  data,
  contract,
  condition_levels = NULL,
  condition_coding = c(-0.5, 0.5),
  scale_predictors = character(),
  scale_time = FALSE,
  outcome_multiplier = 1,
  converted_unit = NULL,
  missing = c("error", "drop")
)
```

## Arguments

- data:

  A data frame containing columns declared in `contract`.

- contract:

  A duration `gp3bayes_model_contract`.

- condition_levels:

  Optional two-value condition order.

- condition_coding:

  Two distinct finite numeric condition codes.

- scale_predictors:

  Declared numeric predictors to centre and scale.

- scale_time:

  Whether to centre and scale the declared linear time variable.

- outcome_multiplier:

  Positive unit-conversion multiplier.

- converted_unit:

  Required destination unit when `outcome_multiplier != 1`.

- missing:

  Either `"error"` or `"drop"`.

## Value

A `gp3bayes_duration_prepared` object.

## Details

Zero, negative, non-finite, censored, truncated, or shifted durations
are not supported. Unit conversion is never inferred.
