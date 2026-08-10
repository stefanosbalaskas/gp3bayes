# Audit the Fixed-Effects Design Matrix

Checks rank, singular values, condition number, invariant columns and
extreme leverage before fitting Stan. The audit never rewrites a formula
or drops a predictor automatically.

## Usage

``` r
audit_fixed_effect_design(
  x,
  contract = NULL,
  condition_number_review = 30,
  condition_number_fail = 100,
  leverage_multiplier = 3
)
```

## Arguments

- x:

  A data frame, prepared object, model specification, or fit.

- contract:

  Required when `x` is a raw data frame.

- condition_number_review:

  Condition number triggering review.

- condition_number_fail:

  Condition number triggering fail.

- leverage_multiplier:

  Review observations whose hat value is above
  `leverage_multiplier * p / n`.

## Value

A `gp3bayes_fixed_effect_design_audit`.
