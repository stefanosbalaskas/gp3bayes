# Audit Random-Effects Support

Audits participant repetition, item crossing, and within-participant
condition support for a requested random slope.

## Usage

``` r
audit_random_effects_support(
  x,
  contract = NULL,
  minimum_repeated_rows = 2L,
  minimum_group_levels = 2L,
  minimum_condition_cell_rows = 2L
)
```

## Arguments

- x:

  A data frame, prepared object, model specification, or fit.

- contract:

  Required when `x` is a raw data frame.

- minimum_repeated_rows:

  Minimum observations per participant.

- minimum_group_levels:

  Minimum participant/item levels.

- minimum_condition_cell_rows:

  Minimum rows in each observed participant-condition cell when a random
  slope is requested.

## Value

A `gp3bayes_random_effects_support_audit`.
