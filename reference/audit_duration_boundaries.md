# Audit Duration Range and Censoring Boundaries

Checks an explicitly declared plausible measurement range and an
explicitly supplied censoring indicator. Candidate censoring-like column
names are only reported when no indicator is supplied; they are not
interpreted silently.

## Usage

``` r
audit_duration_boundaries(
  data,
  contract,
  allowed_range = NULL,
  censor_col = NULL,
  detect_candidate_columns = TRUE
)
```

## Arguments

- data:

  A data frame.

- contract:

  An approved duration contract.

- allowed_range:

  Optional positive lower and upper bounds in the contract's recorded
  outcome unit.

- censor_col:

  Optional censoring-indicator column.

- detect_candidate_columns:

  Whether common censoring/truncation names should be reported for
  review.

## Value

A `gp3bayes_duration_boundary_audit` object.
