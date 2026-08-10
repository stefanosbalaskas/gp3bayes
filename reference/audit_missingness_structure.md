# Audit Missingness Structure Before Model Fitting

Summarises missing values in declared analysis columns and, where
possible, by participant, item, and condition. This is a reporting
audit; no rows are dropped or imputed.

## Usage

``` r
audit_missingness_structure(
  x,
  contract = NULL,
  review_fraction = 0.05,
  fail_fraction = 0.2
)
```

## Arguments

- x:

  A data frame, prepared object, model specification, or fit.

- contract:

  Required when `x` is a raw data frame.

- review_fraction:

  Column-level missing fraction above which a component is marked for
  review.

- fail_fraction:

  Column-level missing fraction above which a component is marked fail.
  A fail does not automatically exclude data.

## Value

A `gp3bayes_missingness_audit`.

## Examples

``` r
data <- data.frame(
  participant_id = rep(c("p1", "p2"), each = 4),
  trial_id = rep(1:4, 2),
  condition = rep(c("control", "treatment"), 4),
  selected = c(0, 1, NA, 1, 1, 0, 1, 0)
)
contract <- create_model_contract(
  "binary", "selected", "participant_id",
  trial_col = "trial_id", condition_col = "condition"
)
audit_missingness_structure(data, contract)
#> <gp3bayes_missingness_audit>
#>   Status: review
#>   Rows: 8
#>   Missing cells: 1
```
