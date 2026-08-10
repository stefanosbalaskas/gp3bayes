# Capture the Structural Schema of a gp3bayes Object

Captures classes, types, lengths, and field names without storing the
object's values. The result is intended for release compatibility
auditing, not for validating numerical or statistical equivalence.

## Usage

``` r
capture_gp3bayes_schema(x, max_depth = 3L)
```

## Arguments

- x:

  A gp3bayes object.

- max_depth:

  Maximum nested list depth to record.

## Value

A `gp3bayes_object_schema`.

## Examples

``` r
contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  condition_col = "condition"
)
capture_gp3bayes_schema(contract)
#> <gp3bayes_object_schema>
#>   Object class: gp3bayes_model_contract
#>   Recorded nodes: 41
#>   Maximum depth: 3
#>   Values recorded: FALSE
```
