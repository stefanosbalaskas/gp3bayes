# Compare gp3bayes Object Schemas

Compare gp3bayes Object Schemas

## Usage

``` r
compare_gp3bayes_schemas(x, y, compare_lengths = FALSE)
```

## Arguments

- x:

  A gp3bayes object or captured schema.

- y:

  A gp3bayes object or captured schema.

- compare_lengths:

  Whether vector/list lengths are part of the structural compatibility
  rule. The default is `FALSE` because analysis-specific cardinalities
  such as numbers of predictors or groups may legitimately differ while
  the serialized object contract remains compatible.

## Value

A `gp3bayes_schema_comparison`.
