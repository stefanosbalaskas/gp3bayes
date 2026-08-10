# Validate an Object Against a Frozen gp3bayes Schema

Validate an Object Against a Frozen gp3bayes Schema

## Usage

``` r
validate_gp3bayes_schema(x, schema, strict = FALSE, compare_lengths = FALSE)
```

## Arguments

- x:

  A gp3bayes object.

- schema:

  A `gp3bayes_object_schema`.

- strict:

  Whether structural drift should raise an error.

- compare_lengths:

  Whether analysis-specific object lengths should be required to match
  the frozen schema.

## Value

A `gp3bayes_schema_validation` object.
