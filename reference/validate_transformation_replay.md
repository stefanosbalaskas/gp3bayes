# Validate Exact Transformation Replay

Round-trips the prepared data through the recorded inverse and forward
transformation and compares transformed columns and fixed-effect
matrices.

## Usage

``` r
validate_transformation_replay(prepared, tolerance = 1e-10)
```

## Arguments

- prepared:

  A binary or duration prepared object.

- tolerance:

  Numeric comparison tolerance.

## Value

A `gp3bayes_transformation_replay_audit`.
