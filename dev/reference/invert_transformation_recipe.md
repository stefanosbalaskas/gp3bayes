# Invert a Recorded Transformation Recipe

Reconstructs a raw-scale representation from prepared data when the
recorded transformations are invertible. This is intended for replay
tests and controlled sensitivity construction, not recovery of discarded
rows.

## Usage

``` r
invert_transformation_recipe(data, recipe)
```

## Arguments

- data:

  Prepared-scale data.

- recipe:

  A transformation recipe or prepared object.

## Value

A raw-scale data frame.
