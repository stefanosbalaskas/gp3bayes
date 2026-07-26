# Apply a Recorded Transformation Recipe

Apply a Recorded Transformation Recipe

## Usage

``` r
apply_transformation_recipe(
  new_data,
  recipe,
  input_scale = c("raw", "prepared"),
  require_outcome = FALSE,
  input_unit = NULL
)
```

## Arguments

- new_data:

  New data to transform.

- recipe:

  A `gp3bayes_transformation_recipe` or prepared gp3bayes object.

- input_scale:

  Either `"raw"` or `"prepared"`.

- require_outcome:

  Whether the outcome column must be present.

- input_unit:

  Optional source duration unit used to guard duration replay.

## Value

A transformed data frame with a recipe attribute.
