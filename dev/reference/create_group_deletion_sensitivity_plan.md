# Create a Group-Deletion Sensitivity Plan

Create a Group-Deletion Sensitivity Plan

## Usage

``` r
create_group_deletion_sensitivity_plan(
  specification,
  group = c("participant", "item"),
  units = NULL,
  max_units = 20L
)
```

## Arguments

- specification:

  An approved binary or duration specification.

- group:

  Either participant or item.

- units:

  Optional explicit group levels. If omitted all levels are used only
  when their count does not exceed `max_units`.

- max_units:

  Maximum automatic number of omission fits.

## Value

A `gp3bayes_group_deletion_sensitivity_plan`.
