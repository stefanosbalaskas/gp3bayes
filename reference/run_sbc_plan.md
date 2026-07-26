# Run a Simulation-Based Calibration Plan

Run a Simulation-Based Calibration Plan

## Usage

``` r
run_sbc_plan(
  plan,
  cores_per_fit = 1L,
  keep_fits = FALSE,
  thin_ranks = NULL,
  cache_mode = "none",
  cache_location = NULL
)
```

## Arguments

- plan:

  A `gp3bayes_sbc_plan`.

- cores_per_fit:

  Cores used by each fit.

- keep_fits:

  Whether to retain fitted objects.

- thin_ranks:

  Optional rank thinning.

- cache_mode:

  SBC cache mode.

- cache_location:

  Optional cache location.

## Value

A `gp3bayes_sbc_result`.
