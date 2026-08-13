# Group-Level Posterior Draw Table

Group-Level Posterior Draw Table

## Usage

``` r
group_effect_draws_table(
  fit,
  groups = NULL,
  coefficients = NULL,
  ndraws = NULL,
  seed = 1L,
  max_rows = 1000000L
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- groups:

  Optional grouping factors.

- coefficients:

  Optional group-level coefficients.

- ndraws:

  Optional number of draws retained.

- seed:

  Seed used only for draw subsampling.

- max_rows:

  Maximum permitted long-format rows.

## Value

A long posterior draw table on the model linear-predictor scale.
