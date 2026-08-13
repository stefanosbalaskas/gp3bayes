# Extract Pointwise Log-Likelihood Draws

Extract Pointwise Log-Likelihood Draws

## Usage

``` r
extract_log_likelihood(
  fit,
  newdata = NULL,
  include_group_effects = TRUE,
  ndraws = NULL
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- newdata:

  Optional prediction data. `NULL` uses the fitted data.

- include_group_effects:

  Whether fitted group-level effects are included.

- ndraws:

  Optional number of posterior draws.

## Value

A numeric matrix with posterior draws in rows and observations in
columns.
