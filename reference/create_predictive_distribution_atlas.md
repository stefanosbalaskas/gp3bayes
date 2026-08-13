# Create a Posterior-Predictive Distribution Atlas

Create a Posterior-Predictive Distribution Atlas

## Usage

``` r
create_predictive_distribution_atlas(
  fit,
  ndraws = 500L,
  include_group_effects = TRUE,
  seed = 1L
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- ndraws:

  Number of posterior predictive draws.

- include_group_effects:

  Whether fitted group effects are included.

- seed:

  Predictive seed.

## Value

A `gp3bayes_predictive_distribution_atlas`.
