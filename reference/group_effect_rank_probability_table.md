# Group-Level Rank-Probability Table

Group-Level Rank-Probability Table

## Usage

``` r
group_effect_rank_probability_table(
  fit,
  group,
  coefficient = "Intercept",
  ndraws = 1000L,
  seed = 1L
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- group:

  One grouping-factor name.

- coefficient:

  One group-level coefficient.

- ndraws:

  Number of posterior draws.

- seed:

  Draw-subsampling seed.

## Value

Descriptive posterior rank probabilities. Rank 1 is the largest
group-level deviation.
