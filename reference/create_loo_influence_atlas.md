# Create a LOO Influence Atlas

Create a LOO Influence Atlas

## Usage

``` r
create_loo_influence_atlas(x, data = NULL, threshold = 0.7)
```

## Arguments

- x:

  A gp3bayes PSIS-LOO or raw `loo` object.

- data:

  Optional observation-level data.

- threshold:

  Pareto-k threshold used for the flagged table.

## Value

A `gp3bayes_loo_influence_atlas`.
