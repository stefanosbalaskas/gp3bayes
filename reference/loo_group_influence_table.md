# Aggregate LOO Influence by a Declared Group

Aggregate LOO Influence by a Declared Group

## Usage

``` r
loo_group_influence_table(x, group, data = NULL)
```

## Arguments

- x:

  A LOO influence atlas, pointwise LOO table, gp3bayes PSIS-LOO, or raw
  `loo` object.

- group:

  Grouping-column name.

- data:

  Optional observation-level data when needed.

## Value

A descriptive group-level influence table.
