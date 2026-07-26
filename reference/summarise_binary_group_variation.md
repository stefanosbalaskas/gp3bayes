# Summarise Binary Outcome Variation Within Groups

Identifies participants or items whose observed binary outcomes are all
zero or all one. Such groups are retained and reported; they are not
deleted.

## Usage

``` r
summarise_binary_group_variation(
  data,
  contract,
  group = c("participant", "item")
)
```

## Arguments

- data:

  A data frame.

- contract:

  An approved binary model contract.

- group:

  Either `"participant"` or `"item"`.

## Value

A `gp3bayes_binary_group_variation` object.
