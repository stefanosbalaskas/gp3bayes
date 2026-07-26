# Detect Separation in the Binary Fixed-Effects Screen

Runs
[`detectseparation::detect_separation()`](https://rdrr.io/pkg/detectseparation/man/detect_separation.html)
as a pre-fit fixed-effects screen. This screen does not replace the
hierarchical model or prove that the Bayesian posterior is adequate.

## Usage

``` r
detect_binary_separation(x, formula = NULL, data = NULL)
```

## Arguments

- x:

  An approved binary specification, or a data frame when `formula` is
  supplied.

- formula:

  Optional fixed-effects binomial formula.

- data:

  Optional data frame. It overrides data extracted from `x`.

## Value

A `gp3bayes_separation_screen`.
