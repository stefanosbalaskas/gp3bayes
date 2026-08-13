# Posterior Direction and ROPE Probability Table

Posterior Direction and ROPE Probability Table

## Usage

``` r
posterior_probability_table(x, variables = NULL, regex = NULL, rope = NULL)
```

## Arguments

- x:

  A gp3bayes fit or posterior draws accepted by
  [`posterior_interval_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/posterior_interval_table.md).

- variables, regex:

  Posterior variable selectors.

- rope:

  Optional two-element interval defining a region of practical
  equivalence. It is descriptive only.

## Value

A data frame with posterior direction probabilities and, when requested,
the posterior probability inside the supplied interval.

## Examples

``` r
draws <- cbind(alpha = rnorm(500), beta = rnorm(500, 0.4))
posterior_probability_table(draws, rope = c(-0.1, 0.1))
#>       variable probability_gt_zero probability_lt_zero probability_in_rope
#> alpha    alpha               0.490               0.510               0.098
#> beta      beta               0.662               0.338               0.078
#>       rope_lower rope_upper
#> alpha       -0.1        0.1
#> beta        -0.1        0.1
```
