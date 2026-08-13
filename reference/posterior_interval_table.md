# Posterior Interval Table

Posterior Interval Table

## Usage

``` r
posterior_interval_table(
  x,
  variables = NULL,
  regex = NULL,
  probs = c(0.025, 0.5, 0.975)
)
```

## Arguments

- x:

  A gp3bayes fit, posterior draws object, numeric matrix, or numeric
  data frame.

- variables:

  Optional exact posterior variable names.

- regex:

  Optional regular expression for posterior variable names.

- probs:

  Three probabilities defining lower, median, and upper summaries.

## Value

A data frame containing posterior location, spread, and intervals.

## Examples

``` r
draws <- cbind(alpha = rnorm(200), beta = rnorm(200, 0.5))
posterior_interval_table(draws)
#>   variable      mean        sd     lower    median    upper
#> 1    alpha 0.1127221 1.0326214 -1.742285 0.1440568 2.129558
#> 2     beta 0.4579440 0.9883658 -1.250047 0.4082620 2.421420
```
