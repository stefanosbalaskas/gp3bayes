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
#>   variable        mean        sd     lower     median    upper
#> 1    alpha -0.07697793 1.0501184 -2.117753 -0.1242940 1.948645
#> 2     beta  0.36602203 0.8923331 -1.361916  0.3787615 2.119325
```
