# Extract Posterior Draws from a gp3bayes Fit

Converts the fitted `brms` posterior into a standard `posterior` draws
representation without changing the fitted model.

## Usage

``` r
extract_posterior_draws(
  fit,
  variables = NULL,
  regex = NULL,
  format = c("array", "matrix", "df", "rvars")
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- variables:

  Optional exact posterior variable names.

- regex:

  Optional regular expression used to retain posterior variables.

- format:

  One of `"array"`, `"matrix"`, `"df"`, or `"rvars"`.

## Value

A posterior draws object in the requested format.
