# Summarise Posterior Estimand Draws

Summarise Posterior Estimand Draws

## Usage

``` r
summarise_estimand_draws(x, quantities = NULL, probs = c(0.025, 0.5, 0.975))
```

## Arguments

- x:

  A `gp3bayes_estimand` or finite numeric vector.

- quantities:

  Optional estimand-draw columns to summarise.

- probs:

  Three probabilities defining lower, middle, and upper summaries.

## Value

A data frame.
