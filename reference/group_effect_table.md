# Group-Level Effect Table

Group-Level Effect Table

## Usage

``` r
group_effect_table(fit, groups = NULL, probs = c(0.025, 0.975))
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- groups:

  Optional grouping factors to retain.

- probs:

  Lower and upper credible interval probabilities.

## Value

A tidy data frame of estimated group-level deviations.
