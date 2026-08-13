# Random-Intercept Latent Variance Partition

For binary logit models the residual latent variance is `pi^2 / 3`. For
lognormal duration models residual log-scale variance is `sigma^2`.
Random-slope variance is deliberately excluded.

## Usage

``` r
random_intercept_variance_partition(fit, probs = c(0.025, 0.5, 0.975))
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- probs:

  Three interval probabilities.

## Value

A `gp3bayes_random_intercept_variance_partition`.
