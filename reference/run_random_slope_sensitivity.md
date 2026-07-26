# Run Random-Intercept versus Random-Slope Sensitivity

Run Random-Intercept versus Random-Slope Sensitivity

## Usage

``` r
run_random_slope_sensitivity(
  plan,
  backend = c("rstan", "cmdstanr"),
  chains = 4L,
  iter = 2000L,
  warmup = 1000L,
  cores = 1L,
  seed = 1L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L,
  ndraws = NULL,
  retain_fits = FALSE
)
```

## Arguments

- plan:

  A random-slope sensitivity plan.

- backend:

  `"rstan"` or `"cmdstanr"`.

- chains, iter, warmup, cores, seed, adapt_delta, max_treedepth,
  refresh:

  Restricted sampling controls.

- ndraws:

  Optional number of draws used for each estimand.

- retain_fits:

  Whether fitted objects are retained.

## Value

A `gp3bayes_random_slope_sensitivity`.
