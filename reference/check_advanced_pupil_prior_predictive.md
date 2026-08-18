# Check advanced pupil priors through prior-only simulation

This optional backend gate samples only from priors using brms. It does
not establish model adequacy.

## Usage

``` r
check_advanced_pupil_prior_predictive(
  specification,
  backend = c("rstan", "cmdstanr"),
  chains = 2L,
  iter = 800L,
  warmup = 400L,
  cores = min(2L, chains),
  seed = 2026
)
```

## Arguments

- specification:

  An advanced specification.

- backend:

  `"rstan"` or `"cmdstanr"`.

- chains, iter, warmup, cores, seed:

  Sampling controls.

## Value

A `gp3bayes_pupil_advanced_prior_predictive` object.
