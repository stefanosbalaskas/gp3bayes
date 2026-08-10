# Fit an Approved Hierarchical Binary Model

Fits an approved binary model specification using full MCMC sampling
through the fixed `brms` and `rstan` route.

## Usage

``` r
fit_binary_model(
  specification,
  chains = 4L,
  iter = 2000L,
  warmup = 1000L,
  cores = .gp3b_default_cores(chains),
  seed = 1L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L
)
```

## Arguments

- specification:

  A `gp3bayes_binary_model_specification`.

- chains:

  Number of MCMC chains.

- iter:

  Total iterations per chain, including warmup.

- warmup:

  Warmup iterations per chain.

- cores:

  Number of processor cores. It cannot exceed `chains`.

- seed:

  Non-negative integer random-number seed.

- adapt_delta:

  Target acceptance probability for the No-U-Turn sampler.

- max_treedepth:

  Maximum tree depth for the No-U-Turn sampler.

- refresh:

  Console progress refresh interval. Use zero to suppress iteration
  progress output.

## Value

A `gp3bayes_binary_fit` containing the fitted backend object, original
specification, restricted translation, and recorded sampling settings.

## Details

The function fixes the likelihood to Bernoulli, the link to logit, the
interface to `brms`, the sampling backend to `rstan`, and the algorithm
to full MCMC sampling. It does not expose arbitrary backend arguments.

A returned fit is not evidence of convergence, posterior adequacy,
causal identification, or substantive validity. Those assessments
require separate diagnostic and reporting gates.

## Examples

``` r
# \donttest{
if (
  requireNamespace("brms", quietly = TRUE) &&
    requireNamespace("rstan", quietly = TRUE) &&
    identical(
      validate_backend_environment(
        "rstan",
        compile_test = TRUE,
        strict = FALSE
      )$status,
      "pass"
    )
) {
  simulation <- simulate_hierarchical_binary_data(
    n_participants = 8,
    trials_per_participant = 6,
    n_items = 4,
    random_slope_sd = 0,
    seed = 2026
  )

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "item_id",
    trial_col = "trial_id",
    condition_col = "condition"
  )

  prepared <- prepare_hierarchical_binary_data(
    simulation$data,
    contract,
    condition_levels = c("control", "treatment")
  )

  specification <- specify_binary_model(prepared, baseline = 0.35)

  fit <- fit_binary_model(
    specification,
    chains = 2,
    iter = 200,
    warmup = 100,
    cores = 2,
    seed = 2026,
    refresh = 0
  )
}
# }
```
