# Fitting hierarchical pupil time-course models

## Approved family

The first direct pupil model family is Gaussian with an identity link.
It supports a governed temporal trajectory, optional condition-specific
trajectory, participant hierarchy, optional item hierarchy, declared
numeric nuisance covariates, and optional AR(1) dependence for
sufficiently regular within-trial sampling.

``` r

sim <- simulate_pupil_timecourse(
  n_participants = 5,
  trials_per_participant = 4,
  sampling_frequency = 20,
  time_window = c(-0.4, 1.2),
  baseline_window = c(-0.4, 0),
  blink_trial_probability = 0,
  seed = 2026
)
contract <- create_pupil_contract(
  outcome_col = "pupil_mm",
  participant_col = "participant_id",
  trial_col = "trial_id",
  item_col = "item_id",
  condition_col = "condition",
  time_col = "event_time",
  pupil_unit = "millimetres",
  sampling_frequency = 20,
  eye = "combined"
)
prepared <- prepare_pupil_timecourse(sim$data, contract)
spec <- specify_pupil_timecourse_model(
  prepared,
  temporal_structure = "smooth",
  smooth_basis_dimension = 5,
  condition_trajectory = TRUE,
  autocorrelation = "none"
)
spec
#> <gp3bayes_pupil_model_specification>
#>   Family: Gaussian pupil time-course
#>   Formula: .pupil_model ~ .condition + s(.event_time, by = .condition, k = 5) +      (1 | .participant) + (1 | .item)
#>   Temporal structure: smooth
#>   Condition trajectory: TRUE
#>   Autocorrelation: none
#>   Outcome unit: millimetres
#>   Baseline: none
#>   Unrestricted formula: FALSE
#>   Fit performed: FALSE
```

## Translation without fitting

``` r

translation <- translate_pupil_model_to_brms(spec)
translation$formula
translation$prior
```

Translation is restricted. The user does not provide an arbitrary
formula, family, Stan program, algorithm, or open-ended backend argument
list.

## Prior-predictive gate

Prior-predictive execution is governed separately from posterior
fitting. The default call records the approved prior-only plan and does
not compile Stan.

``` r

prior_plan <- check_pupil_prior_predictive(
  spec,
  execute = FALSE,
  draws = 100,
  chains = 2,
  iter = 200,
  warmup = 100
)
as.data.frame(prior_plan)
#>          field       value
#> 1       family    Gaussian
#> 2      backend       rstan
#> 3        draws         100
#> 4       chains           2
#> 5         iter         200
#> 6       warmup         100
#> 7        cores           2
#> 8 outcome_unit millimetres
#> 9      execute       FALSE
```

A researcher can set `execute = TRUE` with either approved backend
during manual analysis. The operation never changes priors automatically
and its evidence does not certify model adequacy.

## Full-MCMC backends

Real fitting is optional and requires `brms` plus one approved backend.

``` r

fit_rstan <- fit_pupil_model_backend(
  spec,
  backend = "rstan",
  chains = 2,
  iter = 1000,
  warmup = 500,
  cores = 2,
  seed = 20260814
)

fit_cmdstanr <- fit_pupil_model_backend(
  spec,
  backend = "cmdstanr",
  chains = 2,
  iter = 1000,
  warmup = 500,
  cores = 2,
  seed = 20260814
)
```

The wrappers preserve a common gp3bayes object shape. A fitted object
does not by itself establish convergence, adequacy, measurement
validity, or a causal interpretation.
