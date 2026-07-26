# Advanced Optional Bayesian Workflows

This article describes the optional post-0.1.1 extensions. They remain
contract-first: neither unrestricted formulas nor automatic model
selection are introduced.

## Capability audit

``` r

bayesian_backend_capabilities()
#> 
#> Optional Bayesian capabilities
#> 
#>         component installed    version usable
#>              brms      TRUE     2.23.0   TRUE
#>             rstan      TRUE     2.32.7   TRUE
#>          cmdstanr      TRUE      0.9.0  FALSE
#>               loo      TRUE     2.10.1   TRUE
#>        priorsense      TRUE      1.2.0   TRUE
#>  detectseparation      TRUE      0.4.0   TRUE
#>               SBC      TRUE 0.5.0.9000   TRUE
#>                                                     detail
#>                                          package available
#>                                          package available
#>  CmdStan path has not been set yet. See ?set_cmdstan_path.
#>                                          package available
#>                                          package available
#>                                          package available
#>                                          package available
```

## Separate interaction priors

``` r

binary_sim <- simulate_hierarchical_binary_data(
  n_participants = 12,
  trials_per_participant = 8,
  n_items = 6,
  random_slope_sd = 0,
  seed = 2026
)

binary_contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  predictors = c("participant_covariate", "trial_covariate"),
  interaction = c("condition", "participant_covariate"),
  random_slope = FALSE
)

binary_prepared <- prepare_hierarchical_binary_data(
  binary_sim$data,
  binary_contract,
  condition_levels = c("control", "treatment")
)

binary_spec <- specify_binary_model_with_interaction_prior(
  binary_prepared,
  baseline = 0.35
)

interaction_prior_summary(binary_spec)
#>   family                     interaction main_effect_scale interaction_scale
#> 1 binary condition:participant_covariate              0.75               0.5
#>   interaction_tag
#> 1     interaction
```

The binary advanced default is `normal(0, 0.75)` for population main
effects and `normal(0, 0.50)` for the single approved interaction. The
duration advanced defaults are 0.35 and 0.25 respectively. These are
candidate workflow defaults and still require prior-predictive review.

## Full-MCMC backend selection

The advanced fitting functions accept only `rstan` or `cmdstanr`, and
they always use full MCMC sampling.

``` r

fit_rstan <- fit_binary_model_backend(
  binary_spec,
  backend = "rstan"
)

fit_cmdstanr <- fit_binary_model_backend(
  binary_spec,
  backend = "cmdstanr"
)
```

## Separation screening

``` r

separation_screen <- detect_binary_separation(binary_spec)
separation_screen
#> 
#> Binary separation screen
#>  Status: pass
#>  Separation detected: FALSE
#>  Observations: 96
#>                      coefficient separation_code infinite direction
#>                      (Intercept)               0    FALSE    finite
#>                        condition               0    FALSE    finite
#>            participant_covariate               0    FALSE    finite
#>                  trial_covariate               0    FALSE    finite
#>  condition:participant_covariate               0    FALSE    finite
#> 
#> This is a fixed-effects logistic separation screen. Grouping terms from the hierarchical specification are not included in this screening GLM. The screen neither fits nor validates the hierarchical Bayesian model.
plot(separation_screen)
```

![](advanced-optional-workflows_files/figure-html/unnamed-chunk-4-1.png)

The screen is a fixed-effects design diagnostic. It is not a replacement
for the hierarchical Bayesian fit or its posterior diagnostics.

## PSIS-LOO and model averaging

``` r

loo_a <- compute_psis_loo(fit_a)
loo_b <- compute_psis_loo(fit_b)

comparison <- compare_psis_loo(list(contract_a = loo_a, contract_b = loo_b))
comparison

weights <- compute_loo_model_weights(comparison, method = "stacking")
weights
```

The comparison reports predictive differences and diagnostics but never
selects a model automatically.

## Power-scaling sensitivity

``` r

sensitivity <- assess_powerscaled_sensitivity(
  fit_rstan,
  variable = c("b_Intercept", "b_condition")
)

sensitivity
plot(sensitivity, type = "ecdf")
plot(sensitivity, type = "quantities")
```

Low local sensitivity is not a proof of universal robustness.

## Simulation-based calibration

``` r

plan <- create_brms_sbc_plan(
  binary_spec,
  n_sims = 50,
  backend = "cmdstanr"
)

sbc_result <- run_sbc_plan(plan)
sbc_result
plot(sbc_result, type = "rank")
plot(sbc_result, type = "ecdf")
```

The brms generator and brms inference backend share implementation code.
An independently coded generator is preferable when the goal is to
identify shared implementation defects.
