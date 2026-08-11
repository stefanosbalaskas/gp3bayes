# A Reproducible 0.2.0 Release Case Study

## Purpose

This case study exercises the stable 0.2.0 workflow on deterministic
synthetic data. The vignette evaluates every backend-independent stage
and leaves the optional Stan fits unevaluated so package documentation
remains portable.

## 1. Simulate known data

``` r

simulation <- simulate_hierarchical_binary_data(
  n_participants = 24,
  trials_per_participant = 12,
  n_items = 8,
  random_slope_sd = 0,
  seed = 202602
)
```

## 2. Declare the analysis contract

``` r

contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  predictors = "trial_covariate"
)

readiness <- audit_model_readiness(simulation$data, contract)
readiness
#> <gp3bayes_readiness_audit>
#>   Family: binary
#>   Rows: 288
#>   Status: ready
#>   Ready: TRUE
#>   Checks: 22 passed, 0 warnings, 0 failures
```

## 3. Preflight the design

``` r

design <- audit_design_support(
  simulation$data,
  contract,
  separation = FALSE,
  strict_readiness = TRUE
)
design
#> <gp3bayes_design_support_audit>
#>   Status: review
#>               component       status
#>      standard_readiness         pass
#>        strict_readiness         pass
#>             missingness         pass
#>     fixed_effect_design       review
#>  random_effects_support         pass
#>              separation not_assessed
#>   Automatic model changes: FALSE
```

## 4. Prepare and specify

``` r

prepared <- prepare_hierarchical_binary_data(
  simulation$data,
  contract,
  condition_levels = c("control", "treatment"),
  scale_predictors = "trial_covariate"
)

specification <- specify_binary_model(
  prepared,
  baseline = 0.35
)

prior_check <- check_binary_prior_predictive(
  specification,
  draws = 200,
  seed = 202603
)
prior_check
#> <gp3bayes_binary_prior_predictive_check>
#>   Adequate: TRUE
#>   Draws: 200
#>   Failed checks: 0
#>   Backend: none
#>   Fit performed: FALSE
```

## 5. Freeze analysis provenance

``` r

sensitivity_plan <- create_sensitivity_suite_plan(
  prior_scale = TRUE,
  psis_loo = TRUE
)

manifest <- create_analysis_manifest(
  specification = specification,
  estimands = "standardized_probability_contrast",
  sensitivity_plan = sensitivity_plan,
  seed = 202604,
  label = "gp3bayes 0.2.0 synthetic release case"
)

frozen_manifest <- freeze_analysis_manifest(manifest)
frozen_manifest
#> <gp3bayes_analysis_manifest>
#>   Version: 0.2
#>   Label: gp3bayes 0.2.0 synthetic release case
#>   Family: binary
#>   Data: 288 x 8
#>   Data hash: f3612a97dabe7adffe8782487b233903
#>   Frozen: TRUE
#>   Manifest hash: e87e01f3ffb2e7c9f23cec8174448670
```

## 6. Optional dual-backend fitting

``` r

fit_rstan <- fit_binary_model_backend(
  specification,
  backend = "rstan",
  chains = 2,
  iter = 2000,
  warmup = 1000,
  cores = 2,
  seed = 202604
)

fit_cmdstanr <- fit_binary_model_backend(
  specification,
  backend = "cmdstanr",
  chains = 2,
  iter = 2000,
  warmup = 1000,
  cores = 2,
  seed = 202604
)
```

## 7. Unified posterior review

``` r

diagnostics <- diagnose_model_fit(fit_cmdstanr)
posterior <- summarise_model_posterior(fit_cmdstanr)
ppc <- check_model_ppc(fit_cmdstanr, draws = 500, seed = 202605)
estimands <- estimate_model_estimands(fit_cmdstanr)
loo_result <- compute_psis_loo(fit_cmdstanr)
suite <- run_sensitivity_suite(fit_cmdstanr, sensitivity_plan)
```

## 8. Cross-backend consistency

``` r

parity <- audit_backend_parity(fit_rstan, fit_cmdstanr)
parity
plot(parity)
```

## 9. Evidence and compatibility

``` r

evidence <- collect_model_evidence(
  fit = fit_cmdstanr,
  design = design,
  diagnostics = diagnostics,
  posterior = posterior,
  ppc = ppc,
  estimands = estimands,
  loo = loo_result,
  sensitivity = suite,
  manifest = frozen_manifest
)

fit_schema <- freeze_gp3bayes_schema(
  capture_gp3bayes_schema(fit_cmdstanr)
)

evidence
model_workflow_status(evidence)
```

The end product is an inspectable chain from design contract to evidence
inventory. At no stage does the package infer emotion, cognition,
diagnosis, causality, model adequacy, robustness, or a preferred model
automatically.
