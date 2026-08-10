# Unified Sensitivity Suites and Evidence Inventories

## Orchestration without automatic robustness claims

`gp3bayes` already provides prior sensitivity, power scaling, PSIS-LOO,
structural sensitivity, group-deletion sensitivity, coding/scaling
variants, duration-unit invariance and exact K-fold validation. Version
0.2.0 adds a thin orchestration layer so these results can be planned
and collected without turning them into an automatic “robust/not robust”
verdict.

## Declare a suite before running it

``` r

plan <- create_sensitivity_suite_plan(
  prior_scale = TRUE,
  powerscale = TRUE,
  psis_loo = TRUE
)
plan
#> <gp3bayes_sensitivity_plan>
#>   Prior-scale refit: TRUE
#>   Power-scaling: TRUE
#>   PSIS-LOO: TRUE
#>   Random-slope plan: FALSE
#>   Group-deletion plan: FALSE
```

Creating the plan runs **nothing**. Expensive components only run when
[`run_sensitivity_suite()`](https://stefanosbalaskas.github.io/gp3bayes/reference/run_sensitivity_suite.md)
receives both a fitted model and an explicit plan.

``` r

suite <- run_sensitivity_suite(
  fit,
  plan,
  stop_on_error = FALSE
)

summarise_sensitivity_suite(suite)
plot(suite)
```

Structural sensitivity can be declared using the package’s existing
governed plans:

``` r

random_slope_plan <- create_random_slope_sensitivity_plan(specification)
group_plan <- create_group_deletion_sensitivity_plan(
  specification,
  group = "participant",
  units = c("p001", "p002")
)

plan <- create_sensitivity_suite_plan(
  prior_scale = TRUE,
  psis_loo = TRUE,
  random_slope_plan = random_slope_plan,
  group_deletion_plan = group_plan
)
```

## Evidence is an inventory

Already-computed results can be collected into one review object.

``` r

evidence <- collect_model_evidence(
  fit = fit,
  design = design,
  diagnostics = diagnostics,
  posterior = posterior,
  ppc = ppc,
  estimands = estimands,
  loo = loo_result,
  sensitivity = suite,
  manifest = frozen_manifest
)

evidence
plot(evidence)
```

Reports require an explicit file path:

``` r

report <- tempfile(fileext = ".md")
create_model_evidence_report(evidence, report)
unlink(report)
```

The inventory deliberately withholds aggregate adequacy, robustness,
causal, and model-selection claims. Different evidence components answer
different questions and can disagree without being collapsed into a
single score.
