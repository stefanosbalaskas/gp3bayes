# End-to-End Evidence and Publication Showcase

The expanded post-fit system separates fitting, diagnostics,
sensitivity, simulation validation, provenance, publication output, and
interpretation.

``` r

bundle <- create_analysis_bundle(fit, ndraws = 1000, include_loo = TRUE)
bridge <- prior_posterior_bridge(fit)

loo_atlas <- create_loo_influence_atlas(
  bundle$components$loo$value,
  data = fit$specification$prepared$data
)

manifest <- create_analysis_manifest(fit = fit, estimands = "primary")
card <- create_model_card(
  fit,
  analysis_bundle = bundle,
  manifest = manifest,
  label = "Primary model"
)

dashboard <- create_diagnostic_dashboard(
  fit = fit,
  analysis_bundle = bundle,
  model_card = card,
  loo = loo_atlas,
  prior_posterior = bridge,
  sensitivity = prior_sensitivity,
  recovery = recovery,
  sbc = sbc_result,
  label = "Primary-model evidence"
)

inventory <- create_complete_evidence_inventory(
  bundle = bundle,
  model_card = card,
  loo = loo_atlas,
  prior_posterior = bridge,
  sensitivity = prior_sensitivity,
  recovery = recovery,
  sbc = sbc_result
)
```

No dashboard, registry, sensitivity result, recovery result, SBC
graphic, or LOO diagnostic automatically establishes causal validity or
selects a preferred model.
