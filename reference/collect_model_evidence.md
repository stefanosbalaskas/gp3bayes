# Collect Model Evidence Without Declaring Model Adequacy

Collects already-computed design, diagnostics, posterior summaries,
posterior predictive checks, estimands, predictive validation,
sensitivity, and reproducibility provenance into a single review object.

## Usage

``` r
collect_model_evidence(
  fit = NULL,
  design = NULL,
  diagnostics = NULL,
  posterior = NULL,
  ppc = NULL,
  estimands = NULL,
  loo = NULL,
  kfold = NULL,
  sensitivity = NULL,
  manifest = NULL,
  compute = character()
)
```

## Arguments

- fit:

  Optional gp3bayes fit.

- design, diagnostics, posterior, ppc, estimands, loo, kfold,
  sensitivity:

  Optional evidence components.

- manifest:

  Optional `gp3bayes_analysis_manifest` containing reproducibility
  provenance for the analysis.

- compute:

  Character vector selecting inexpensive components to compute from
  `fit` when not supplied. Supported values are `"diagnostics"`,
  `"posterior"`, and `"estimands"`. Posterior predictive checks and
  refitting sensitivity are intentionally not automatic.

## Value

A `gp3bayes_model_evidence`.
