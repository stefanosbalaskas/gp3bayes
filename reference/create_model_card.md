# Create a gp3bayes Model Card

Creates a compact, structured record of model identity, computational
diagnostics, prediction evidence, provenance, and interpretation
boundaries. The card is documentation; it does not issue a
model-adequacy certificate.

## Usage

``` r
create_model_card(fit, analysis_bundle = NULL, manifest = NULL, label = NULL)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- analysis_bundle:

  Optional `gp3bayes_analysis_bundle`.

- manifest:

  Optional `gp3bayes_analysis_manifest`.

- label:

  Optional human-readable label.

## Value

A `gp3bayes_model_card`.
