# Model Cards and Reporting Inventories

A model card records what was fitted and what evidence is available. It
does not turn documentation completeness into model validity.

``` r

bundle <- create_analysis_bundle(
  fit,
  ndraws = 1000,
  include_loo = TRUE
)

manifest <- create_analysis_manifest(
  fit = fit,
  estimands = "primary"
)

card <- create_model_card(
  fit,
  analysis_bundle = bundle,
  manifest = manifest,
  label = "Primary confirmatory model"
)

model_card_table(card)
checklist <- create_reporting_checklist(card)
plot_reporting_checklist(checklist)
```

Writing is explicit:

``` r

write_model_card(
  card,
  file = file.path(tempdir(), "gp3bayes-model-card.md")
)
```

The card records that automatic model selection, automatic adequacy
certification, and automatic causal identification remain false.
