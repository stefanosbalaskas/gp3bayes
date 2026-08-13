# Publication-Ready Analysis Bundles

The analysis-bundle layer assembles post-fit evidence while preserving
the distinction between computation, presentation, and interpretation.

``` r

bundle <- create_analysis_bundle(
  fit,
  ndraws = 1000,
  include_group_effects = FALSE,
  include_loo = TRUE
)

analysis_bundle_table(bundle)

tables <- create_publication_table_set(bundle)
figures <- create_analysis_figure_set(bundle)
```

No component is silently dropped: failures are retained in the bundle
status table with their error message.

## Explicit output

Reports and figures require explicit paths.

``` r

write_analysis_bundle_report(
  bundle,
  file = file.path(tempdir(), "gp3bayes-analysis.md")
)

save_figure_set(
  figures,
  directory = file.path(tempdir(), "gp3bayes-figures"),
  device = "png"
)
```

This design prevents analysis functions from writing automatically to
the working directory and keeps publication formatting downstream of the
model contract and validation objects.
