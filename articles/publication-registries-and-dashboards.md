# Publication Registries and Diagnostic Dashboards

Registries bind named tables and figures to captions and provenance
labels. They write nothing unless an explicit output path is supplied.

``` r

registry <- create_publication_registry("Primary analysis")
registry <- register_publication_table(
  registry,
  "posterior",
  posterior_interval_table(fit),
  caption = "Posterior summaries",
  source = "primary fit"
)
registry <- register_publication_figure(
  registry,
  "posterior_intervals",
  plot_posterior_intervals(fit)
)

validate_publication_registry(registry)
publication_registry_table(registry)
```

Dashboards are non-interactive evidence indices and do not launch
expensive analyses implicitly.

``` r

dashboard <- create_diagnostic_dashboard(
  fit = fit,
  model_card = card,
  loo = loo_atlas,
  prior_posterior = bridge,
  sensitivity = sensitivity,
  recovery = recovery,
  sbc = sbc_result
)

diagnostic_dashboard_table(dashboard)
plot_diagnostic_dashboard(dashboard)
create_diagnostic_dashboard_figures(dashboard)
```
