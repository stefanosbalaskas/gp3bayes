# Pointwise LOO Influence Atlases

Aggregate PSIS-LOO summaries can be read together with observation-level
predictive contributions and influence diagnostics.

``` r

atlas <- create_loo_influence_atlas(
  loo_result,
  data = fit$specification$prepared$data
)

loo_influence_summary(atlas$table)
loo_flagged_data(atlas$table)

plot_loo_pointwise_elpd(atlas$table)
plot_loo_pareto_vs_elpd(atlas$table)
plot_loo_influence_rank(atlas$table)
```

Flagged observations request inspection and are never removed
automatically.
