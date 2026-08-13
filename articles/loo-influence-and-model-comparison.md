# LOO Influence and Predictive Model Comparison

The LOO display layer converts existing `gp3bayes_psis_loo`,
`gp3bayes_loo_comparison`, and `gp3bayes_loo_weights` objects into
explicit tables and publication-oriented figures.

``` r

loo_result <- compute_psis_loo(fit, cores = 1)

loo_summary_table(loo_result)
loo_diagnostic_table(loo_result)
plot_loo_influence(loo_result)
```

For multiple prespecified models:

``` r

comparison <- compare_psis_loo(
  list(reference = fit_reference, sensitivity = fit_sensitivity),
  cores = 1
)

weights <- compute_loo_model_weights(comparison, method = "stacking")

model_comparison_table(comparison)
model_weights_table(weights)

plot_model_comparison(comparison)
plot_model_weights(weights)
```

ELPD differences and predictive weights are retained as descriptive
predictive evidence. The package does not promote the highest-ranked
model to an automatically preferred substantive model.
