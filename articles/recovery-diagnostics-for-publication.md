# Parameter Recovery Diagnostics for Publication

Recovery objects retain parameter summaries, repetition-level estimates,
and fit statuses. The publication layer exposes each separately.

``` r

recovery_parameter_table(recovery)
recovery_estimate_table(recovery)
recovery_fit_status_table(recovery)

plot_recovery_bias(recovery)
plot_recovery_coverage(recovery)
plot_recovery_rmse(recovery)
plot_recovery_estimates(recovery)
plot_recovery_fit_status(recovery)
```

Recovery evidence remains a validation diagnostic; no figure certifies
an implementation or model automatically.
