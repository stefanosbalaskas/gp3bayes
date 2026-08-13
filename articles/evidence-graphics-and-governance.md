# Evidence Graphics and Governance

The stable gp3bayes objects already preserve provenance, design support,
sensitivity results, backend checks, and schema comparisons. The
development graphics layer adds ggplot-based views while leaving the
underlying objects unchanged.

``` r

sensitivity_suite_table(sensitivity)
plot_sensitivity_suite_gg(sensitivity)

model_evidence_table(evidence)
plot_model_evidence_gg(evidence)

backend_parity_table(parity)
plot_backend_parity_gg(parity)

manifest_comparison_table(manifest_diff)
plot_manifest_comparison_gg(manifest_diff)

schema_comparison_table(schema_diff)
plot_schema_comparison_gg(schema_diff)

design_support_table(design_audit)
plot_design_support_gg(design_audit)

missingness_audit_table(missingness)
plot_missingness_gg(missingness)
```

A status such as `pass`, `review`, or `fail` retains the semantics
defined by the originating gp3bayes audit. The plotting adapter does not
reinterpret it.
