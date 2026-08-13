# Quality Hardening and Failure Contracts

The development API is deliberately frozen during this hardening phase.
The purpose is to deepen integration and maintenance guarantees rather
than add new analytical surface area.

## Public API contract

A machine-readable manifest records all exported function names and
formal argument names. Tests compare the installed namespace against
this manifest so accidental public additions, removals, or signature
changes become explicit failures rather than silent drift.

The relevant governance interfaces include
[`validate_gp3bayes_object()`](https://stefanosbalaskas.github.io/gp3bayes/reference/validate_gp3bayes_object.md),
[`capture_gp3bayes_schema()`](https://stefanosbalaskas.github.io/gp3bayes/reference/capture_gp3bayes_schema.md),
[`validate_gp3bayes_schema()`](https://stefanosbalaskas.github.io/gp3bayes/reference/validate_gp3bayes_schema.md),
[`create_analysis_manifest()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_analysis_manifest.md),
and
[`compare_analysis_manifests()`](https://stefanosbalaskas.github.io/gp3bayes/reference/compare_analysis_manifests.md).

## Lightweight post-fit adapters

Small adapters are useful because they let downstream reports use stable
data frames rather than inspect internal object fields. Examples include
[`backend_environment_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/backend_environment_table.md),
[`loo_influence_atlas_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/loo_influence_atlas_table.md),
[`prediction_profile_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/prediction_profile_table.md),
[`prediction_surface_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/prediction_surface_table.md),
[`prediction_draws_long()`](https://stefanosbalaskas.github.io/gp3bayes/reference/prediction_draws_long.md),
and
[`prior_posterior_draws_long()`](https://stefanosbalaskas.github.io/gp3bayes/reference/prior_posterior_draws_long.md).

## Explicit failure boundaries

Fit-dependent extraction helpers such as
[`extract_expected_predictions()`](https://stefanosbalaskas.github.io/gp3bayes/reference/extract_expected_predictions.md),
[`extract_posterior_predictions()`](https://stefanosbalaskas.github.io/gp3bayes/reference/extract_posterior_predictions.md),
[`extract_linear_predictions()`](https://stefanosbalaskas.github.io/gp3bayes/reference/extract_linear_predictions.md),
[`extract_log_likelihood()`](https://stefanosbalaskas.github.io/gp3bayes/reference/extract_log_likelihood.md),
and
[`extract_sampler_diagnostics()`](https://stefanosbalaskas.github.io/gp3bayes/reference/extract_sampler_diagnostics.md)
reject malformed inputs rather than guessing.

Prediction-comparison helpers retain explicit bounds. In particular,
[`prediction_pairwise_contrasts()`](https://stefanosbalaskas.github.io/gp3bayes/reference/prediction_pairwise_contrasts.md)
and
[`prediction_rank_probabilities()`](https://stefanosbalaskas.github.io/gp3bayes/reference/prediction_rank_probabilities.md)
require the analyst to opt into larger comparison sets rather than
expanding combinatorially without review.

## Prediction diagnostics

The diagnostic layer separates descriptive posterior evidence from
automatic decisions.
[`binary_group_calibration()`](https://stefanosbalaskas.github.io/gp3bayes/reference/binary_group_calibration.md),
[`posterior_predictive_summary_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/posterior_predictive_summary_table.md),
[`predictive_coverage_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/predictive_coverage_table.md),
[`duration_pit_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/duration_pit_table.md),
and
[`loo_group_influence_table()`](https://stefanosbalaskas.github.io/gp3bayes/reference/loo_group_influence_table.md)
return evidence for review; none automatically certifies adequacy or
excludes observations/groups.

## Output safety

Writers such as
[`write_model_card()`](https://stefanosbalaskas.github.io/gp3bayes/reference/write_model_card.md),
[`write_publication_registry()`](https://stefanosbalaskas.github.io/gp3bayes/reference/write_publication_registry.md),
[`write_diagnostic_dashboard_report()`](https://stefanosbalaskas.github.io/gp3bayes/reference/write_diagnostic_dashboard_report.md),
[`write_analysis_bundle_report()`](https://stefanosbalaskas.github.io/gp3bayes/reference/write_analysis_bundle_report.md),
and
[`save_publication_registry_figures()`](https://stefanosbalaskas.github.io/gp3bayes/reference/save_publication_registry_figures.md)
remain explicit-output operations. The package does not use the current
working directory as an implicit reporting destination.

## Why examples are selective

Not every exported wrapper has a runnable Rd example. Many functions
require a fitted Bayesian backend object, and duplicating expensive fits
across hundreds of help topics would make checks slower without
improving the underlying API. The package therefore combines short
deterministic Rd examples for lightweight functions with articles, unit
tests, integration tests, and complete reference documentation for
fit-dependent workflows.
