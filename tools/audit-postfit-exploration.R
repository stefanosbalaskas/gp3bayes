cat("\n===== gp3bayes post-fit exploration audit =====\n")

expected_files <- c(
  "R/postfit-exploration.R",
  "R/prediction-support.R",
  "R/publication-graphics.R",
  "R/analysis-bundle.R",
  "vignettes/posterior-exploration-and-graphics.Rmd",
  "vignettes/prediction-calibration-and-scoring.Rmd",
  "vignettes/hierarchical-effects-and-uncertainty.Rmd",
  "vignettes/loo-influence-and-model-comparison.Rmd",
  "vignettes/publication-analysis-bundles.Rmd"
)
stopifnot(all(file.exists(expected_files)))

expected_exports <- c(
  "extract_posterior_draws",
  "extract_sampler_diagnostics",
  "extract_log_likelihood",
  "posterior_interval_table",
  "posterior_probability_table",
  "posterior_correlation_table",
  "mcmc_diagnostic_table",
  "sampler_diagnostic_table",
  "identify_mcmc_issues",
  "summarise_mcmc_quality",
  "group_effect_table",
  "variance_component_table",
  "loo_diagnostic_table",
  "loo_summary_table",
  "model_comparison_table",
  "model_weights_table",
  "create_prediction_grid",
  "audit_prediction_support",
  "prediction_support_table",
  "predict_model",
  "predict_binary_probability",
  "predict_duration",
  "extract_expected_predictions",
  "extract_posterior_predictions",
  "extract_linear_predictions",
  "prediction_table",
  "prediction_contrast",
  "prediction_exceedance_probability",
  "prediction_uncertainty_decomposition",
  "grouped_prediction_check",
  "predictive_residuals",
  "binary_prediction_scores",
  "binary_threshold_metrics",
  "binary_calibration_table",
  "duration_prediction_scores",
  "duration_quantile_calibration",
  "duration_pit_table",
  "predictive_coverage_table",
  "posterior_predictive_summary_table",
  "theme_gp3bayes",
  "plot_posterior_intervals",
  "plot_posterior_areas",
  "plot_posterior_density",
  "plot_posterior_correlations",
  "plot_posterior_pairs",
  "plot_rank_diagnostics",
  "plot_autocorrelation",
  "plot_mcmc_quality",
  "plot_sampler_diagnostics",
  "plot_estimand_intervals",
  "plot_prediction_intervals",
  "plot_binary_calibration",
  "plot_binary_threshold_metrics",
  "plot_duration_quantile_calibration",
  "plot_duration_pit",
  "plot_exceedance_probability",
  "plot_predictive_coverage",
  "plot_predictive_residuals",
  "plot_prediction_support",
  "plot_uncertainty_decomposition",
  "plot_grouped_prediction_check",
  "plot_group_effects",
  "plot_variance_components",
  "plot_loo_influence",
  "plot_model_comparison",
  "plot_model_weights",
  "create_figure_set",
  "save_figure_set",
  "create_analysis_bundle",
  "analysis_bundle_table",
  "create_publication_table_set",
  "create_analysis_figure_set",
  "write_analysis_bundle_report"
)

namespace <- readLines("NAMESPACE", warn = FALSE)
observed_exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", namespace, value = TRUE))
missing <- setdiff(expected_exports, observed_exports)
if (length(missing)) {
  stop("Missing expected exports: ", paste(missing, collapse = ", "), call. = FALSE)
}

desc <- read.dcf("DESCRIPTION")
stopifnot(
  identical(unname(desc[1L, "Version"]), "0.3.0.9000"),
  grepl("\\bggplot2\\b", unname(desc[1L, "Suggests"]), perl = TRUE)
)

all_new <- c(
  readLines("R/postfit-exploration.R", warn = FALSE),
  readLines("R/prediction-support.R", warn = FALSE),
  readLines("R/publication-graphics.R", warn = FALSE),
  readLines("R/analysis-bundle.R", warn = FALSE)
)
stopifnot(
  !any(grepl("setwd\\(", all_new)),
  !any(grepl("\\.GlobalEnv", all_new, fixed = TRUE)),
  !any(grepl("parallel::detectCores", all_new, fixed = TRUE)),
  !any(grepl("automatic_selection = TRUE", all_new, fixed = TRUE)),
  !any(grepl("automatic_decision = TRUE", all_new, fixed = TRUE))
)

cat("Expected new exports:", length(expected_exports), "\n")
cat("Total exports:", length(observed_exports), "\n")
cat("New articles: 5\n")
cat("Scope/compliance audit: PASS\n")
