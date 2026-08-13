cat("\n===== gp3bayes advanced post-fit audit =====\n")

expected_exports <- c(
  "prediction_draws_long",
  "posterior_predictive_statistic",
  "ppc_statistic_table",
  "binary_confusion_table",
  "binary_roc_curve",
  "binary_precision_recall_curve",
  "binary_calibration_error",
  "binary_group_calibration",
  "duration_qq_table",
  "duration_tail_check",
  "group_prediction_summary",
  "prediction_pairwise_contrasts",
  "prediction_interval_width",
  "prediction_rank_probabilities",
  "sensitivity_suite_table",
  "model_evidence_table",
  "backend_parity_table",
  "backend_environment_table",
  "manifest_comparison_table",
  "schema_comparison_table",
  "design_support_table",
  "missingness_audit_table",
  "plot_sensitivity_suite_gg",
  "plot_model_evidence_gg",
  "plot_backend_parity_gg",
  "plot_backend_environment_gg",
  "plot_manifest_comparison_gg",
  "plot_schema_comparison_gg",
  "plot_design_support_gg",
  "plot_missingness_gg",
  "plot_prediction_draws",
  "plot_ppc_statistic",
  "plot_binary_roc",
  "plot_binary_precision_recall",
  "plot_binary_group_calibration",
  "plot_duration_qq",
  "plot_duration_tail",
  "plot_group_predictions",
  "plot_prediction_interval_width",
  "plot_prediction_rank_probabilities",
  "create_model_card",
  "model_card_table",
  "create_reporting_checklist",
  "plot_reporting_checklist",
  "write_model_card"
)

namespace <- readLines("NAMESPACE", warn = FALSE)
exports <- sub(
  "^export\\((.*)\\)$",
  "\\1",
  grep("^export\\(", namespace, value = TRUE)
)

missing <- setdiff(expected_exports, exports)
if (length(missing)) {
  stop("Missing Phase 2 exports: ", paste(missing, collapse = ", "), call. = FALSE)
}

stopifnot(
  identical(unname(read.dcf("DESCRIPTION")[1L, "Version"]), "0.3.0.9000"),
  file.exists("R/predictive-diagnostics-advanced.R"),
  file.exists("R/evidence-graphics-gg.R"),
  file.exists("R/advanced-predictive-graphics.R"),
  file.exists("R/model-card.R")
)

source_text <- unlist(lapply(
  c(
    "R/predictive-diagnostics-advanced.R",
    "R/evidence-graphics-gg.R",
    "R/advanced-predictive-graphics.R",
    "R/model-card.R"
  ),
  readLines,
  warn = FALSE
))

stopifnot(
  !any(grepl("\\.GlobalEnv", source_text, fixed = TRUE)),
  !any(grepl("setwd\\(", source_text)),
  !any(grepl("automatic_adequacy_verdict = TRUE", source_text, fixed = TRUE)),
  !any(grepl("automatic_selection = TRUE", source_text, fixed = TRUE))
)

cat("Phase 2 new exports:", length(expected_exports), "\n")
cat("Total exports:", length(exports), "\n")
cat("Advanced articles: 4\n")
cat("Scope/compliance audit: PASS\n")
