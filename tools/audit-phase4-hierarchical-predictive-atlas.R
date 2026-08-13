cat("\n===== gp3bayes Phase 4 hierarchical/predictive atlas audit =====\n")

expected_exports <- c(
  "group_effect_draws_table",
  "group_effect_rank_probability_table",
  "random_intercept_variance_partition",
  "random_intercept_variance_partition_table",
  "plot_group_effect_distribution",
  "plot_group_effect_rank_probability",
  "plot_random_intercept_variance_partition",
  "create_prediction_profile",
  "prediction_profile_table",
  "prediction_gradient_table",
  "plot_prediction_profile",
  "plot_prediction_gradient",
  "create_prediction_surface",
  "prediction_surface_table",
  "plot_prediction_surface",
  "plot_prediction_surface_uncertainty",
  "create_prediction_contrast_profile",
  "prediction_contrast_profile_table",
  "plot_prediction_contrast_profile",
  "create_predictive_distribution_atlas",
  "predictive_distribution_atlas_table",
  "predictive_quantile_envelope",
  "prediction_score_uncertainty",
  "prediction_score_uncertainty_table",
  "binary_calibration_uncertainty",
  "binary_calibration_uncertainty_table",
  "plot_predictive_atlas_statistics",
  "plot_predictive_quantile_envelope",
  "plot_prediction_score_uncertainty",
  "plot_binary_calibration_uncertainty",
  "loo_group_influence_table",
  "plot_loo_group_influence",
  "plot_loo_group_elpd"
)

namespace <- readLines("NAMESPACE", warn = FALSE, encoding = "UTF-8")
exports <- sub(
  "^export\\((.*)\\)$",
  "\\1",
  grep("^export\\(", namespace, value = TRUE)
)

missing <- setdiff(expected_exports, exports)

if (length(missing)) {
  stop("Missing Phase 4 exports: ", paste(missing, collapse = ", "), call. = FALSE)
}

stopifnot(
  length(expected_exports) == 33L,
  length(exports) == 324L,
  identical(unname(read.dcf("DESCRIPTION")[1L, "Version"]), "0.3.0.9000")
)

new_files <- c(
  "R/hierarchical-effects-advanced.R",
  "R/prediction-surfaces.R",
  "R/predictive-distribution-atlas.R",
  "R/loo-group-influence.R"
)

stopifnot(all(file.exists(new_files)))

source_text <- unlist(lapply(new_files, readLines, warn = FALSE))

stopifnot(
  !any(grepl("\\.GlobalEnv", source_text, fixed = TRUE)),
  !any(grepl("setwd\\(", source_text)),
  !any(grepl("automatic_group_exclusion = TRUE", source_text, fixed = TRUE)),
  !any(grepl("automatic_ranking_decision = TRUE", source_text, fixed = TRUE)),
  !any(grepl("automatic_model_ranking = TRUE", source_text, fixed = TRUE)),
  !any(grepl("automatic_calibration_decision = TRUE", source_text, fixed = TRUE)),
  !any(grepl("automatic_adequacy_decision = TRUE", source_text, fixed = TRUE))
)

cat("Phase 4 exports:", length(expected_exports), "\n")
cat("Total exports:", length(exports), "\n")
cat("Phase 4 articles: 4\n")
cat("Scope/compliance audit: PASS\n")
