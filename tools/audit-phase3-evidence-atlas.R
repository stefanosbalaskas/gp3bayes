cat("\n===== gp3bayes Phase 3 evidence-atlas audit =====\n")

expected_exports <- c(
  "recovery_parameter_table",
  "recovery_estimate_table",
  "recovery_fit_status_table",
  "prior_sensitivity_table",
  "prior_sensitivity_scenario_table",
  "estimand_sensitivity_table",
  "group_deletion_sensitivity_table",
  "random_slope_sensitivity_table",
  "powerscale_sensitivity_table",
  "sbc_stats_table",
  "sbc_overview_table",
  "plot_recovery_bias",
  "plot_recovery_coverage",
  "plot_recovery_rmse",
  "plot_recovery_estimates",
  "plot_recovery_fit_status",
  "plot_prior_sensitivity",
  "plot_prior_sensitivity_scenarios",
  "plot_estimand_sensitivity_gg",
  "plot_group_deletion_sensitivity",
  "plot_random_slope_sensitivity",
  "plot_powerscale_sensitivity_gg",
  "plot_sbc_rank_gg",
  "plot_sbc_ecdf_gg",
  "plot_sbc_coverage_gg",
  "plot_sbc_simulated_vs_estimated_gg",
  "prior_specification_table",
  "simulate_declared_prior_draws",
  "prior_posterior_bridge",
  "prior_posterior_summary_table",
  "prior_posterior_distance_table",
  "prior_posterior_draws_long",
  "plot_prior_posterior_density",
  "plot_prior_posterior_intervals",
  "plot_prior_posterior_shift",
  "plot_prior_posterior_contraction",
  "loo_pointwise_table",
  "loo_influence_summary",
  "loo_flagged_data",
  "plot_loo_pointwise_elpd",
  "plot_loo_pareto_vs_elpd",
  "plot_loo_influence_rank",
  "create_loo_influence_atlas",
  "loo_influence_atlas_table",
  "create_publication_registry",
  "register_publication_table",
  "register_publication_figure",
  "publication_registry_table",
  "validate_publication_registry",
  "write_publication_registry",
  "save_publication_registry_figures",
  "create_diagnostic_dashboard",
  "diagnostic_dashboard_table",
  "create_diagnostic_dashboard_figures",
  "write_diagnostic_dashboard_report",
  "plot_diagnostic_dashboard",
  "create_complete_evidence_inventory",
  "evidence_inventory_table"
)

namespace <- readLines("NAMESPACE", warn = FALSE, encoding = "UTF-8")
exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", namespace, value = TRUE))
missing <- setdiff(expected_exports, exports)

if (length(missing)) {
  stop("Missing Phase 3 exports: ", paste(missing, collapse = ", "), call. = FALSE)
}

stopifnot(
  length(expected_exports) == 58L,
  length(exports) == 291L,
  identical(unname(read.dcf("DESCRIPTION")[1L, "Version"]), "0.3.0.9000")
)

new_files <- c(
  "R/recovery-sensitivity-publication.R",
  "R/prior-posterior-bridge.R",
  "R/loo-influence-atlas.R",
  "R/publication-registry-dashboard.R"
)
stopifnot(all(file.exists(new_files)))

source_text <- unlist(lapply(new_files, readLines, warn = FALSE))
stopifnot(
  !any(grepl("\\.GlobalEnv", source_text, fixed = TRUE)),
  !any(grepl("setwd\\(", source_text)),
  !any(grepl("automatic_exclusion = TRUE", source_text, fixed = TRUE)),
  !any(grepl("automatic_selection = TRUE", source_text, fixed = TRUE)),
  !any(grepl("automatic_decision = TRUE", source_text, fixed = TRUE)),
  !any(grepl("calibration_established = TRUE", source_text, fixed = TRUE))
)

cat("Phase 3 exports:", length(expected_exports), "\n")
cat("Total exports:", length(exports), "\n")
cat("Phase 3 articles: 7\n")
cat("Scope/compliance audit: PASS\n")
