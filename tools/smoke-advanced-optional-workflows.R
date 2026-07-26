#!/usr/bin/env Rscript

options(warn = 2)
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
pkgload::load_all(root, export_all = FALSE, helpers = FALSE, quiet = TRUE)

capabilities <- bayesian_backend_capabilities()
stopifnot(inherits(capabilities, "gp3bayes_backend_capabilities"))

binary_failure <- simulate_binary_pathology(
  "rank_deficiency",
  seed = 2026
)
binary_evaluation <- evaluate_pathological_simulation(binary_failure)
stopifnot(binary_evaluation$actual_structural_status == "fail")

duration_failure <- simulate_duration_pathology(
  "zero_duration",
  seed = 2026
)
duration_evaluation <- evaluate_pathological_simulation(duration_failure)
stopifnot(duration_evaluation$actual_structural_status == "fail")

if (requireNamespace("detectseparation", quietly = TRUE)) {
  dat <- data.frame(
    y = c(0, 0, 0, 1, 1, 1),
    x = c(-3, -2, -1, 1, 2, 3)
  )
  separation <- detect_binary_separation(dat, y ~ x)
  stopifnot(separation$separation_detected)
}

if (requireNamespace("loo", quietly = TRUE)) {
  draw_axis <- seq(
    from = -1.02,
    to = -0.98,
    length.out = 2000L
  )

  log_lik <- cbind(
    observation_1 = draw_axis,
    observation_2 = draw_axis + 0.010,
    observation_3 = draw_axis - 0.010,
    observation_4 = draw_axis + 0.005
  )

  loo_result <- compute_psis_loo_from_log_lik(log_lik)

  stopifnot(
    inherits(loo_result, "gp3bayes_psis_loo"),
    length(loo_result$pareto_k) == 4L,
    all(is.finite(loo_result$pareto_k)),
    loo_result$status %in% c("pass", "review", "fail"),
    identical(loo_result$automatic_selection, FALSE)
  )
}

cat("ADVANCED OPTIONAL WORKFLOW SMOKE TEST PASSED\n")
