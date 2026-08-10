fake_binary_fit <- function() {
  structure(
    list(
      fit_version = "test",
      family = "binary",
      model_family = "hierarchical_binary",
      specification = NULL,
      backend_fit = NULL,
      backend_interface = "brms",
      sampling_backend = "rstan",
      algorithm = "sampling",
      sampling = list(chains = 2L, cores = 1L, seed = 1L),
      fit_performed = TRUE
    ),
    class = c("gp3bayes_binary_fit", "gp3bayes_fit")
  )
}

test_that("sensitivity plans are declarative and inert", {
  plan <- create_sensitivity_suite_plan()

  expect_s3_class(plan, "gp3bayes_sensitivity_plan")
  expect_false(plan$prior_scale$run)
  expect_false(plan$powerscale$run)
  expect_false(plan$psis_loo$run)
  expect_false(plan$automatic_model_selection)
  expect_false(plan$automatic_exclusion)
})

test_that("empty sensitivity suite performs no refitting", {
  suite <- run_sensitivity_suite(fake_binary_fit())

  expect_s3_class(suite, "gp3bayes_sensitivity_suite")
  expect_identical(suite$status, "not_run")
  expect_length(suite$results, 0L)
  expect_false(suite$robustness_established)
  expect_false(suite$automatic_model_selection)
})

test_that("evidence collection remains an inventory rather than a verdict", {
  design <- structure(
    list(status = "pass"),
    class = "gp3bayes_design_support_audit"
  )
  evidence <- collect_model_evidence(
    fit = fake_binary_fit(),
    design = design
  )

  expect_s3_class(evidence, "gp3bayes_model_evidence")
  expect_true(evidence$component_table$available[evidence$component_table$component == "design"])
  expect_false(evidence$adequacy_established)
  expect_false(evidence$robustness_established)
  expect_false(evidence$causal_identification_established)
})

test_that("model evidence reports require explicit files", {
  evidence <- collect_model_evidence(fit = fake_binary_fit())
  path <- tempfile(fileext = ".md")
  on.exit(unlink(path), add = TRUE)

  result <- create_model_evidence_report(evidence, path)

  expect_true(file.exists(path))
  expect_identical(result, normalizePath(path, winslash = "/", mustWork = TRUE))
  text <- readLines(path, warn = FALSE)
  expect_true(any(grepl("evidence inventory", text, ignore.case = TRUE)))
})

test_that("empty sensitivity suite covers duration fits without backend work", {
  fit <- structure(
    list(
      fit_version = "test",
      family = "duration",
      model_family = "hierarchical_duration",
      specification = NULL,
      backend_fit = NULL,
      backend_interface = "brms",
      sampling_backend = "rstan",
      algorithm = "sampling",
      sampling = list(chains = 2L, cores = 1L, seed = 1L),
      fit_performed = TRUE
    ),
    class = c("gp3bayes_duration_fit", "gp3bayes_fit")
  )

  suite <- run_sensitivity_suite(fit)

  expect_identical(suite$family, "duration")
  expect_identical(suite$status, "not_run")
})
