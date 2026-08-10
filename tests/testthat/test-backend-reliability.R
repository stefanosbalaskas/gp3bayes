test_that("backend capability wrapper retains the approved two-backend scope", {
  capabilities <- backend_capabilities()

  expect_s3_class(capabilities, "gp3bayes_backend_capabilities_v2")
  expect_setequal(capabilities$backend, c("rstan", "cmdstanr"))
  expect_true(all(capabilities$algorithm == "sampling"))
  expect_false(any(capabilities$unrestricted_modeling))
})

test_that("backend environment validation is non-compiling by default", {
  environment <- validate_backend_environment("rstan", compile_test = FALSE)

  expect_s3_class(environment, "gp3bayes_backend_environment")
  expect_false(environment$compile_test)
  expect_true("compiler_smoke_test" %in% environment$checks$check)
})

test_that("backend parity uses MCSE bands rather than identical draws", {
  rstan_summary <- data.frame(
    variable = c("b_Intercept", "b_conditiontreatment"),
    mean = c(-0.60, 0.40),
    sd = c(0.20, 0.15),
    mcse_mean = c(0.01, 0.01),
    stringsAsFactors = FALSE
  )
  cmdstanr_summary <- data.frame(
    variable = c("b_Intercept", "b_conditiontreatment"),
    mean = c(-0.59, 0.41),
    sd = c(0.21, 0.15),
    mcse_mean = c(0.01, 0.01),
    stringsAsFactors = FALSE
  )

  parity <- audit_backend_parity(rstan_summary, cmdstanr_summary)

  expect_s3_class(parity, "gp3bayes_backend_parity_audit")
  expect_identical(parity$status, "pass")
  expect_false(parity$identical_draws_expected)
  expect_false(parity$model_adequacy_established)
})

test_that("backend parity marks materially different summaries for review", {
  a <- data.frame(variable = "b_x", mean = 0, sd = 0.2, mcse_mean = 0.01)
  b <- data.frame(variable = "b_x", mean = 0.5, sd = 0.4, mcse_mean = 0.01)

  parity <- audit_backend_parity(a, b)

  expect_identical(parity$status, "review")
  expect_identical(parity$table$status, "review")
})

test_that("schema capture and comparison detect structural drift", {
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    condition_col = "condition"
  )
  schema <- capture_gp3bayes_schema(contract)

  expect_s3_class(schema, "gp3bayes_object_schema")
  expect_false(schema$values_recorded)

  same <- compare_gp3bayes_schemas(schema, capture_gp3bayes_schema(contract))
  expect_identical(same$status, "pass")

  changed <- contract
  changed$new_field <- TRUE
  comparison <- compare_gp3bayes_schemas(schema, capture_gp3bayes_schema(changed))
  expect_identical(comparison$status, "review")
})

test_that("schema files are written only to explicit paths", {
  contract <- create_model_contract(
    "binary", "selected", "participant_id", condition_col = "condition"
  )
  schema <- freeze_gp3bayes_schema(contract)
  expect_true(schema$frozen)

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  freeze_gp3bayes_schema(schema, file = path)
  expect_true(file.exists(path))
  restored <- read_gp3bayes_schema(path)
  expect_s3_class(restored, "gp3bayes_object_schema")
})

test_that("backend parity treats unavailable MCSE as review rather than pass", {
  a <- data.frame(variable = "b_x", mean = 0, sd = 0.2, mcse_mean = NA_real_)
  b <- data.frame(variable = "b_x", mean = 0, sd = 0.2, mcse_mean = 0.01)

  parity <- audit_backend_parity(a, b)

  expect_identical(parity$status, "review")
  expect_identical(parity$table$status, "review")
})

test_that("schema compatibility ignores analysis-specific lengths by default", {
  a <- create_model_contract(
    "binary", "selected", "participant_id",
    condition_col = "condition", predictors = "x"
  )
  b <- create_model_contract(
    "binary", "selected", "participant_id",
    condition_col = "condition", predictors = c("x", "z")
  )

  relaxed <- compare_gp3bayes_schemas(a, b)
  strict_lengths <- compare_gp3bayes_schemas(a, b, compare_lengths = TRUE)

  expect_identical(relaxed$status, "pass")
  expect_identical(strict_lengths$status, "review")
})


test_that("backend parity reports parameters missing from either backend", {
  a <- data.frame(
    variable = c("b_Intercept", "b_x"),
    mean = c(0, 0.2), sd = c(0.2, 0.1), mcse_mean = c(0.01, 0.01)
  )
  b <- data.frame(
    variable = c("b_Intercept", "b_y"),
    mean = c(0, -0.2), sd = c(0.2, 0.1), mcse_mean = c(0.01, 0.01)
  )

  parity <- audit_backend_parity(a, b)

  expect_identical(parity$status, "review")
  expect_identical(parity$missing_from_rstan, "b_y")
  expect_identical(parity$missing_from_cmdstanr, "b_x")
})

test_that("backend audit objects expose tabular data", {
  a <- data.frame(variable = "b_x", mean = 0, sd = 0.2, mcse_mean = 0.01)
  b <- data.frame(variable = "b_x", mean = 0.01, sd = 0.2, mcse_mean = 0.01)
  parity <- audit_backend_parity(a, b)

  expect_s3_class(as.data.frame(parity), "data.frame")
  expect_true("variable" %in% names(as.data.frame(parity)))
})
