manifest_fixture <- function(seed = 201) {
  simulation <- simulate_hierarchical_binary_data(
    n_participants = 8,
    trials_per_participant = 6,
    n_items = 4,
    random_slope_sd = 0,
    seed = seed
  )
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "item_id",
    trial_col = "trial_id",
    condition_col = "condition"
  )
  prepared <- prepare_hierarchical_binary_data(
    simulation$data,
    contract,
    condition_levels = c("control", "treatment")
  )
  specify_binary_model(prepared, baseline = 0.35)
}

test_that("analysis manifests fingerprint data and specification", {
  specification <- manifest_fixture()
  manifest <- create_analysis_manifest(
    specification = specification,
    estimands = "standardized_probability_contrast",
    seed = 2026,
    label = "synthetic binary analysis"
  )

  expect_s3_class(manifest, "gp3bayes_analysis_manifest")
  expect_identical(manifest$family, "binary")
  expect_true(manifest$data$available)
  expect_match(manifest$data$hash, "^[0-9a-f]{32}$")
  expect_identical(manifest$data$hash_method, "MD5-of-RDS-v3")
  expect_true(manifest$data$row_order_sensitive)
  expect_match(manifest$specification$hash, "^[0-9a-f]{32}$")
  expect_false(manifest$frozen)
  expect_identical(validate_analysis_manifest(manifest)$status, "pass")
})

test_that("frozen manifests round trip through explicit temporary files", {
  manifest <- create_analysis_manifest(
    specification = manifest_fixture(202),
    estimands = "standardized_probability_contrast",
    seed = 2026
  )
  frozen <- freeze_analysis_manifest(manifest)

  expect_true(frozen$frozen)
  expect_match(frozen$manifest_hash, "^[0-9a-f]{32}$")

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  freeze_analysis_manifest(manifest, file = path)
  expect_true(file.exists(path))
  restored <- read_analysis_manifest(path)
  expect_s3_class(restored, "gp3bayes_analysis_manifest")
  expect_true(restored$frozen)
})

test_that("manifest comparison exposes provenance changes", {
  specification <- manifest_fixture(203)
  a <- create_analysis_manifest(specification, seed = 1)
  b <- create_analysis_manifest(specification, seed = 2)
  comparison <- compare_analysis_manifests(a, b)

  expect_s3_class(comparison, "gp3bayes_manifest_comparison")
  expect_false(comparison$identical)
  expect_true("seed" %in% comparison$changed_components)
})

test_that("reproducibility report requires an explicit destination", {
  manifest <- freeze_analysis_manifest(
    create_analysis_manifest(manifest_fixture(204), seed = 204)
  )
  path <- tempfile(fileext = ".md")
  on.exit(unlink(path), add = TRUE)

  result <- write_reproducibility_report(manifest, path)

  expect_true(file.exists(path))
  expect_identical(result, normalizePath(path, winslash = "/", mustWork = TRUE))
  text <- readLines(path, warn = FALSE)
  expect_true(any(grepl("Data fingerprint", text, fixed = TRUE)))
  expect_true(any(grepl("Interpretation boundary", text, fixed = TRUE)))
})

test_that("analysis manifests cover duration workflows", {
  simulation <- simulate_hierarchical_duration_data(
    n_participants = 8,
    trials_per_participant = 6,
    n_items = 4,
    random_slope_sd = 0,
    seed = 205
  )
  contract <- create_model_contract(
    "duration", "duration", "participant_id",
    item_col = "item_id", trial_col = "trial_id",
    condition_col = "condition", outcome_unit = "milliseconds"
  )
  prepared <- prepare_hierarchical_duration_data(
    simulation$data, contract,
    condition_levels = c("control", "treatment")
  )
  specification <- specify_duration_model(prepared, baseline = 500)
  manifest <- create_analysis_manifest(
    specification = specification,
    estimands = "standardized_duration_estimands",
    seed = 205
  )

  expect_identical(manifest$family, "duration")
  expect_identical(validate_analysis_manifest(manifest)$status, "pass")
  expect_match(manifest$data$hash, "^[0-9a-f]{32}$")
})
