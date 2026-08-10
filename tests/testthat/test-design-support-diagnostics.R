design_fixture <- function(random_slope = FALSE) {
  simulation <- simulate_hierarchical_binary_data(
    n_participants = 10,
    trials_per_participant = 8,
    n_items = 5,
    random_slope_sd = if (random_slope) 0.2 else 0,
    seed = 301
  )
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "item_id",
    trial_col = "trial_id",
    condition_col = "condition",
    predictors = "trial_covariate",
    random_slope = random_slope
  )
  list(data = simulation$data, contract = contract)
}

test_that("missingness audit reports but does not alter data", {
  fixture <- design_fixture()
  data <- fixture$data
  original_rows <- nrow(data)
  data$trial_covariate[1:3] <- NA_real_

  audit <- audit_missingness_structure(data, fixture$contract)

  expect_s3_class(audit, "gp3bayes_missingness_audit")
  expect_equal(nrow(data), original_rows)
  expect_false(audit$automatic_exclusion)
  expect_false(audit$automatic_imputation)
  expect_true(any(audit$column_table$n_missing > 0L))
})

test_that("fixed-effect audit records rank and condition information", {
  fixture <- design_fixture()
  audit <- audit_fixed_effect_design(fixture$data, fixture$contract)

  expect_s3_class(audit, "gp3bayes_fixed_effect_design_audit")
  expect_true(is.numeric(audit$condition_number))
  expect_true(audit$rank <= audit$n_columns)
  expect_false(audit$automatic_reparameterization)
  expect_false(audit$automatic_variable_removal)
})

test_that("random-effect audit preserves declared structure", {
  fixture <- design_fixture(random_slope = TRUE)
  audit <- audit_random_effects_support(fixture$data, fixture$contract)

  expect_s3_class(audit, "gp3bayes_random_effects_support_audit")
  expect_true(audit$random_slope_requested)
  expect_false(audit$automatic_simplification)
  expect_true("random_slope_support" %in% audit$component_table$component)
})

test_that("combined design support is an inspectable pre-fit object", {
  fixture <- design_fixture()
  audit <- audit_design_support(
    fixture$data,
    fixture$contract,
    separation = FALSE,
    strict_readiness = TRUE
  )

  expect_s3_class(audit, "gp3bayes_design_support_audit")
  expect_false(audit$automatic_model_change)
  expect_false(audit$automatic_exclusion)
  expect_true(all(c(
    "standard_readiness", "strict_readiness", "missingness",
    "fixed_effect_design", "random_effects_support"
  ) %in% audit$component_table$component))
})

test_that("pre-fit design support covers duration data", {
  simulation <- simulate_hierarchical_duration_data(
    n_participants = 8,
    trials_per_participant = 6,
    n_items = 4,
    random_slope_sd = 0,
    seed = 302
  )
  contract <- create_model_contract(
    "duration", "duration", "participant_id",
    item_col = "item_id", trial_col = "trial_id",
    condition_col = "condition", outcome_unit = "milliseconds"
  )

  audit <- audit_design_support(
    simulation$data,
    contract,
    separation = FALSE,
    strict_readiness = TRUE
  )

  expect_s3_class(audit, "gp3bayes_design_support_audit")
  expect_identical(audit$family, "duration")
  expect_true(is.null(audit$separation))
})


test_that("item crossing ignores rows with missing item identifiers", {
  fixture <- design_fixture()
  data <- fixture$data
  data$item_id[1] <- NA_character_

  audit <- audit_random_effects_support(data, fixture$contract)

  expect_s3_class(audit, "gp3bayes_random_effects_support_audit")
  expect_true(all(audit$item_table$n_participants <= length(unique(data$participant_id))))
})


test_that("fixed-effect audit returns a fail object when no complete design rows remain", {
  fixture <- design_fixture()
  data <- fixture$data
  data$selected[] <- NA_real_

  audit <- audit_fixed_effect_design(data, fixture$contract)

  expect_s3_class(audit, "gp3bayes_fixed_effect_design_audit")
  expect_identical(audit$status, "fail")
  expect_true(is.character(audit$error))
})

test_that("random-effect audit returns a fail object when participant data are absent", {
  fixture <- design_fixture()
  data <- fixture$data
  data$participant_id <- NULL

  audit <- audit_random_effects_support(data, fixture$contract)

  expect_s3_class(audit, "gp3bayes_random_effects_support_audit")
  expect_identical(audit$status, "fail")
  expect_true(is.character(audit$error))
})
