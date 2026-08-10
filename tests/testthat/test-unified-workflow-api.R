test_that("object validation recognizes contracts and specifications", {
  simulation <- simulate_hierarchical_binary_data(
    n_participants = 8,
    trials_per_participant = 6,
    n_items = 4,
    random_slope_sd = 0,
    seed = 101
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
  specification <- specify_binary_model(prepared, baseline = 0.35)

  contract_check <- validate_gp3bayes_object(contract)
  specification_check <- validate_gp3bayes_object(specification)

  expect_s3_class(contract_check, "gp3bayes_object_validation")
  expect_identical(contract_check$status, "pass")
  expect_s3_class(specification_check, "gp3bayes_object_validation")
  expect_identical(specification_check$status, "pass")
})

test_that("strict object validation rejects non-gp3bayes objects", {
  expect_error(
    validate_gp3bayes_object(list(a = 1), strict = TRUE),
    "validation failed"
  )
})

test_that("workflow status reports pre-fit stages without adequacy claims", {
  simulation <- simulate_hierarchical_binary_data(
    n_participants = 8,
    trials_per_participant = 6,
    n_items = 4,
    random_slope_sd = 0,
    seed = 102
  )
  contract <- create_model_contract(
    "binary", "selected", "participant_id",
    item_col = "item_id",
    trial_col = "trial_id",
    condition_col = "condition"
  )
  prepared <- prepare_hierarchical_binary_data(
    simulation$data, contract,
    condition_levels = c("control", "treatment")
  )
  specification <- specify_binary_model(prepared, baseline = 0.35)

  status <- model_workflow_status(specification)

  expect_s3_class(status, "gp3bayes_workflow_status")
  expect_true(status$completed[status$stage == "contract"])
  expect_true(status$completed[status$stage == "prepared_data"])
  expect_true(status$completed[status$stage == "specification"])
  expect_false(status$completed[status$stage == "fit"])
})

test_that("unified fit wrappers reject unsupported inputs", {
  expect_error(diagnose_model_fit(structure(list(), class = "not_a_fit")))
  expect_error(summarise_model_posterior(structure(list(), class = "not_a_fit")))
  expect_error(check_model_ppc(structure(list(), class = "not_a_fit")))
  expect_error(estimate_model_estimands(structure(list(), class = "not_a_fit")))
})

test_that("unified structural API covers duration specifications", {
  simulation <- simulate_hierarchical_duration_data(
    n_participants = 8,
    trials_per_participant = 6,
    n_items = 4,
    random_slope_sd = 0,
    seed = 103
  )
  contract <- create_model_contract(
    "duration", "duration", "participant_id",
    item_col = "item_id",
    trial_col = "trial_id",
    condition_col = "condition",
    outcome_unit = "milliseconds"
  )
  prepared <- prepare_hierarchical_duration_data(
    simulation$data,
    contract,
    condition_levels = c("control", "treatment")
  )
  specification <- specify_duration_model(prepared, baseline = 500)

  validation <- validate_gp3bayes_object(specification)
  workflow <- model_workflow_status(specification)

  expect_identical(validation$status, "pass")
  expect_true(workflow$completed[workflow$stage == "specification"])
  expect_false(workflow$completed[workflow$stage == "fit"])
})


test_that("workflow status derives fitted stages from evidence objects", {
  fit <- structure(
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
  evidence <- collect_model_evidence(fit = fit)
  status <- model_workflow_status(evidence)

  expect_true(status$completed[status$stage == "fit"])
})
