test_that("optional capability audit is deterministic", {
  result <- bayesian_backend_capabilities()
  expect_s3_class(result, "gp3bayes_backend_capabilities")
  expect_true(all(c("component", "installed", "version", "usable", "detail") %in%
                    names(result)))
  expect_true(all(c("brms", "rstan", "cmdstanr", "loo", "priorsense",
                    "detectseparation", "SBC") %in% result$component))
})

test_that("binary separate interaction prior defaults are recorded", {
  sim <- simulate_hierarchical_binary_data(
    n_participants = 12,
    trials_per_participant = 8,
    n_items = 6,
    random_slope_sd = 0,
    seed = 101
  )
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "item_id",
    trial_col = "trial_id",
    condition_col = "condition",
    predictors = c("participant_covariate", "trial_covariate"),
    interaction = c("condition", "participant_covariate"),
    random_slope = FALSE
  )
  prepared <- prepare_hierarchical_binary_data(
    sim$data,
    contract,
    condition_levels = c("control", "treatment")
  )
  spec <- specify_binary_model_with_interaction_prior(
    prepared,
    baseline = 0.35
  )
  summary <- interaction_prior_summary(spec)
  expect_equal(summary$main_effect_scale, 0.75)
  expect_equal(summary$interaction_scale, 0.50)
  expect_s3_class(spec, "gp3bayes_binary_model_specification")
})

test_that("duration separate interaction prior defaults are recorded", {
  sim <- simulate_hierarchical_duration_data(
    n_participants = 12,
    trials_per_participant = 8,
    n_items = 6,
    random_slope_sd = 0,
    seed = 102
  )
  contract <- create_model_contract(
    family = "duration",
    outcome_col = "duration",
    participant_col = "participant_id",
    item_col = "item_id",
    trial_col = "trial_id",
    condition_col = "condition",
    predictors = c("participant_covariate", "trial_covariate"),
    interaction = c("condition", "participant_covariate"),
    random_slope = FALSE,
    outcome_unit = "milliseconds"
  )
  prepared <- prepare_hierarchical_duration_data(
    sim$data,
    contract,
    condition_levels = c("control", "treatment")
  )
  spec <- specify_duration_model_with_interaction_prior(
    prepared,
    baseline = 500
  )
  summary <- interaction_prior_summary(spec)
  expect_equal(summary$main_effect_scale, 0.35)
  expect_equal(summary$interaction_scale, 0.25)
  expect_s3_class(spec, "gp3bayes_duration_model_specification")
})

test_that("pathological scenarios are explicit and evaluable", {
  binary <- simulate_binary_pathology("rank_deficiency", seed = 103)
  duration <- simulate_duration_pathology("zero_duration", seed = 104)

  expect_s3_class(binary, "gp3bayes_pathological_simulation")
  expect_s3_class(duration, "gp3bayes_pathological_simulation")
  expect_equal(binary$expected_gate, "fail")
  expect_equal(duration$expected_gate, "fail")

  binary_eval <- evaluate_pathological_simulation(binary)
  duration_eval <- evaluate_pathological_simulation(duration)

  expect_s3_class(binary_eval, "gp3bayes_pathology_evaluation")
  expect_s3_class(duration_eval, "gp3bayes_pathology_evaluation")
  expect_equal(binary_eval$actual_structural_status, "fail")
  expect_equal(duration_eval$actual_structural_status, "fail")
})

test_that("semantic duration failures are never silently accepted", {
  censoring <- simulate_duration_pathology("censoring", seed = 105)
  wrong_unit <- simulate_duration_pathology("incorrect_unit", seed = 106)

  censoring_eval <- evaluate_pathological_simulation(censoring)
  wrong_unit_eval <- evaluate_pathological_simulation(wrong_unit)

  expect_true(censoring_eval$semantic_failure)
  expect_true(wrong_unit_eval$semantic_failure)
  expect_equal(censoring_eval$actual_structural_status, "fail")
  expect_equal(wrong_unit_eval$actual_structural_status, "fail")
})

test_that("detectseparation adapter supports raw and specification inputs", {
  skip_if_not_installed("detectseparation")

  separated_data <- data.frame(
    y = c(0, 0, 0, 1, 1, 1),
    x = c(-3, -2, -1, 1, 2, 3)
  )

  result <- detect_binary_separation(
    separated_data,
    y ~ x
  )

  expect_s3_class(
    result,
    "gp3bayes_separation_screen"
  )
  expect_true(result$separation_detected)
  expect_equal(result$status, "review")
  expect_true(result$screening_only)

  plot_file <- tempfile(fileext = ".png")

  grDevices::png(
    filename = plot_file,
    width = 900,
    height = 700
  )

  plot_result <- tryCatch(
    plot(result),
    finally = grDevices::dev.off()
  )

  expect_identical(plot_result, result)
  expect_true(file.exists(plot_file))
  expect_gt(
    unname(file.info(plot_file)$size),
    0
  )

  unlink(plot_file)

  binary_sim <- simulate_hierarchical_binary_data(
    n_participants = 12L,
    trials_per_participant = 8L,
    n_items = 6L,
    random_slope_sd = 0,
    seed = 2026
  )

  binary_contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "item_id",
    trial_col = "trial_id",
    condition_col = "condition",
    predictors = c(
      "participant_covariate",
      "trial_covariate"
    ),
    interaction = c(
      "condition",
      "participant_covariate"
    ),
    random_slope = FALSE
  )

  binary_prepared <- prepare_hierarchical_binary_data(
    binary_sim$data,
    binary_contract,
    condition_levels = c("control", "treatment")
  )

  binary_spec <- specify_binary_model_with_interaction_prior(
    binary_prepared,
    baseline = 0.35
  )

  specification_result <- detect_binary_separation(
    binary_spec
  )

  expect_s3_class(
    specification_result,
    "gp3bayes_separation_screen"
  )
  expect_true(specification_result$screening_only)
  expect_true(
    specification_result$status %in% c("pass", "review")
  )
  expect_equal(
    specification_result$n_observations,
    nrow(binary_prepared$data)
  )

  fixed_formula_text <- paste(
    deparse(specification_result$formula),
    collapse = " "
  )

  expect_false(
    grepl(
      "\\|",
      fixed_formula_text
    )
  )

  expect_true(
    all(
      c(
        "condition",
        "participant_covariate",
        "trial_covariate"
      ) %in% all.vars(specification_result$formula)
    )
  )

  specification_plot <- tempfile(fileext = ".png")

  grDevices::png(
    filename = specification_plot,
    width = 900,
    height = 700
  )

  specification_plot_result <- tryCatch(
    plot(specification_result),
    finally = grDevices::dev.off()
  )

  expect_identical(
    specification_plot_result,
    specification_result
  )
  expect_true(file.exists(specification_plot))
  expect_gt(
    unname(file.info(specification_plot)$size),
    0
  )

  unlink(specification_plot)
})

test_that("LOO adapter works from a log-likelihood matrix", {
  skip_if_not_installed("loo")
  set.seed(107)
  log_lik <- matrix(rnorm(400, -1, 0.25), nrow = 100, ncol = 4)
  result <- compute_psis_loo_from_log_lik(log_lik)
  expect_s3_class(result, "gp3bayes_psis_loo")
  expect_length(result$pareto_k, 4)
  expect_false(result$automatic_selection)
})

test_that("backend wrappers expose only approved backend choices", {
  expect_true(all(c("rstan", "cmdstanr") %in%
                    eval(formals(fit_binary_model_backend)$backend)))
  expect_true(all(c("rstan", "cmdstanr") %in%
                    eval(formals(fit_duration_model_backend)$backend)))
  expect_identical(
    deparse(formals(fit_binary_model_backend)$backend),
    'c("rstan", "cmdstanr")'
  )
})
