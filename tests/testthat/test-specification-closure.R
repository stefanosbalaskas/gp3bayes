make_binary_closure_objects <- function(n_participants = 12L) {
  sim <- simulate_hierarchical_binary_data(
    n_participants = n_participants,
    trials_per_participant = 8L,
    n_items = 6L,
    random_slope_sd = 0.2,
    seed = 4101L
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
    condition_levels = c("control", "treatment"),
    scale_predictors = c("participant_covariate", "trial_covariate")
  )
  specification <- specify_binary_model_with_interaction_prior(
    prepared,
    baseline = 0.35
  )
  list(sim = sim, contract = contract, prepared = prepared, specification = specification)
}

make_duration_closure_objects <- function(n_participants = 12L) {
  sim <- simulate_hierarchical_duration_data(
    n_participants = n_participants,
    trials_per_participant = 8L,
    n_items = 6L,
    random_slope_sd = 0.1,
    outcome_unit = "milliseconds",
    seed = 4201L
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
    condition_levels = c("control", "treatment"),
    scale_predictors = c("participant_covariate", "trial_covariate")
  )
  specification <- specify_duration_model_with_interaction_prior(
    prepared,
    baseline = 500
  )
  list(sim = sim, contract = contract, prepared = prepared, specification = specification)
}

test_that("condition balance uses explicit review and failure thresholds", {
  obj <- make_binary_closure_objects()
  balanced <- summarise_condition_balance(obj$sim$data, obj$contract)
  expect_s3_class(balanced, "gp3bayes_condition_balance")
  expect_equal(nrow(balanced$table), 2L)
  expect_true(balanced$status %in% c("pass", "review"))

  dat <- obj$sim$data
  dat$condition <- "control"
  dat$condition[[1L]] <- "treatment"
  imbalanced <- summarise_condition_balance(
    dat,
    obj$contract,
    warning_fraction = 0.10,
    failure_fraction = 0.02
  )
  expect_equal(imbalanced$status, "fail")
  expect_lt(imbalanced$minimum_fraction, 0.02)
})

test_that("binary group variation reports constant participants without deleting them", {
  obj <- make_binary_closure_objects()
  dat <- obj$sim$data
  first <- unique(dat$participant_id)[[1L]]
  dat$selected[dat$participant_id == first] <- 0L
  result <- summarise_binary_group_variation(dat, obj$contract, "participant")
  expect_s3_class(result, "gp3bayes_binary_group_variation")
  expect_gte(result$n_no_variation, 1L)
  expect_true(first %in% result$table$group_id[!result$table$variation])
})

test_that("identifier-like numeric predictor audit is heuristic and explicit", {
  obj <- make_binary_closure_objects()
  dat <- obj$sim$data
  dat$row_id <- seq_len(nrow(dat))
  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "item_id",
    trial_col = "trial_id",
    condition_col = "condition",
    predictors = c("participant_covariate", "row_id"),
    random_slope = FALSE
  )
  result <- identify_identifier_like_predictors(dat, contract)
  expect_s3_class(result, "gp3bayes_identifier_predictor_audit")
  expect_true("row_id" %in% result$flagged)
  expect_equal(result$status, "review")
})

test_that("duration extremes are review signals and values are retained", {
  obj <- make_duration_closure_objects()
  dat <- obj$sim$data
  dat$duration[[1L]] <- max(dat$duration) * 100
  result <- review_duration_extremes(dat, obj$contract)
  expect_s3_class(result, "gp3bayes_duration_extreme_review")
  expect_gte(result$n_flagged, 1L)
  expect_equal(nrow(dat), result$n)
})

test_that("duration boundary audit recognises declared range and censoring", {
  obj <- make_duration_closure_objects()
  dat <- obj$sim$data
  dat$censored <- FALSE
  dat$censored[[1L]] <- TRUE
  upper <- stats::quantile(dat$duration, 0.90, names = FALSE)
  result <- audit_duration_boundaries(
    dat,
    obj$contract,
    allowed_range = c(10, upper),
    censor_col = "censored"
  )
  expect_s3_class(result, "gp3bayes_duration_boundary_audit")
  expect_equal(result$status, "fail")
  expect_true(any(result$checks$check_id == "uncensored_contract"))
  expect_true(any(result$checks$check_id == "declared_duration_range"))
})

test_that("strict readiness adds the specification-closure checks", {
  b <- make_binary_closure_objects()
  strict_b <- audit_model_readiness_strict(
    b$sim$data,
    b$contract,
    run_separation = FALSE
  )
  expect_s3_class(strict_b, "gp3bayes_strict_readiness_audit")
  expect_true(all(c(
    "overall_condition_balance",
    "participant_binary_outcome_variation",
    "identifier_like_predictors",
    "fixed_effect_rank"
  ) %in% strict_b$checks$check_id))

  d <- make_duration_closure_objects()
  strict_d <- audit_model_readiness_strict(
    d$sim$data,
    d$contract,
    duration_allowed_range = c(1, max(d$sim$data$duration) * 2),
    run_separation = FALSE
  )
  expect_s3_class(strict_d, "gp3bayes_strict_readiness_audit")
  expect_true(all(c(
    "duration_extreme_review",
    "declared_duration_range",
    "uncensored_contract"
  ) %in% strict_d$checks$check_id))
})

test_that("binary transformation recipes round-trip exactly", {
  obj <- make_binary_closure_objects()
  recipe <- create_transformation_recipe(obj$prepared)
  expect_s3_class(recipe, "gp3bayes_transformation_recipe")
  audit <- validate_transformation_replay(obj$prepared)
  expect_s3_class(audit, "gp3bayes_transformation_replay_audit")
  expect_equal(audit$status, "pass")
  raw <- invert_transformation_recipe(obj$prepared$data, recipe)
  replay <- apply_transformation_recipe(raw, recipe, require_outcome = TRUE)
  expect_equal(replay$participant_covariate, obj$prepared$data$participant_covariate)
  expect_equal(replay$trial_covariate, obj$prepared$data$trial_covariate)
})

test_that("duration transformation recipes preserve units and scaled predictors", {
  obj <- make_duration_closure_objects()
  recipe <- create_transformation_recipe(obj$prepared)
  audit <- validate_transformation_replay(obj$prepared)
  expect_equal(audit$status, "pass")
  raw <- invert_transformation_recipe(obj$prepared$data, recipe)
  replay <- apply_transformation_recipe(
    raw,
    recipe,
    require_outcome = TRUE,
    input_unit = "milliseconds"
  )
  expect_equal(replay$duration, obj$prepared$data$duration)
  expect_error(
    apply_transformation_recipe(raw, recipe, input_unit = "seconds"),
    "does not match"
  )
})

test_that("random-slope and deletion sensitivity plans remain non-selecting", {
  obj <- make_binary_closure_objects(8L)
  slope <- create_random_slope_sensitivity_plan(obj$specification)
  expect_s3_class(slope, "gp3bayes_random_slope_sensitivity_plan")
  expect_false(slope$automatic_selection)
  expect_true(slope$intercept_only$ready)
  expect_true(slope$random_slope$ready)

  units <- unique(as.character(obj$prepared$data$participant_id))[1:2]
  deletion <- create_group_deletion_sensitivity_plan(
    obj$specification,
    group = "participant",
    units = units
  )
  expect_s3_class(deletion, "gp3bayes_group_deletion_sensitivity_plan")
  expect_false(deletion$automatic_exclusion)
  expect_equal(nrow(deletion$table), 2L)
})

test_that("contrast and predictor-scaling sensitivity specifications are explicit", {
  obj <- make_binary_closure_objects()
  contrast <- create_contrast_coding_sensitivity_specification(
    obj$specification,
    condition_coding = c(0, 1),
    baseline = 0.35
  )
  expect_s3_class(contrast, "gp3bayes_model_specification")
  expect_equal(as.numeric(contrast$prepared$transformations$condition$coding), c(0, 1))

  scaled <- create_predictor_scaling_sensitivity_specification(
    obj$specification,
    predictor = "trial_covariate",
    scale_factor = 2,
    coefficient_scale = 1.5,
    interaction_scale = 0.5
  )
  expect_s3_class(scaled, "gp3bayes_model_specification")
  old_scale <- obj$prepared$transformations$numeric_scaling$trial_covariate[["scale"]]
  new_scale <- scaled$prepared$transformations$numeric_scaling$trial_covariate[["scale"]]
  expect_equal(unname(new_scale), unname(old_scale) * 2)
})

test_that("duration unit sensitivity rescales the stored analysis unit", {
  obj <- make_duration_closure_objects()
  seconds <- create_duration_unit_sensitivity_specification(
    obj$specification,
    multiplier = 0.001,
    new_unit = "seconds"
  )
  expect_s3_class(seconds, "gp3bayes_duration_model_specification")
  expect_equal(seconds$prepared$outcome_unit, "seconds")
  expect_equal(
    seconds$prepared$data$duration,
    obj$prepared$data$duration * 0.001
  )
})

test_that("estimand summary, comparison and invariance are governed", {
  make_est <- function(values, family = "binary", quantity = "probability_difference") {
    structure(list(
      family = family,
      primary_quantity = quantity,
      draws = data.frame(.draw = seq_along(values), value = values),
      metadata = list(),
      automatic_decision = FALSE
    ), class = "gp3bayes_estimand")
  }
  ref <- make_est(seq(-0.05, 0.15, length.out = 100))
  names(ref$draws)[names(ref$draws) == "value"] <- "probability_difference"
  alt <- make_est(seq(-0.04, 0.16, length.out = 100))
  names(alt$draws)[names(alt$draws) == "value"] <- "probability_difference"
  summary <- summarise_estimand_draws(ref)
  expect_true(all(c("mean", "median", "lower", "upper") %in% names(summary)))
  comparison <- compare_estimand_sensitivity(ref, list(alternative = alt))
  expect_s3_class(comparison, "gp3bayes_estimand_sensitivity")
  expect_false(comparison$robustness_established)
  audit <- audit_estimand_invariance(ref, alt, tolerance = 0.02)
  expect_true(audit$status %in% c("pass", "review"))
})

test_that("duration-unit invariance compares ratios and scaled quantities", {
  make_duration_est <- function(multiplier) {
    base <- seq(400, 600, length.out = 100)
    structure(list(
      family = "duration",
      primary_quantity = "conditional_median_ratio",
      draws = data.frame(
        .draw = seq_along(base),
        conditional_median_ratio = rep(1.2, length(base)),
        predictive_quantile_ratio = rep(1.1, length(base)),
        reference_average_conditional_median = base * multiplier,
        focal_average_conditional_median = base * 1.2 * multiplier,
        reference_predictive_quantile = base * 1.8 * multiplier,
        focal_predictive_quantile = base * 1.98 * multiplier
      )
    ), class = "gp3bayes_estimand")
  }
  ref <- make_duration_est(1)
  sec <- make_duration_est(0.001)
  result <- audit_duration_unit_invariance(ref, sec, 0.001, tolerance = 1e-8)
  expect_equal(result$status, "pass")
  expect_true(result$invariance_established)
})

test_that("specification traceability documents all closure areas", {
  trace <- gp3bayes_specification_traceability()
  expect_true(is.data.frame(trace))
  expect_true(all(c("requirement", "implementation", "status", "automatic_decision") %in% names(trace)))
  expect_true(any(grepl("condition imbalance", trace$requirement, ignore.case = TRUE)))
  expect_true(any(grepl("K-fold", trace$requirement, ignore.case = TRUE)))
})

test_that("closure plot methods render to a graphics device", {
  obj <- make_binary_closure_objects()
  balance <- summarise_condition_balance(obj$sim$data, obj$contract)
  strict <- audit_model_readiness_strict(obj$sim$data, obj$contract, run_separation = FALSE)
  replay <- validate_transformation_replay(obj$prepared)

  path <- tempfile(fileext = ".png")
  grDevices::png(path, width = 1000, height = 700)
  tryCatch({
    plot(balance)
    plot(strict, type = "status")
    plot(replay)
  }, finally = grDevices::dev.off())
  expect_true(file.exists(path))
  expect_gt(unname(file.info(path)$size), 0)
  unlink(path)
})

test_that("K-fold adapter rejects non-gp3bayes fits before expensive work", {
  expect_error(compute_kfold_cv(list()), "gp3bayes_fit")
})
