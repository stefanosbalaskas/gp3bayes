make_validation_prepared <- function() {
  sim <- simulate_pupil_timecourse(
    n_participants=5,trials_per_participant=5,
    sampling_frequency=10,blink_trial_probability=0,seed=33
  )
  c <- create_pupil_contract(
    "pupil_mm","participant_id","trial_id","event_time",
    "millimetres",10,condition_col="condition",
    interpolation_col="interpolated",blink_col="blink",
    gaze_x_col="gaze_x",gaze_y_col="gaze_y",luminance_col="luminance",
    baseline_window=c(-.5,0)
  )
  prepare_pupil_timecourse(sim$data,c)
}

test_that("validation partitions match declared targets without leakage", {
  p <- make_validation_prepared()
  tr <- create_pupil_validation_plan(p,"new_trial_known_participant",K=3,seed=1)
  expect_false(tr$leakage_detected)
  by_series <- split(tr$fold_id,p$data$.series_id)
  expect_true(all(vapply(by_series,function(z) length(unique(z))==1L,logical(1))))

  np <- create_pupil_validation_plan(p,"new_participant",K=3,seed=1)
  expect_false(np$leakage_detected)
  by_p <- split(np$fold_id,p$data$.participant)
  expect_true(all(vapply(by_p,function(z) length(unique(z))==1L,logical(1))))

  fu <- create_pupil_validation_plan(p,"future_segment",future_fraction=.2)
  expect_false(fu$leakage_detected)
  expect_identical(fu$strategy,"leave_future_segment_out")
})

test_that("sensitivity suites are declarative and do not select winners", {
  p <- make_validation_prepared()
  s <- specify_pupil_timecourse_model(p,autocorrelation="none",smooth_basis_dimension=5)
  suite <- create_pupil_sensitivity_suite(
    s,
    baseline_windows=list(c(-.5,-.1)),
    baseline_window_operation="subtract",
    interpolation_policy=c("retain","exclude_flagged"),
    gaze_adjustment=c("none","declared_covariates"),
    luminance_adjustment=c("none","declared_covariate"),
    smooth_basis_dimensions=c(4L,6L),
    autocorrelation=c("none"),
    analysis_windows=list(c(.2,.8))
  )
  expect_s3_class(suite,"gp3bayes_pupil_sensitivity")
  expect_false(suite$automatic_effect_maximization)
  expect_true(nrow(pupil_sensitivity_table(suite)) >= 9)
  one <- materialize_pupil_sensitivity_scenario(
    suite,
    suite$scenarios$scenario_id[suite$scenarios$axis=="smooth_basis_dimension"][1]
  )
  expect_false(one$fit_performed)
  expect_s3_class(one$specification,"gp3bayes_pupil_model_specification")
})

test_that("sensitivity comparison combines estimands without ranking", {
  grid <- data.frame(.event_time=seq(0,1,length.out=5))
  a <- as_pupil_prediction_draws(matrix(rnorm(500),100,5),grid,"standardized")
  e1 <- estimate_pupil_window(a,c(.1,.9))
  e2 <- estimate_pupil_window(a,c(.2,.8))
  x <- compare_pupil_sensitivity_estimands(list(S001=e1,S002=e2))
  expect_false(x$automatic_selection)
  expect_setequal(unique(x$table$scenario_id),c("S001","S002"))
})


test_that("baseline-window sensitivity requires an explicit operation when baseline is none", {
  p <- make_validation_prepared()
  s <- specify_pupil_timecourse_model(
    p, autocorrelation = "none", smooth_basis_dimension = 5
  )
  expect_error(
    create_pupil_sensitivity_suite(
      s, baseline_windows = list(c(-0.5, -0.1))
    ),
    "baseline_window_operation"
  )
})

test_that("PFE sensitivity accepts only explicit prepared alternatives", {
  p <- make_validation_prepared()
  s <- specify_pupil_timecourse_model(
    p, autocorrelation = "none", smooth_basis_dimension = 5
  )
  alt <- p
  alt$contract$preprocessing$pfe_corrected <- TRUE
  alt$contract$preprocessing$pfe_method <- "upstream-test-method"
  suite <- create_pupil_sensitivity_suite(
    s, pfe_prepared = list(upstream_corrected = alt)
  )
  row <- suite$scenarios[suite$scenarios$axis == "pfe_prepared", , drop = FALSE]
  expect_equal(nrow(row), 1L)
  materialized <- materialize_pupil_sensitivity_scenario(
    suite, row$scenario_id
  )
  expect_s3_class(materialized$prepared, "gp3bayes_pupil_prepared")
  expect_false(materialized$pfe_correction_performed)
  expect_false(suite$automatic_effect_maximization)
})

test_that("validation plans exclude missing outcome rows from fold vectors", {
  p <- make_validation_prepared()
  p$data$.pupil_model[1:3] <- NA_real_
  plan <- create_pupil_validation_plan(
    p, target = "new_participant", K = 3L, seed = 7
  )
  expect_equal(length(plan$fold_id), sum(!is.na(p$data$.pupil_model)))
  expect_equal(plan$n_rows, length(plan$fold_id))
  expect_false(plan$leakage_detected)
})

test_that("zero divergence counts pass independent of integer storage", {
  divergence <- sum(c(FALSE, FALSE), na.rm = TRUE)

  expect_type(divergence, "integer")
  expect_equal(divergence, 0)

  status <- if (
    is.finite(divergence) && divergence == 0
  ) "pass" else "review"

  expect_identical(status, "pass")

  sampler_body <- paste(
    deparse(.gp3p_sampler_evidence),
    collapse = "\n"
  )

  expect_true(
    grepl(
      "is\\.finite\\(divergence\\)\\s*&&\\s*divergence\\s*==\\s*0",
      sampler_body,
      perl = TRUE
    )
  )
  expect_false(
    grepl(
      "identical(divergence, 0)",
      sampler_body,
      fixed = TRUE
    )
  )
})
