test_that("measurement model is explicit and auditable", {
  sim <- simulate_advanced_pupil_timecourse(seed = 21, n_participants = 4, trials_per_participant = 2, time_points = 12)
  mm <- create_pupil_measurement_model(
    baseline_error = "baseline_se",
    luminance_error = "luminance_se",
    response_error = "pupil_se"
  )
  spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    covariates = c("baseline_pupil", "luminance"),
    measurement_model = mm,
    autocorrelation = "none"
  )
  audit <- audit_pupil_measurement_model(spec)
  expect_s3_class(audit, "gp3bayes_pupil_measurement_audit_05")
  expect_true(all(audit$table$status %in% c("pass", "review")))
  expect_equal(nrow(pupil_measurement_uncertainty_table(spec)), 3)
})

test_that("invalid standard errors are rejected before fitting", {
  sim <- simulate_advanced_pupil_timecourse(seed = 22, n_participants = 4, trials_per_participant = 2, time_points = 12)
  sim$data$baseline_se[1] <- -1
  mm <- create_pupil_measurement_model(baseline_error = "baseline_se")
  expect_error(
    specify_advanced_pupil_timecourse_model(
      sim$data, covariates = "baseline_pupil", measurement_model = mm
    ),
    "strictly positive"
  )
})

test_that("missingness specification declares MAR rather than inferring it", {
  sim <- simulate_advanced_pupil_timecourse(seed = 23, n_participants = 4, trials_per_participant = 2, time_points = 12)
  ms <- create_pupil_missingness_spec(response = "model", predictors = "luminance", assumptions = "MAR")
  spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    covariates = "luminance",
    missingness_model = ms,
    autocorrelation = "none"
  )
  audit <- audit_pupil_missingness(spec)
  expect_s3_class(audit, "gp3bayes_pupil_missingness_audit")
  expect_identical(audit$assumptions, "MAR")
  expect_true(any(audit$table$role == "response"))
  expect_error(create_pupil_missingness_spec(assumptions = "MNAR"), "does not implement MNAR")
})

test_that("missing-response model and ARMA are blocked", {
  sim <- simulate_advanced_pupil_timecourse(seed = 24, n_participants = 4, trials_per_participant = 2, time_points = 12)
  ms <- create_pupil_missingness_spec(response = "model")
  expect_error(
    specify_advanced_pupil_timecourse_model(sim$data, missingness_model = ms, autocorrelation = "ar1"),
    "cannot currently be combined"
  )
})


test_that("missingness audit handles degenerate time grids without cut failures", {
  sim <- simulate_advanced_pupil_timecourse(seed = 25, n_participants = 4, trials_per_participant = 2, time_points = 8)
  one_time <- sim$data[sim$data$time_ms == sim$data$time_ms[[1L]], , drop = FALSE]
  ms <- create_pupil_missingness_spec(response = "model")
  spec <- specify_advanced_pupil_timecourse_model(
    one_time,
    temporal_structure = "linear",
    condition_trajectory = FALSE,
    missingness_model = ms,
    autocorrelation = "none"
  )
  a <- audit_pupil_missingness(spec)
  expect_equal(nrow(a$by_time), 1L)
  expect_identical(as.character(a$by_time$time_bin[[1L]]), "all_times")
})
