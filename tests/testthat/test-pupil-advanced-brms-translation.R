test_that("advanced brms translations build without fitting", {
  skip_if_not_installed("brms")
  sim <- simulate_advanced_pupil_timecourse(seed = 81, n_participants = 4, trials_per_participant = 2, time_points = 12)

  gaussian <- specify_advanced_pupil_timecourse_model(
    sim$data,
    family = "gaussian",
    residual_scale = "condition",
    autocorrelation = "ar1",
    covariates = "luminance"
  )
  tr <- translate_advanced_pupil_model_to_brms(gaussian)
  expect_s3_class(tr, "gp3bayes_pupil_advanced_brms_specification")
  expect_false(tr$fit_performed)
  expect_true(nrow(tr$data) > 0)

  robust <- specify_advanced_pupil_timecourse_model(
    sim$data,
    family = "student",
    temporal_structure = "gaussian_process",
    gp_spec = create_pupil_gp_spec("matern32", "approximate", 10),
    autocorrelation = "none"
  )
  tr2 <- translate_advanced_pupil_model_to_brms(robust)
  expect_s3_class(tr2, "gp3bayes_pupil_advanced_brms_specification")
})

test_that("measurement and missing predictor translation uses mi submodels", {
  skip_if_not_installed("brms")
  sim <- simulate_advanced_pupil_timecourse(seed = 82, n_participants = 4, trials_per_participant = 2, time_points = 12)
  sim$data$luminance[c(2, 20)] <- NA_real_
  mm <- create_pupil_measurement_model(baseline_error = "baseline_se")
  ms <- create_pupil_missingness_spec(response = "model", predictors = "luminance")
  spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    covariates = c("baseline_pupil", "luminance"),
    measurement_model = mm,
    missingness_model = ms,
    autocorrelation = "none"
  )
  tr <- translate_advanced_pupil_model_to_brms(spec)
  expect_s3_class(tr, "gp3bayes_pupil_advanced_brms_specification")
  expect_false(inherits(tr$family, "brmsfamily"))
  expect_true(all(vapply(tr$family, inherits, logical(1L), what = "brmsfamily")))
  pr <- as.data.frame(tr$priors)
  response_priors <- unique(pr$resp[!is.na(pr$resp) & nzchar(pr$resp)])
  expect_gte(length(response_priors), 2L)
})

test_that("binocular brms translation is multivariate without fitting", {
  skip_if_not_installed("brms")
  sim <- simulate_binocular_pupil_timecourse(seed = 83, n_participants = 4, trials_per_participant = 2, time_points = 12)
  prep <- prepare_binocular_pupil_timecourse(sim$data)
  spec <- specify_binocular_pupil_model(prep)
  tr <- translate_binocular_pupil_model_to_brms(spec)
  expect_s3_class(tr, "gp3bayes_binocular_brms_specification")
})

test_that("nonlinear response-shape translation is inspectable", {
  skip_if_not_installed("brms")
  sim <- simulate_pupil_response_shape(seed = 84, n_participants = 4, trials_per_participant = 2, time_points = 12)
  spec <- specify_pupil_response_shape_model(sim$data)
  tr <- translate_pupil_response_shape_to_brms(spec)
  expect_s3_class(tr, "gp3bayes_pupil_response_shape_brms_specification")
  pr <- as.data.frame(tr$priors)
  onset_rows <- pr[pr$nlpar == "onset" & !is.na(pr$coef) & nzchar(pr$coef), , drop = FALSE]
  if (nrow(onset_rows)) {
    intercept <- onset_rows[onset_rows$coef == "Intercept", , drop = FALSE]
    slopes <- onset_rows[onset_rows$coef != "Intercept", , drop = FALSE]
    if (nrow(intercept)) expect_false(all(grepl("normal\\(0,", intercept$prior)))
    if (nrow(slopes)) expect_true(all(grepl("normal\\(0,", slopes$prior)))
  }
})

test_that("combined response missingness and known error use one mi(sdy=...) response term", {
  skip_if_not_installed("brms")
  sim <- simulate_advanced_pupil_timecourse(
    n_participants = 4, trials_per_participant = 2,
    time_points = 8, time_range = c(0, 700),
    missing_fraction = 0.05,
    measurement_error_sd = 0.03,
    seed = 91
  )
  mm <- create_pupil_measurement_model(response_error = "pupil_se")
  ms <- create_pupil_missingness_spec(response = "model")
  sp <- specify_advanced_pupil_timecourse_model(
    sim$data,
    family = "gaussian",
    autocorrelation = "none",
    measurement_model = mm,
    missingness_model = ms
  )
  tr <- translate_advanced_pupil_model_to_brms(sp)
  txt <- paste(deparse(tr$formula), collapse = " ")
  expect_match(txt, "mi")
  expect_match(txt, "pupil_se")
})
