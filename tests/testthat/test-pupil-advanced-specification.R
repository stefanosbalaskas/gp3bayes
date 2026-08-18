test_that("advanced simulation and mapping are deterministic", {
  a <- simulate_advanced_pupil_timecourse(seed = 11, n_participants = 4, trials_per_participant = 2, time_points = 12)
  b <- simulate_advanced_pupil_timecourse(seed = 11, n_participants = 4, trials_per_participant = 2, time_points = 12)
  expect_identical(a$data, b$data)
  expect_identical(a$truth, b$truth)
  expect_s3_class(a, "gp3bayes_pupil_advanced_simulation")
  map <- pupil_advanced_mapping_table(a$data)
  expect_true(all(c("response", "time", "participant", "condition") %in% map$role))
})

test_that("advanced specification supports robust and GP declarations", {
  sim <- simulate_advanced_pupil_timecourse(seed = 12, n_participants = 4, trials_per_participant = 2, time_points = 12)
  dist <- specify_pupil_distribution("student", "condition_time")
  spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    temporal_structure = "gaussian_process",
    distribution = dist,
    gp_spec = create_pupil_gp_spec("matern32", "approximate", k = 12),
    autocorrelation = "none",
    participant_trajectory = "none",
    covariates = c("baseline_pupil", "luminance")
  )
  expect_s3_class(spec, "gp3bayes_pupil_advanced_specification")
  expect_identical(spec$family, "student")
  expect_identical(spec$residual_scale, "condition_time")
  expect_identical(spec$gp_spec$kernel, "matern32")
  expect_false(spec$fit_performed)
  tab <- pupil_advanced_specification_table(spec)
  expect_identical(tab$family, "student")
})

test_that("governed compatibility blocks Student ARMA", {
  sim <- simulate_advanced_pupil_timecourse(seed = 13, n_participants = 4, trials_per_participant = 2, time_points = 12)
  expect_error(
    specify_advanced_pupil_timecourse_model(
      sim$data, family = "student", autocorrelation = "ar1"
    ),
    "does not combine Student-t"
  )
})

test_that("ARMA orders are bounded", {
  expect_s3_class(create_pupil_arma_spec(3, 2), "gp3bayes_pupil_arma_spec")
  expect_error(create_pupil_arma_spec(4, 0), "[0, 3]", fixed = TRUE)
  expect_error(create_pupil_arma_spec(1, 3), "[0, 2]", fixed = TRUE)
  expect_error(create_pupil_arma_spec(2, 1, covariance = TRUE), "restricted")
})

test_that("capability and compatibility tables make exclusions explicit", {
  caps <- pupil_advanced_capabilities()
  expect_true(any(caps$status == "excluded"))
  expect_true(any(grepl("cognitive", caps$capability, ignore.case = TRUE)))
  comp <- pupil_advanced_compatibility_table()
  expect_true(any(comp$status == "blocked"))
})


test_that("integer simulation and ARMA controls do not silently truncate", {
  expect_error(
    simulate_advanced_pupil_timecourse(n_participants = 4.5, trials_per_participant = 2, time_points = 12),
    "integer"
  )
  expect_error(create_pupil_arma_spec(1.5, 0), "integer")
  expect_error(create_pupil_arma_spec(1, 0.5), "integer")
})

test_that("simulation fractions accept zero but reject invalid bounds", {
  expect_s3_class(
    simulate_advanced_pupil_timecourse(
      n_participants = 4, trials_per_participant = 2, time_points = 12,
      outlier_fraction = 0, missing_fraction = 0
    ),
    "gp3bayes_pupil_advanced_simulation"
  )
  expect_error(
    simulate_advanced_pupil_timecourse(
      n_participants = 4, trials_per_participant = 2, time_points = 12,
      missing_fraction = 1
    ),
    "fraction"
  )
})


test_that("condition-dependent advanced layers require actual condition variation", {
  sim <- simulate_advanced_pupil_timecourse(seed = 14, n_participants = 4, trials_per_participant = 2, time_points = 12)
  one <- sim$data
  one$condition <- factor("control")
  base <- specify_advanced_pupil_timecourse_model(
    one, condition_trajectory = FALSE, residual_scale = "constant", autocorrelation = "none"
  )
  expect_false(base$condition_trajectory)
  expect_error(
    specify_advanced_pupil_timecourse_model(one, condition_trajectory = TRUE, autocorrelation = "none"),
    "at least two observed condition levels"
  )
  expect_error(
    specify_advanced_pupil_timecourse_model(one, condition_trajectory = FALSE, residual_scale = "condition", autocorrelation = "none"),
    "Condition-dependent residual scale"
  )
})
