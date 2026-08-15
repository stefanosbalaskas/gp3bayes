test_that("pupil simulation is deterministic and stores truth separately", {
  a <- simulate_pupil_timecourse(
    n_participants = 3, trials_per_participant = 4,
    n_items = 3, sampling_frequency = 20,
    blink_trial_probability = 1, seed = 99
  )
  b <- simulate_pupil_timecourse(
    n_participants = 3, trials_per_participant = 4,
    n_items = 3, sampling_frequency = 20,
    blink_trial_probability = 1, seed = 99
  )
  expect_identical(a$data, b$data)
  expect_identical(a$truth, b$truth)
  expect_true(any(a$data$blink))
  expect_true(anyNA(a$data$pupil_mm))
  expect_equal(a$truth$ar1, 0.55)
})

test_that("preparation records transformations without repairing missingness", {
  sim <- simulate_pupil_timecourse(
    n_participants = 3, trials_per_participant = 4,
    sampling_frequency = 20, seed = 17
  )
  contract <- create_pupil_contract(
    "pupil_mm", "participant_id", "trial_id", "event_time",
    "millimetres", 20, item_col = "item_id", condition_col = "condition",
    blink_col = "blink", interpolation_col = "interpolated",
    validity_col = "valid", gaze_x_col = "gaze_x", gaze_y_col = "gaze_y",
    luminance_col = "luminance", baseline_window = c(-0.5, 0),
    source_vendor = "Gazepoint", device_model = "synthetic"
  )
  x <- prepare_pupil_timecourse(
    sim$data, contract, baseline_operation = "subtract"
  )
  expect_s3_class(x, "gp3bayes_pupil_prepared")
  expect_equal(nrow(x$data), nrow(sim$data))
  expect_true(".pupil_source" %in% names(x$data))
  expect_true(".pupil_model" %in% names(x$data))
  expect_identical(x$baseline_operation, "subtract")
  expect_true(length(x$transformations) >= 1)
  expect_s3_class(x$audit, "gp3bayes_pupil_readiness")
  expect_s3_class(pupil_readiness_table(x$audit, "trial"), "data.frame")
})

test_that("preparation blocks duplicates and double baseline correction", {
  d <- data.frame(
    id = c("p1","p1"), trial = c("t1","t1"),
    time = c(0,0), pupil = c(3,3)
  )
  c1 <- create_pupil_contract(
    "pupil","id","trial","time","millimetres",60
  )
  expect_error(prepare_pupil_timecourse(d, c1), "Duplicated")

  c2 <- create_pupil_contract(
    "pupil","id","trial","time","millimetres",60,
    baseline_applied = TRUE, baseline_window = c(-.2,0)
  )
  d2 <- data.frame(id="p1",trial="t1",time=c(-.2,0,.2),pupil=c(3,3,3.1))
  expect_error(
    prepare_pupil_timecourse(d2, c2, baseline_operation = "subtract"),
    "already applied"
  )
})

test_that("physical unit conversion is restricted", {
  d <- data.frame(id="p1",trial="t1",time=c(0,.1),pupil=c(.003,.0032))
  c1 <- create_pupil_contract("pupil","id","trial","time","metres",10)
  x <- prepare_pupil_timecourse(d,c1,output_unit="millimetres")
  expect_equal(x$data$.pupil_model, c(3,3.2))
  expect_error(
    prepare_pupil_timecourse(
      transform(d, pupil = c(30,32)),
      create_pupil_contract("pupil","id","trial","time","pixels",10),
      output_unit="millimetres"
    ),
    "No deterministic physical conversion"
  )
})


test_that("time units and baseline-ratio transformations are explicit", {
  d <- data.frame(
    id = rep("p1", 4),
    trial = rep("t1", 4),
    time_ms = c(-200, 0, 100, 200),
    pupil = c(4, 4, 4.2, 4.4)
  )
  c1 <- create_pupil_contract(
    "pupil", "id", "trial", "time_ms", "millimetres", 10,
    time_unit = "milliseconds", baseline_window = c(-200, 0)
  )
  x <- prepare_pupil_timecourse(d, c1, baseline_operation = "divide")
  expect_equal(x$data$.event_time, c(-.2, 0, .1, .2))
  expect_identical(x$model_time_unit, "seconds")
  expect_identical(x$model_unit, "ratio")
  expect_equal(x$data$.pupil_model[1:2], c(1, 1))
  expect_true(any(vapply(
    x$transformations,
    function(z) identical(z$operation, "time_unit_conversion"),
    logical(1)
  )))
})

test_that("declared optional identifiers and numeric measurement context are validated", {
  d <- data.frame(
    id = c("p1", "p1"), trial = c("t1", "t1"),
    time = c(0, .1), pupil = c(3, 3.1),
    condition = c("a", NA_character_)
  )
  c1 <- create_pupil_contract(
    "pupil", "id", "trial", "time", "millimetres", 10,
    condition_col = "condition"
  )
  expect_error(prepare_pupil_timecourse(d, c1), "condition")

  d2 <- transform(d, condition = c("a", "a"), gaze = factor(c("1", "2")))
  c2 <- create_pupil_contract(
    "pupil", "id", "trial", "time", "millimetres", 10,
    condition_col = "condition", gaze_x_col = "gaze"
  )
  expect_error(prepare_pupil_timecourse(d2, c2), "must be numeric")
})


test_that("paired pupil channels support vendor-neutral disagreement auditing", {
  d <- data.frame(
    id = rep("p1", 3), trial = rep("t1", 3), time = c(0, .1, .2),
    left = c(3.0, 3.1, 3.2), right = c(3.1, 3.0, 3.3)
  )
  c1 <- create_pupil_contract(
    "left", "id", "trial", "time", "millimetres", 10,
    eye = "left", left_pupil_col = "left", right_pupil_col = "right"
  )
  p <- prepare_pupil_timecourse(d, c1)
  tab <- pupil_readiness_table(p$audit)
  observed <- as.numeric(
    tab$value[tab$metric == "left_right_pupil_disagreement"]
  )
  expect_equal(observed, 0.1, tolerance = 1e-8)
})
