test_that("pupil contract is explicit and scale aware", {
  x <- create_pupil_contract(
    outcome_col = "pupil",
    participant_col = "id",
    trial_col = "trial",
    time_col = "time",
    pupil_unit = "millimetres",
    sampling_frequency = 60,
    condition_col = "condition",
    eye = "combined"
  )
  expect_s3_class(x, "gp3bayes_pupil_contract")
  expect_identical(x$family, "pupil")
  expect_false(x$preprocessing$pfe_corrected)
  expect_false(x$fit_performed)
  expect_error(
    create_pupil_contract("p","id","trial","time","furlongs",60),
    "Unsupported"
  )
  expect_error(
    create_pupil_contract("p","id","trial","time","millimetres",0),
    "positive"
  )
})

test_that("Gazepoint inspection preserves documented unit distinctions", {
  d <- data.frame(
    TIME = c(0, 1/60),
    LPD = c(32, 33),
    RPD = c(31, 32),
    LPV = c(1, 1),
    RPV = c(1, 1),
    LPUPILD = c(.0032, .0033)
  )
  x <- inspect_gazepoint_pupil_schema(d)
  expect_s3_class(x, "gp3bayes_gazepoint_pupil_schema")
  expect_identical(x$status, "ambiguous_pupil_channel")
  tab <- gazepoint_pupil_mapping_table(x)
  expect_identical(tab$unit[tab$field == "LPD"], "pixels")
  expect_identical(tab$unit[tab$field == "LPUPILD"], "metres")
  expect_false(x$automatic_channel_selection)
})

test_that("Gazepoint inspection refuses to invent absent fields", {
  x <- inspect_gazepoint_pupil_schema(data.frame(foo = 1:3))
  expect_identical(x$status, "missing_pupil_channel")
  expect_equal(nrow(x$pupil_candidates), 0)
})
