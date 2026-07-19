test_that("the approved initial model-family scope is stable", {
  expect_identical(
    names(.gp3bayes_model_families),
    c("binary", "duration")
  )

  expect_identical(
    unname(.gp3bayes_model_families),
    c(
      "hierarchical_binary",
      "hierarchical_lognormal_duration"
    )
  )

  expect_length(
    unique(.gp3bayes_model_families),
    2L
  )
})
