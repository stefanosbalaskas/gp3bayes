test_that("recovery adapters retain recorded evidence", {
  x <- structure(
    list(
      parameter_summary = data.frame(
        variable = c("a", "b"),
        standardized_bias = c(0.1, -0.2),
        coverage = c(0.9, 0.85),
        rmse = c(0.2, 0.3)
      ),
      estimates = data.frame(
        variable = c("a", "a"),
        truth = c(0, 0),
        median = c(0.1, -0.1),
        lower = c(-0.2, -0.3),
        upper = c(0.3, 0.2),
        repetition = 1:2
      ),
      fit_status = data.frame(
        repetition = 1:2,
        diagnostic_status = c("pass", "pass"),
        completed = TRUE
      )
    ),
    class = "gp3bayes_recovery"
  )
  expect_equal(nrow(recovery_parameter_table(x)), 2L)
  expect_equal(nrow(recovery_estimate_table(x)), 2L)
  expect_equal(nrow(recovery_fit_status_table(x)), 2L)
})

test_that("sensitivity adapters expose existing tables", {
  p <- structure(
    list(
      comparison = data.frame(
        scenario = c("tight", "wide"),
        scale_multiplier = c(0.5, 2),
        variable = c("b_x", "b_x"),
        standardized_shift = c(0.1, 0.2)
      ),
      scenario_status = data.frame(
        scenario = c("tight", "wide"),
        maximum_standardized_shift = c(0.1, 0.2)
      )
    ),
    class = "gp3bayes_prior_sensitivity"
  )
  expect_equal(nrow(prior_sensitivity_table(p)), 2L)
  expect_equal(nrow(prior_sensitivity_scenario_table(p)), 2L)
})

test_that("recovery plots return ggplot objects", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(
    variable = c("a", "b"),
    standardized_bias = c(0.1, -0.1),
    coverage = c(0.9, 0.85),
    rmse = c(0.2, 0.3)
  )
  expect_s3_class(plot_recovery_bias(d), "ggplot")
  expect_s3_class(plot_recovery_coverage(d), "ggplot")
  expect_s3_class(plot_recovery_rmse(d), "ggplot")
})
