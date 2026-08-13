test_that("declared prior simulation supports approved classes", {
  priors <- structure(
    list(
      table = data.frame(
        parameter_class = c("Intercept", "b", "sd", "sigma", "cor"),
        distribution = c("normal", "normal", "student_t", "student_t", "lkj"),
        location = c(0, 0, 0, 0, NA),
        scale = c(1, 0.5, 1, 1, NA),
        df = c(NA, NA, 3, 3, NA),
        shape = c(NA, NA, NA, NA, 2),
        lower = c(-Inf, -Inf, 0, 0, -1),
        stringsAsFactors = FALSE
      )
    ),
    class = "gp3bayes_prior_specification"
  )
  draws <- simulate_declared_prior_draws(
    priors,
    variables = c(
      "b_Intercept",
      "b_condition",
      "sd_participant__Intercept",
      "sigma",
      "cor_participant__Intercept__condition"
    ),
    ndraws = 500,
    seed = 10
  )
  expect_equal(dim(draws), c(500L, 5L))
  expect_true(all(draws[, "sd_participant__Intercept"] >= 0))
  expect_true(all(draws[, "sigma"] >= 0))
  expect_true(all(abs(draws[, "cor_participant__Intercept__condition"]) <= 1))
})

test_that("prior specification table accessor is stable", {
  priors <- structure(
    list(table = data.frame(parameter_class = "b", distribution = "normal")),
    class = "gp3bayes_prior_specification"
  )
  expect_equal(prior_specification_table(priors), priors$table)
})
