test_that("prediction gradient uses adjacent posterior finite differences", {
  pred <- structure(
    list(draws = rbind(c(0, 1, 2), c(0, 2, 4), c(0, 3, 6))),
    class = "gp3bayes_prediction"
  )
  profile <- structure(
    list(
      variable = "x",
      table = data.frame(
        profile_x = c(0, 1, 2),
        predicted_median = c(0, 2, 4),
        lower = c(0, 1, 2),
        upper = c(0, 3, 6)
      ),
      prediction = pred
    ),
    class = "gp3bayes_prediction_profile"
  )

  out <- prediction_gradient_table(profile)
  expect_equal(nrow(out), 2L)
  expect_true(all(out$gradient_median > 0))
})

test_that("profile and surface graphics return ggplots", {
  skip_if_not_installed("ggplot2")

  profile <- structure(
    list(
      variable = "x",
      table = data.frame(
        profile_x = c(0, 1, 2),
        predicted_median = c(0, 1, 2),
        lower = c(-0.1, 0.9, 1.9),
        upper = c(0.1, 1.1, 2.1)
      ),
      prediction = structure(
        list(draws = matrix(rep(c(0, 1, 2), each = 20), nrow = 20)),
        class = "gp3bayes_prediction"
      )
    ),
    class = "gp3bayes_prediction_profile"
  )

  surface <- structure(
    list(
      x = "x",
      y = "z",
      table = transform(
        expand.grid(surface_x = 1:3, surface_y = 1:3),
        predicted_median = seq_len(9),
        interval_width = 0.5
      )
    ),
    class = "gp3bayes_prediction_surface"
  )

  expect_s3_class(plot_prediction_profile(profile), "ggplot")
  expect_s3_class(plot_prediction_gradient(profile), "ggplot")
  expect_s3_class(plot_prediction_surface(surface), "ggplot")
  expect_s3_class(plot_prediction_surface_uncertainty(surface), "ggplot")
})

test_that("contrast profile table and plot are stable", {
  skip_if_not_installed("ggplot2")

  x <- structure(
    list(
      variable = "x",
      contrast_levels = c("A", "B"),
      measure = "difference",
      table = data.frame(
        profile_x = 1:3,
        contrast_lower = c(-0.2, 0.0, 0.2),
        contrast_median = c(0, 0.2, 0.4),
        contrast_upper = c(0.2, 0.4, 0.6)
      )
    ),
    class = "gp3bayes_prediction_contrast_profile"
  )

  expect_equal(nrow(prediction_contrast_profile_table(x)), 3L)
  expect_s3_class(plot_prediction_contrast_profile(x), "ggplot")
})
