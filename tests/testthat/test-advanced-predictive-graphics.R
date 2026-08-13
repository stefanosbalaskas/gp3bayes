test_that("advanced predictive plots return ggplot objects", {
  skip_if_not_installed("ggplot2")

  roc <- data.frame(
    threshold = c(Inf, 0.5, -Inf),
    false_positive_rate = c(0, 0.2, 1),
    true_positive_rate = c(0, 0.8, 1)
  )
  pr <- data.frame(
    threshold = c(Inf, 0.5, -Inf),
    recall = c(0, 0.8, 1),
    precision = c(1, 0.9, 0.5)
  )
  qq <- data.frame(
    observed_quantile = c(1, 2, 3),
    predictive_mean_quantile = c(1.1, 2.1, 2.9),
    predictive_lower_quantile = c(0.8, 1.7, 2.4),
    predictive_upper_quantile = c(1.4, 2.5, 3.4)
  )

  expect_s3_class(plot_binary_roc(roc), "ggplot")
  expect_s3_class(plot_binary_precision_recall(pr), "ggplot")
  expect_s3_class(plot_duration_qq(qq), "ggplot")
})
