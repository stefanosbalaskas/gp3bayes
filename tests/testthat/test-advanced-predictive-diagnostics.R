test_that("binary ROC and precision-recall curves are deterministic", {
  p <- c(0.05, 0.2, 0.8, 0.95)
  y <- c(0, 0, 1, 1)

  roc <- binary_roc_curve(p, y)
  pr <- binary_precision_recall_curve(p, y)

  expect_true(all(c("false_positive_rate", "true_positive_rate") %in% names(roc)))
  expect_true(all(c("recall", "precision") %in% names(pr)))
  expect_true(all(roc$false_positive_rate >= 0 & roc$false_positive_rate <= 1))
  expect_true(all(roc$true_positive_rate >= 0 & roc$true_positive_rate <= 1))
})

test_that("binary calibration error and confusion table are conservative", {
  p <- c(0.1, 0.2, 0.8, 0.9)
  y <- c(0, 0, 1, 1)
  ce <- binary_calibration_error(p, y, bins = 2)
  ct <- binary_confusion_table(p, y, threshold = 0.5)

  expect_true(ce$expected_calibration_error >= 0)
  expect_false(ce$automatic_adequacy_verdict)
  expect_equal(sum(ct$count), 4L)
})

test_that("prediction ranking never selects automatically", {
  draws <- matrix(
    c(
      1, 2, 3,
      1, 3, 2,
      2, 1, 3,
      2, 3, 1
    ),
    nrow = 4,
    byrow = TRUE
  )
  obj <- structure(
    list(
      family = "duration",
      type = "expected",
      draws = draws,
      summary = data.frame(
        observation = 1:3,
        predicted_mean = colMeans(draws),
        predicted_sd = apply(draws, 2, sd),
        lower = c(1, 1, 1),
        predicted_median = c(1.5, 2.5, 2.5),
        upper = c(2, 3, 3)
      ),
      observed = NULL,
      newdata = data.frame(condition = c("a", "b", "c"))
    ),
    class = "gp3bayes_prediction"
  )

  rank <- prediction_rank_probabilities(obj)
  expect_equal(nrow(rank), 3L)
  expect_false(any(rank$automatic_selection))
  expect_equal(sum(rank$probability_rank_1), 1)
})
