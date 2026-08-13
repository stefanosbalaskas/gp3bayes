test_that("binary prediction scores reproduce simple perfect predictions", {
  p <- c(0.05, 0.95, 0.9, 0.1)
  y <- c(0, 1, 1, 0)
  s <- binary_prediction_scores(p, y)
  expect_equal(s$accuracy, 1)
  expect_equal(s$sensitivity, 1)
  expect_equal(s$specificity, 1)
  expect_gt(s$auc, 0.99)
})

test_that("binary threshold metrics retain requested thresholds", {
  p <- c(0.1, 0.2, 0.8, 0.9)
  y <- c(0, 0, 1, 1)
  th <- c(0.25, 0.5, 0.75)
  out <- binary_threshold_metrics(p, y, th)
  expect_equal(out$threshold, th)
  expect_false(any(out$automatic_decision))
})

test_that("duration scores are finite for positive data", {
  pred <- c(100, 120, 140)
  obs <- c(105, 118, 150)
  out <- duration_prediction_scores(pred, obs)
  expect_true(all(is.finite(unlist(
    out[1, setdiff(names(out), "automatic_decision")],
    use.names = FALSE
  ))))
  expect_false(out$automatic_decision)
})

test_that("prediction object accessors are stable", {
  draws <- matrix(c(
    0.1, 0.2,
    0.2, 0.3,
    0.3, 0.4
  ), nrow = 3, byrow = TRUE)
  obj <- structure(
    list(
      family = "binary",
      type = "expected",
      draws = draws,
      summary = data.frame(
        observation = 1:2,
        predicted_mean = colMeans(draws),
        predicted_sd = apply(draws, 2, sd),
        lower = c(0.1, 0.2),
        predicted_median = c(0.2, 0.3),
        upper = c(0.3, 0.4),
        observed = c(0, 1)
      ),
      observed = c(0, 1),
      include_group_effects = FALSE
    ),
    class = "gp3bayes_prediction"
  )
  expect_equal(prediction_table(obj), obj$summary)
  contrast <- prediction_contrast(obj, 1, 2)
  expect_equal(contrast$measure, "difference")
  exc <- prediction_exceedance_probability(obj, 0.25)
  expect_equal(nrow(exc), 2L)
})
