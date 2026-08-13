test_that("fit-dependent extraction wrappers reject non-fit inputs early", {
  expect_error(
    extract_expected_predictions(NULL),
    "gp3bayes_fit"
  )
  expect_error(
    extract_posterior_predictions(NULL),
    "gp3bayes_fit"
  )
  expect_error(
    extract_linear_predictions(NULL),
    "gp3bayes_fit"
  )
  expect_error(
    extract_log_likelihood(NULL),
    "fit"
  )
  expect_error(
    extract_sampler_diagnostics(NULL),
    "fit"
  )
})

test_that("prediction helpers reject malformed objects and unsupported requests", {
  expect_error(
    prediction_draws_long(list()),
    "gp3bayes_prediction"
  )
  expect_error(
    posterior_predictive_summary_table(
      structure(
        list(
          type = "expected",
          draws = matrix(0.5, nrow = 2, ncol = 2)
        ),
        class = "gp3bayes_prediction"
      )
    ),
    "posterior predictive"
  )
  expect_error(
    binary_group_calibration(
      structure(
        list(
          family = "binary",
          type = "expected",
          draws = matrix(0.5, nrow = 2, ncol = 2),
          summary = data.frame(predicted_mean = c(0.5, 0.5)),
          observed = c(0, 1),
          newdata = data.frame(condition = c("A", "B"))
        ),
        class = "gp3bayes_prediction"
      ),
      "missing_group"
    ),
    "must name one column"
  )
})

test_that("prediction combinatoric guards remain bounded", {
  draws <- matrix(
    rep(seq(0.1, 0.9, length.out = 25), each = 20),
    nrow = 20
  )

  prediction <- structure(
    list(
      family = "binary",
      type = "expected",
      draws = draws,
      summary = data.frame(
        observation = 1:25,
        predicted_mean = colMeans(draws),
        lower = colMeans(draws) - 0.01,
        predicted_median = colMeans(draws),
        upper = colMeans(draws) + 0.01
      ),
      observed = rep(c(0, 1), length.out = 25),
      newdata = data.frame(row = 1:25)
    ),
    class = "gp3bayes_prediction"
  )

  expect_error(
    prediction_pairwise_contrasts(
      prediction,
      rows = 1:25,
      max_rows = 20L
    ),
    "maximum"
  )

  expect_error(
    prediction_rank_probabilities(
      prediction,
      rows = 1:25,
      max_rows = 20L
    ),
    "Too many rows requested for ranking"
  )
})
