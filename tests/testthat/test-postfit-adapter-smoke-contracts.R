make_binary_prediction_fixture <- function(type = "expected") {
  draws <- matrix(
    c(
      0.10, 0.20, 0.80, 0.90,
      0.15, 0.25, 0.75, 0.85,
      0.20, 0.30, 0.70, 0.80,
      0.12, 0.22, 0.78, 0.88
    ),
    nrow = 4,
    byrow = TRUE
  )

  if (identical(type, "predictive")) {
    draws <- matrix(
      c(
        0, 0, 1, 1,
        0, 1, 1, 1,
        0, 0, 1, 1,
        0, 0, 0, 1
      ),
      nrow = 4,
      byrow = TRUE
    )
  }

  observed <- c(0, 0, 1, 1)

  structure(
    list(
      family = "binary",
      type = type,
      scale = "response",
      draws = draws,
      summary = data.frame(
        observation = 1:4,
        predicted_mean = colMeans(draws),
        predicted_sd = apply(draws, 2L, stats::sd),
        lower = apply(draws, 2L, min),
        predicted_median = apply(draws, 2L, stats::median),
        upper = apply(draws, 2L, max),
        observed = observed
      ),
      observed = observed,
      newdata = data.frame(
        condition = c("A", "A", "B", "B"),
        block = c("first", "second", "first", "second"),
        stringsAsFactors = FALSE
      ),
      include_group_effects = FALSE,
      automatic_decision = FALSE
    ),
    class = "gp3bayes_prediction"
  )
}

test_that("previously under-supported prediction adapters are directly exercised", {
  expected <- make_binary_prediction_fixture("expected")
  predictive <- make_binary_prediction_fixture("predictive")

  grouped <- binary_group_calibration(expected, "condition")
  expect_equal(nrow(grouped), 2L)
  expect_true(all(c(
    "predicted_probability",
    "observed_rate",
    "calibration_gap"
  ) %in% names(grouped)))

  long <- prediction_draws_long(expected, max_draws = 3L, seed = 11L)
  expect_equal(length(unique(long$draw)), 3L)

  width <- prediction_interval_width(expected)
  expect_equal(nrow(width), 4L)

  ranks <- prediction_rank_probabilities(
    expected,
    rows = 1:4,
    direction = "higher"
  )
  expect_equal(nrow(ranks), 4L)
  expect_false(any(ranks$automatic_selection))

  pairwise <- prediction_pairwise_contrasts(
    expected,
    rows = 1:4,
    measure = "difference"
  )
  expect_equal(nrow(pairwise), choose(4L, 2L))

  grouped_prediction <- group_prediction_summary(
    expected,
    by = "condition"
  )
  expect_equal(nrow(grouped_prediction), 2L)

  predictive_summary <- posterior_predictive_summary_table(predictive)
  expect_equal(nrow(predictive_summary), 4L)
})

test_that("backend environment and LOO atlas table adapters are directly exercised", {
  env <- structure(
    list(
      checks = data.frame(
        check = c("brms_package", "backend_package"),
        status = c("pass", "pass"),
        detail = c("available", "available"),
        stringsAsFactors = FALSE
      )
    ),
    class = "gp3bayes_backend_environment"
  )

  expect_equal(
    backend_environment_table(env),
    env$checks
  )

  atlas <- structure(
    list(
      table = data.frame(
        observation = 1:3,
        pareto_k = c(0.1, 0.5, 0.8),
        elpd_loo = c(-1, -1.5, -2),
        stringsAsFactors = FALSE
      ),
      flagged = data.frame(),
      automatic_exclusion = FALSE
    ),
    class = "gp3bayes_loo_influence_atlas"
  )

  expect_equal(
    loo_influence_atlas_table(atlas),
    atlas$table
  )
})

test_that("prior-posterior long-form adapter retains both distributions", {
  bridge <- structure(
    list(
      prior_draws = cbind(
        alpha = seq(-1, 1, length.out = 80),
        beta = seq(0, 2, length.out = 80)
      ),
      posterior_draws = cbind(
        alpha = seq(-0.5, 0.5, length.out = 80),
        beta = seq(0.5, 1.5, length.out = 80)
      )
    ),
    class = "gp3bayes_prior_posterior_bridge"
  )

  out <- prior_posterior_draws_long(
    bridge,
    max_draws = 50L,
    seed = 7L
  )

  expect_setequal(
    unique(out$distribution),
    c("prior", "posterior")
  )
  expect_setequal(
    unique(out$variable),
    c("alpha", "beta")
  )
})

test_that("Phase-4 table adapters are directly exercised", {
  profile <- structure(
    list(
      table = data.frame(
        profile_x = 1:3,
        predicted_median = c(0.2, 0.4, 0.6),
        lower = c(0.1, 0.3, 0.5),
        upper = c(0.3, 0.5, 0.7)
      )
    ),
    class = "gp3bayes_prediction_profile"
  )

  surface <- structure(
    list(
      table = data.frame(
        surface_x = rep(1:2, each = 2),
        surface_y = rep(1:2, times = 2),
        predicted_median = c(0.2, 0.3, 0.5, 0.6),
        interval_width = c(0.2, 0.2, 0.3, 0.3)
      )
    ),
    class = "gp3bayes_prediction_surface"
  )

  expect_equal(
    prediction_profile_table(profile),
    profile$table
  )
  expect_equal(
    prediction_surface_table(surface),
    surface$table
  )
})
