test_that("advanced design-support audit never certifies adequacy", {
  sim <- simulate_advanced_pupil_timecourse(
    n_participants = 12,
    trials_per_participant = 3,
    time_points = 13,
    time_range = c(0, 1200),
    seed = 11
  )
  spec <- specify_advanced_pupil_timecourse_model(
    sim$data,
    temporal_structure = "smooth",
    family = "gaussian",
    autocorrelation = "ar1"
  )
  a <- audit_advanced_pupil_identifiability(spec)
  expect_s3_class(a, "gp3bayes_pupil_identifiability_audit")
  expect_false(a$certification)
  expect_true(all(c("domain", "check", "value", "status", "guidance") %in% names(a$table)))
  expect_true(a$overall %in% c("pass", "review", "high"))
})

test_that("predictive score computes bounded coverage and finite core metrics", {
  set.seed(4)
  y <- rnorm(25)
  draws <- matrix(rnorm(400 * 25, rep(y, each = 400), 0.5), nrow = 400)
  sc <- score_pupil_predictions(y, draws, probability = 0.9)
  expect_s3_class(sc, "gp3bayes_pupil_predictive_score")
  expect_true(all(is.finite(sc$table$value[sc$table$metric != "approx_crps"])))
  cov <- sc$table$value[sc$table$metric == "interval_coverage"]
  expect_true(cov >= 0 && cov <= 1)
  expect_true(sc$table$value[sc$table$metric == "approx_crps"] >= 0)
})

testthat::test_that(
  "advanced pupil diagnostics preserve canonical posterior convergence columns", {

    testthat::skip_if_not_installed("posterior")

    testthat::local_mocked_bindings(
        .p05_underlying_fit = function(fit) {
            posterior::example_draws("eight_schools")
        },
        .package = "gp3bayes"
    )

    warnings_seen <- character()

    out <- withCallingHandlers(
        diagnose_advanced_pupil_fit(list()),
        warning = function(w) {
            warnings_seen <<- c(
                warnings_seen,
                conditionMessage(w)
            )
            invokeRestart("muffleWarning")
        }
    )

    testthat::expect_length(
        warnings_seen,
        0L
    )

    testthat::expect_s3_class(
        out,
        "gp3bayes_pupil_advanced_diagnostics"
    )

    testthat::expect_true(
        all(
            c(
                "rhat",
                "ess_bulk",
                "ess_tail"
            ) %in%
                names(out$parameter_summary)
        )
    )

    testthat::expect_true(
        all(
            vapply(
                out$parameter_summary[
                    c("rhat", "ess_bulk", "ess_tail")
                ],
                is.numeric,
                logical(1L)
            )
        )
    )
})
