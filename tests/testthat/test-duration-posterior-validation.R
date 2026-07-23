test_that(
  "duration posterior APIs reject non-fit objects",
  {
    expect_error(
      diagnose_duration_fit(
        list()
      ),
      "gp3bayes_fit"
    )
    expect_error(
      summarise_duration_posterior(
        list()
      ),
      "gp3bayes_fit"
    )
    expect_error(
      check_duration_posterior_predictive(
        list()
      ),
      "gp3bayes_fit"
    )
    expect_error(
      assess_duration_prior_sensitivity(
        list()
      ),
      "gp3bayes_fit"
    )
    expect_error(
      create_duration_model_report(
        list()
      ),
      "gp3bayes_fit"
    )
  }
)

test_that(
  "duration posterior APIs expose only restricted controls",
  {
    functions <- list(
      diagnose_duration_fit =
        diagnose_duration_fit,
      summarise_duration_posterior =
        summarise_duration_posterior,
      check_duration_posterior_predictive =
        check_duration_posterior_predictive,
      assess_duration_prior_sensitivity =
        assess_duration_prior_sensitivity,
      run_duration_recovery =
        run_duration_recovery
    )

    forbidden <- c(
      "...",
      "formula",
      "family",
      "backend",
      "algorithm",
      "prior",
      "stanvars"
    )

    observed <- lapply(
      functions,
      function(fun) {
        names(
          formals(fun)
        )
      }
    )

    expect_false(
      any(
        vapply(
          observed,
          function(arguments) {
            any(
              forbidden %in%
                arguments
            )
          },
          logical(1)
        )
      )
    )
  }
)

test_that(
  "duration truth mapping uses supported brms parameter names",
  {
    simulation <-
      simulate_hierarchical_duration_data(
        n_participants = 8,
        trials_per_participant = 6,
        n_items = 4,
        seed = 10
      )

    variables <- c(
      "b_Intercept",
      "b_condition",
      "sd_participant_id__Intercept",
      "sigma"
    )

    truth <- .gp3d_duration_truth_table(
      simulation,
      variables
    )

    expect_identical(
      truth$variable,
      variables
    )
    expect_equal(
      truth$truth[
        truth$variable ==
          "b_Intercept"
      ],
      log(
        simulation$truth$
          baseline_median
      )
    )
    expect_equal(
      truth$truth[
        truth$variable ==
          "sigma"
      ],
      simulation$truth$residual_sd
    )
  }
)

test_that(
  "duration posterior predictive status uses common conservative logic",
  {
    expect_identical(
      .gp3b_predictive_status(
        observed = 500,
        pass_lower = 450,
        pass_upper = 550,
        review_lower = 400,
        review_upper = 600
      ),
      "pass"
    )
    expect_identical(
      .gp3b_predictive_status(
        observed = 580,
        pass_lower = 450,
        pass_upper = 550,
        review_lower = 400,
        review_upper = 600
      ),
      "review"
    )
    expect_identical(
      .gp3b_predictive_status(
        observed = 700,
        pass_lower = 450,
        pass_upper = 550,
        review_lower = 400,
        review_upper = 600
      ),
      "fail"
    )
  }
)

test_that(
  "duration summary helper reports positive-scale features",
  {
    y <- c(
      100,
      200,
      300,
      400,
      500,
      600,
      700,
      800
    )
    condition <- rep(
      c(
        -0.5,
        0.5
      ),
      4L
    )
    participant <- factor(
      rep(
        c(
          "p1",
          "p2"
        ),
        each = 4L
      )
    )
    item <- factor(
      rep(
        c(
          "i1",
          "i2"
        ),
        times = 4L
      )
    )

    summary <- .gp3d_duration_summary(
      y,
      condition,
      participant,
      item
    )

    expect_true(
      all(
        c(
          "median",
          "mean",
          "q90",
          "q99",
          "coefficient_of_variation",
          "condition_median_ratio",
          "participant_log_median_sd",
          "item_log_median_sd",
          "nonfinite_fraction"
        ) %in% names(summary)
      )
    )
    expect_gt(
      summary[["median"]],
      0
    )
    expect_gt(
      summary[["condition_median_ratio"]],
      0
    )
  }
)

test_that(
  "duration recovery status cannot imply validation from a small run",
  {
    status <-
      .gp3b_recovery_component_status(
        repetitions = 4,
        standardized_bias = 0.01,
        coverage = 1,
        diagnostic_pass_fraction = 1,
        minimum_repetitions = 20,
        maximum_standardized_bias = 0.25,
        minimum_coverage = 0.8,
        minimum_diagnostic_pass_fraction = 0.8
      )

    expect_identical(
      status,
      "review"
    )
  }
)
