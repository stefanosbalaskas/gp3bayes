test_that(
  "diagnostic status helpers are conservative",
  {
    expect_identical(
      .gp3b_classify_upper(
        1.00,
        pass = 1.01,
        review = 1.05
      ),
      "pass"
    )
    expect_identical(
      .gp3b_classify_upper(
        1.03,
        pass = 1.01,
        review = 1.05
      ),
      "review"
    )
    expect_identical(
      .gp3b_classify_upper(
        1.10,
        pass = 1.01,
        review = 1.05
      ),
      "fail"
    )
    expect_identical(
      .gp3b_classify_lower(
        120,
        pass = 100,
        review = 50
      ),
      "pass"
    )
    expect_identical(
      .gp3b_classify_lower(
        75,
        pass = 100,
        review = 50
      ),
      "review"
    )
    expect_identical(
      .gp3b_classify_lower(
        25,
        pass = 100,
        review = 50
      ),
      "fail"
    )
    expect_identical(
      .gp3b_classify_lower(
        NA_real_,
        pass = 100,
        review = 50
      ),
      "not_assessed"
    )
  }
)

test_that(
  "overall status preserves review and fail states",
  {
    expect_identical(
      .gp3b_worst_status(
        c(
          "pass",
          "pass"
        )
      ),
      "pass"
    )
    expect_identical(
      .gp3b_worst_status(
        c(
          "pass",
          "not_assessed"
        )
      ),
      "review"
    )
    expect_identical(
      .gp3b_worst_status(
        c(
          "pass",
          "review"
        )
      ),
      "review"
    )
    expect_identical(
      .gp3b_worst_status(
        c(
          "pass",
          "fail"
        )
      ),
      "fail"
    )
  }
)

test_that(
  "posterior predictive status distinguishes pass review and fail",
  {
    expect_identical(
      .gp3b_predictive_status(
        observed = 0.5,
        pass_lower = 0.4,
        pass_upper = 0.6,
        review_lower = 0.3,
        review_upper = 0.7
      ),
      "pass"
    )
    expect_identical(
      .gp3b_predictive_status(
        observed = 0.65,
        pass_lower = 0.4,
        pass_upper = 0.6,
        review_lower = 0.3,
        review_upper = 0.7
      ),
      "review"
    )
    expect_identical(
      .gp3b_predictive_status(
        observed = 0.8,
        pass_lower = 0.4,
        pass_upper = 0.6,
        review_lower = 0.3,
        review_upper = 0.7
      ),
      "fail"
    )
    expect_identical(
      .gp3b_predictive_status(
        observed = NA_real_,
        pass_lower = 0.4,
        pass_upper = 0.6,
        review_lower = 0.3,
        review_upper = 0.7
      ),
      "not_applicable"
    )
  }
)

test_that(
  "binary observed summaries preserve declared structure",
  {
    y <- c(
      0L,
      1L,
      0L,
      1L,
      1L,
      1L,
      0L,
      0L
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

    result <- .gp3b_binary_observed_summary(
      y,
      condition,
      participant,
      item
    )

    expect_identical(
      names(result),
      c(
        "overall_rate",
        "condition_low_rate",
        "condition_high_rate",
        "condition_rate_contrast",
        "participant_rate_sd",
        "item_rate_sd"
      )
    )
    expect_equal(
      result[["overall_rate"]],
      mean(y)
    )
    expect_true(
      is.finite(
        result[["participant_rate_sd"]]
      )
    )
  }
)

test_that(
  "recovery status cannot pass with too few repetitions",
  {
    expect_identical(
      .gp3b_recovery_component_status(
        repetitions = 5,
        standardized_bias = 0.1,
        coverage = 0.9,
        diagnostic_pass_fraction = 1,
        minimum_repetitions = 20,
        maximum_standardized_bias = 0.25,
        minimum_coverage = 0.8,
        minimum_diagnostic_pass_fraction = 0.8
      ),
      "review"
    )

    expect_identical(
      .gp3b_recovery_component_status(
        repetitions = 20,
        standardized_bias = 0.1,
        coverage = 0.9,
        diagnostic_pass_fraction = 1,
        minimum_repetitions = 20,
        maximum_standardized_bias = 0.25,
        minimum_coverage = 0.8,
        minimum_diagnostic_pass_fraction = 0.8
      ),
      "pass"
    )

    expect_identical(
      .gp3b_recovery_component_status(
        repetitions = 20,
        standardized_bias = 0.8,
        coverage = 0.5,
        diagnostic_pass_fraction = 0.4,
        minimum_repetitions = 20,
        maximum_standardized_bias = 0.25,
        minimum_coverage = 0.8,
        minimum_diagnostic_pass_fraction = 0.8
      ),
      "fail"
    )
  }
)

test_that(
  "binary diagnostic and validation APIs reject non-fit objects",
  {
    expect_error(
      diagnose_binary_fit(
        list()
      ),
      "gp3bayes_fit"
    )
    expect_error(
      summarise_binary_posterior(
        list()
      ),
      "gp3bayes_fit"
    )
    expect_error(
      check_binary_posterior_predictive(
        list()
      ),
      "gp3bayes_fit"
    )
    expect_error(
      assess_binary_prior_sensitivity(
        list()
      ),
      "gp3bayes_fit"
    )
    expect_error(
      create_binary_model_report(
        list()
      ),
      "gp3bayes_fit"
    )
  }
)

test_that(
  "binary posterior APIs expose no unrestricted backend arguments",
  {
    restricted <- list(
      diagnose_binary_fit =
        names(
          formals(
            diagnose_binary_fit
          )
        ),
      summarise_binary_posterior =
        names(
          formals(
            summarise_binary_posterior
          )
        ),
      check_binary_posterior_predictive =
        names(
          formals(
            check_binary_posterior_predictive
          )
        ),
      assess_binary_prior_sensitivity =
        names(
          formals(
            assess_binary_prior_sensitivity
          )
        ),
      run_binary_recovery =
        names(
          formals(
            run_binary_recovery
          )
        )
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

    expect_false(
      any(
        vapply(
          restricted,
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
  "Markdown tables escape pipes and preserve missing values",
  {
    table <- data.frame(
      label = c(
        "a|b",
        NA_character_
      ),
      value = c(
        1.2345,
        NA_real_
      ),
      stringsAsFactors = FALSE
    )

    output <- .gp3b_markdown_table(
      table
    )

    expect_true(
      any(
        grepl(
          "a\\\\|b",
          output,
          fixed = TRUE
        )
      )
    )
    expect_true(
      any(
        grepl(
          "NA",
          output,
          fixed = TRUE
        )
      )
    )
  }
)

test_that(
  "binary report refuses unsafe output replacement",
  {
    temporary <- tempfile(
      fileext = ".md"
    )
    writeLines(
      "existing",
      temporary
    )

    expect_error(
      create_binary_model_report(
        list(),
        file = temporary
      ),
      "gp3bayes_fit"
    )

    unlink(temporary)
  }
)
