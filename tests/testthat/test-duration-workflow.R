make_duration_simulation <- function(
  seed = 2026,
  random_slope = TRUE,
  include_items = TRUE
) {
  simulate_hierarchical_duration_data(
    n_participants = 12,
    trials_per_participant = 8,
    n_items = 6,
    random_slope_sd =
      if (random_slope) {
        0.20
      } else {
        0
      },
    include_items =
      include_items,
    seed = seed
  )
}

make_duration_contract <- function(
  random_slope = TRUE,
  include_item = TRUE
) {
  create_model_contract(
    family = "duration",
    outcome_col = "duration",
    participant_col =
      "participant_id",
    item_col =
      if (include_item) {
        "item_id"
      } else {
        NULL
      },
    trial_col = "trial_id",
    condition_col = "condition",
    predictors = c(
      "participant_covariate",
      "trial_covariate"
    ),
    interaction = c(
      "condition",
      "participant_covariate"
    ),
    random_slope = random_slope,
    outcome_unit = "milliseconds"
  )
}

make_duration_prepared <- function(
  seed = 2026,
  random_slope = TRUE,
  include_items = TRUE
) {
  simulation <- make_duration_simulation(
    seed = seed,
    random_slope = random_slope,
    include_items = include_items
  )
  contract <- make_duration_contract(
    random_slope = random_slope,
    include_item = include_items
  )

  prepare_hierarchical_duration_data(
    simulation$data,
    contract,
    condition_levels = c(
      "control",
      "treatment"
    )
  )
}

test_that(
  "duration simulation is deterministic and records truth",
  {
    first <- make_duration_simulation(
      seed = 2026
    )
    second <- make_duration_simulation(
      seed = 2026
    )

    expect_s3_class(
      first,
      "gp3bayes_duration_simulation"
    )
    expect_identical(
      first$data,
      second$data
    )
    expect_identical(
      first$truth,
      second$truth
    )
    expect_identical(
      first$random_effects,
      second$random_effects
    )
    expect_equal(
      nrow(first$data),
      96L
    )
    expect_true(
      all(
        is.finite(
          first$data$duration
        )
      )
    )
    expect_true(
      all(
        first$data$duration > 0
      )
    )
    expect_identical(
      first$truth$outcome_unit,
      "milliseconds"
    )
    expect_false(
      first$design$censored
    )
  }
)

test_that(
  "duration simulation supports a design without items or slopes",
  {
    simulation <- make_duration_simulation(
      random_slope = FALSE,
      include_items = FALSE
    )

    expect_false(
      "item_id" %in%
        names(simulation$data)
    )
    expect_null(
      simulation$random_effects$item
    )
    expect_false(
      simulation$design$random_slope
    )
    expect_identical(
      simulation$design$n_items,
      0L
    )
  }
)

test_that(
  "duration simulation validates strictly positive scales",
  {
    expect_error(
      simulate_hierarchical_duration_data(
        baseline_median = 0
      ),
      "baseline_median"
    )
    expect_error(
      simulate_hierarchical_duration_data(
        residual_sd = 0
      ),
      "residual_sd"
    )
    expect_error(
      simulate_hierarchical_duration_data(
        participant_sd = -1
      ),
      "participant_sd"
    )
    expect_error(
      simulate_hierarchical_duration_data(
        random_slope_cor = 1
      ),
      "random_slope_cor"
    )
    expect_error(
      simulate_hierarchical_duration_data(
        outcome_unit = ""
      ),
      "outcome_unit"
    )
  }
)

test_that(
  "duration preparation records explicit unit conversion",
  {
    simulation <- make_duration_simulation()
    contract <- make_duration_contract()

    prepared <-
      prepare_hierarchical_duration_data(
        simulation$data,
        contract,
        condition_levels = c(
          "control",
          "treatment"
        ),
        outcome_multiplier = 0.001,
        converted_unit = "seconds"
      )

    expect_s3_class(
      prepared,
      "gp3bayes_duration_prepared"
    )
    expect_identical(
      prepared$outcome_unit,
      "seconds"
    )
    expect_equal(
      prepared$data$duration,
      simulation$data$duration *
        0.001
    )
    expect_identical(
      prepared$transformations$outcome$
        source_unit,
      "milliseconds"
    )
    expect_identical(
      prepared$transformations$outcome$
        analysis_unit,
      "seconds"
    )
    expect_identical(
      prepared$transformations$outcome$
        multiplier,
      0.001
    )
  }
)

test_that(
  "duration preparation rejects zero negative and nonfinite outcomes",
  {
    simulation <- make_duration_simulation()
    contract <- make_duration_contract()

    zero <- simulation$data
    zero$duration[[1L]] <- 0

    expect_error(
      prepare_hierarchical_duration_data(
        zero,
        contract,
        condition_levels = c(
          "control",
          "treatment"
        )
      ),
      "strictly positive"
    )

    negative <- simulation$data
    negative$duration[[1L]] <- -1

    expect_error(
      prepare_hierarchical_duration_data(
        negative,
        contract,
        condition_levels = c(
          "control",
          "treatment"
        )
      ),
      "strictly positive"
    )

    infinite <- simulation$data
    infinite$duration[[1L]] <- Inf

    expect_error(
      prepare_hierarchical_duration_data(
        infinite,
        contract,
        condition_levels = c(
          "control",
          "treatment"
        )
      ),
      "finite numeric"
    )
  }
)

test_that(
  "duration preparation requires explicit missing-value decisions",
  {
    simulation <- make_duration_simulation()
    simulation$data$duration[[1L]] <-
      NA_real_
    contract <- make_duration_contract()

    expect_error(
      prepare_hierarchical_duration_data(
        simulation$data,
        contract,
        condition_levels = c(
          "control",
          "treatment"
        ),
        missing = "error"
      ),
      "Missing values"
    )

    prepared <-
      prepare_hierarchical_duration_data(
        simulation$data,
        contract,
        condition_levels = c(
          "control",
          "treatment"
        ),
        missing = "drop"
      )

    expect_identical(
      prepared$n_input_rows,
      96L
    )
    expect_identical(
      prepared$n_analysis_rows,
      95L
    )
    expect_identical(
      prepared$rows_removed,
      1L
    )
    expect_identical(
      prepared$transformations$missing$
        dropped_row_positions,
      1L
    )
  }
)

test_that(
  "duration preparation records predictor scaling",
  {
    prepared <- prepare_hierarchical_duration_data(
      make_duration_simulation()$data,
      make_duration_contract(),
      condition_levels = c(
        "control",
        "treatment"
      ),
      scale_predictors = c(
        "participant_covariate",
        "trial_covariate"
      )
    )

    expect_equal(
      mean(
        prepared$data$
          participant_covariate
      ),
      0,
      tolerance = 1e-12
    )
    expect_equal(
      stats::sd(
        prepared$data$
          trial_covariate
      ),
      1,
      tolerance = 1e-12
    )
    expect_true(
      all(
        c(
          "participant_covariate",
          "trial_covariate"
        ) %in%
          names(
            prepared$transformations$
              scaled_columns
          )
      )
    )
  }
)

test_that(
  "duration specification uses existing approved duration priors",
  {
    prepared <- make_duration_prepared()

    specification <- specify_duration_model(
      prepared,
      baseline = 500,
      intercept_scale = 1.2,
      coefficient_scale = 0.4,
      group_sd_scale = 0.8,
      residual_scale = 0.6,
      correlation_eta = 3,
      student_df = 4
    )

    expect_s3_class(
      specification,
      "gp3bayes_duration_model_specification"
    )
    expect_s3_class(
      specification,
      "gp3bayes_model_specification"
    )
    expect_identical(
      specification$family,
      "duration"
    )
    expect_identical(
      specification$outcome_unit,
      "milliseconds"
    )
    expect_identical(
      specification$priors$table$
        parameter_class,
      c(
        "Intercept",
        "b",
        "sd",
        "sigma",
        "cor"
      )
    )
    expect_false(
      specification$fit_performed
    )
    expect_false(
      specification$unrestricted_formula
    )
  }
)

test_that(
  "duration prior predictive checks are deterministic and backend independent",
  {
    specification <- specify_duration_model(
      make_duration_prepared(),
      baseline = 500
    )

    first <- check_duration_prior_predictive(
      specification,
      draws = 60,
      seed = 2027
    )
    second <- check_duration_prior_predictive(
      specification,
      draws = 60,
      seed = 2027
    )

    expect_s3_class(
      first,
      "gp3bayes_duration_prior_predictive_check"
    )
    expect_identical(
      first$summaries,
      second$summaries
    )
    expect_identical(
      first$checks,
      second$checks
    )
    expect_identical(
      first$backend,
      "none"
    )
    expect_false(
      first$fitting_performed
    )
    expect_false(
      first$posterior_adequacy_established
    )
    expect_true(
      all(
        first$checks$status %in%
          c(
            "pass",
            "fail",
            "not_applicable"
          )
      )
    )
  }
)

test_that(
  "duration translation uses fixed lognormal brms contract",
  {
    skip_if_not_installed("brms")

    specification <- specify_duration_model(
      make_duration_prepared(),
      baseline = 500
    )

    translated <-
      translate_duration_model_to_brms(
        specification
      )

    expect_s3_class(
      translated,
      "gp3bayes_duration_backend_specification"
    )
    expect_identical(
      translated$family,
      "duration"
    )
    expect_identical(
      translated$backend_interface,
      "brms"
    )
    expect_identical(
      translated$sampling_backend,
      "rstan"
    )
    expect_identical(
      translated$algorithm,
      "sampling"
    )
    expect_false(
      translated$unrestricted_formula
    )
    expect_false(
      translated$fit_performed
    )
    expect_true(
      all(
        c(
          "Intercept",
          "b",
          "sd",
          "sigma"
        ) %in%
          translated$parameter_table$class[
            translated$parameter_table$source ==
              "user"
          ]
      )
    )
  }
)

test_that(
  "duration fitting exposes no unrestricted backend controls",
  {
    arguments <- names(
      formals(
        fit_duration_model
      )
    )

    expect_identical(
      arguments,
      c(
        "specification",
        "chains",
        "iter",
        "warmup",
        "cores",
        "seed",
        "adapt_delta",
        "max_treedepth",
        "refresh"
      )
    )

    expect_false(
      any(
        c(
          "...",
          "formula",
          "family",
          "backend",
          "algorithm",
          "prior",
          "stanvars"
        ) %in% arguments
      )
    )
  }
)

test_that(
  "duration fitting controls fail before sampling when invalid",
  {
    specification <- specify_duration_model(
      make_duration_prepared(),
      baseline = 500
    )

    expect_error(
      fit_duration_model(
        specification,
        chains = 0
      ),
      "`chains` must lie"
    )
    expect_error(
      fit_duration_model(
        specification,
        iter = 500,
        warmup = 500
      ),
      "`warmup` must be smaller"
    )
    expect_error(
      fit_duration_model(
        specification,
        chains = 2,
        cores = 3
      ),
      "`cores` cannot exceed"
    )
    expect_error(
      fit_duration_model(
        specification,
        adapt_delta = 1
      ),
      "`adapt_delta` must lie"
    )
  }
)

test_that(
  "duration print methods state conservative boundaries",
  {
    simulation <- make_duration_simulation()
    prepared <- make_duration_prepared()
    specification <- specify_duration_model(
      prepared,
      baseline = 500
    )
    prior_check <- check_duration_prior_predictive(
      specification,
      draws = 50,
      seed = 20
    )

    expect_output(
      print(simulation),
      "Censored: FALSE",
      fixed = TRUE
    )
    expect_output(
      print(prepared),
      "Fit performed: FALSE",
      fixed = TRUE
    )
    expect_output(
      print(specification),
      "Family: lognormal",
      fixed = TRUE
    )
    expect_output(
      print(prior_check),
      "Posterior adequacy established: FALSE",
      fixed = TRUE
    )
  }
)
