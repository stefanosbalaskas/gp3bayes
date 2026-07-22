make_binary_fitting_specification <- function(
  random_slope = TRUE
) {
  simulation <- simulate_hierarchical_binary_data(
    n_participants = 12,
    trials_per_participant = 8,
    n_items = 6,
    random_slope_sd = if (random_slope) 0.3 else 0,
    seed = 2026
  )

  contract <- create_model_contract(
    family = "binary",
    outcome_col = "selected",
    participant_col = "participant_id",
    item_col = "item_id",
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
    random_slope = random_slope
  )

  prepared <- prepare_hierarchical_binary_data(
    simulation$data,
    contract,
    condition_levels = c(
      "control",
      "treatment"
    ),
    scale_predictors = c(
      "participant_covariate",
      "trial_covariate"
    )
  )

  specify_binary_model(
    prepared,
    baseline = 0.35
  )
}

test_that(
  "binary specifications translate through the restricted brms interface",
  {
    skip_if_not_installed("brms")

    specification <- make_binary_fitting_specification(
      random_slope = TRUE
    )

    translated <- translate_binary_model_to_brms(
      specification
    )

    expect_s3_class(
      translated,
      "gp3bayes_binary_backend_specification"
    )

    expect_identical(
      translated$family,
      "binary"
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
      translated$compiled
    )

    expect_false(
      translated$fit_performed
    )

    expect_false(
      translated$diagnostics_assessed
    )

    expect_identical(
      translated$formula_text,
      specification$formula_text
    )

    expect_true(
      all(
        c(
          "Intercept",
          "b",
          "sd"
        ) %in%
          translated$parameter_table$class[
            translated$parameter_table$source ==
              "user"
          ]
      )
    )

    expect_true(
      any(
        translated$parameter_table$class == "L" &
          translated$parameter_table$source == "user"
      )
    )

    expect_identical(
      translated$prior_text[["cor"]],
      "lkj(2)"
    )
  }
)

test_that(
  "binary translation produces the approved Stan design without fitting",
  {
    skip_if_not_installed("brms")

    specification <- make_binary_fitting_specification(
      random_slope = TRUE
    )

    translated <- translate_binary_model_to_brms(
      specification
    )

    stan_data <- brms::make_standata(
      formula = translated$formula,
      data = specification$prepared$data,
      family = translated$family_object,
      prior = translated$priors
    )

    fixed_matrix <- stats::model.matrix(
      specification$fixed_formula,
      data = specification$prepared$data
    )

    expect_identical(
      as.integer(stan_data$N),
      nrow(specification$prepared$data)
    )

    expect_identical(
      as.integer(stan_data$K),
      ncol(fixed_matrix)
    )

    expect_identical(
      as.integer(stan_data$Kc),
      ncol(fixed_matrix) - 1L
    )

    expect_identical(
      unname(dim(stan_data$X)),
      c(
        nrow(specification$prepared$data),
        ncol(fixed_matrix)
      )
    )
  }
)

test_that(
  "binary translation omits correlation priors without a random slope",
  {
    skip_if_not_installed("brms")

    specification <- make_binary_fitting_specification(
      random_slope = FALSE
    )

    translated <- translate_binary_model_to_brms(
      specification
    )

    expect_false(
      "cor" %in% names(
        translated$prior_text
      )
    )

    expect_false(
      any(
        translated$parameter_table$class == "L" &
          translated$parameter_table$source == "user"
      )
    )
  }
)

test_that(
  "binary fitting exposes only approved controls",
  {
    fitting_arguments <- names(
      formals(
        fit_binary_model
      )
    )

    expect_identical(
      fitting_arguments,
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
      "..." %in% fitting_arguments
    )

    expect_false(
      any(
        c(
          "formula",
          "family",
          "backend",
          "algorithm",
          "prior",
          "stanvars"
        ) %in% fitting_arguments
      )
    )
  }
)

test_that(
  "binary fitting arguments fail before sampling when invalid",
  {
    specification <- make_binary_fitting_specification()

    expect_error(
      fit_binary_model(
        specification,
        chains = 0
      ),
      "`chains` must lie"
    )

    expect_error(
      fit_binary_model(
        specification,
        iter = 500,
        warmup = 500
      ),
      "`warmup` must be smaller"
    )

    expect_error(
      fit_binary_model(
        specification,
        chains = 2,
        cores = 3
      ),
      "`cores` cannot exceed"
    )

    expect_error(
      fit_binary_model(
        specification,
        adapt_delta = 1
      ),
      "`adapt_delta` must lie"
    )

    expect_error(
      fit_binary_model(
        specification,
        max_treedepth = 4
      ),
      "`max_treedepth` must lie"
    )
  }
)

test_that(
  "binary backend specification prints conservative status",
  {
    skip_if_not_installed("brms")

    specification <- make_binary_fitting_specification()

    translated <- translate_binary_model_to_brms(
      specification
    )

    output <- capture.output(
      print(translated)
    )

    expect_true(
      any(
        grepl(
          "Family: Bernoulli-logit",
          output,
          fixed = TRUE
        )
      )
    )

    expect_true(
      any(
        grepl(
          "Algorithm: sampling",
          output,
          fixed = TRUE
        )
      )
    )

    expect_true(
      any(
        grepl(
          "Fit performed: FALSE",
          output,
          fixed = TRUE
        )
      )
    )
  }
)
