.gp3d_duration_truth_table <- function(
  simulation,
  posterior_variables
) {
  truth <- simulation$truth

  candidates <- c(
    b_Intercept =
      unname(
        truth$fixed_effects[["(Intercept)"]]
      ),
    b_condition =
      unname(
        truth$fixed_effects[["condition"]]
      ),
    b_participant_covariate =
      unname(
        truth$fixed_effects[["participant_covariate"]]
      ),
    b_trial_covariate =
      unname(
        truth$fixed_effects[["trial_covariate"]]
      ),
    `b_condition:participant_covariate` =
      unname(
        truth$fixed_effects[["condition:participant_covariate"]]
      ),
    sd_participant_id__Intercept =
      truth$participant_sd,
    sd_item_id__Intercept =
      truth$item_sd,
    sd_participant_id__condition =
      truth$random_slope_sd,
    cor_participant_id__Intercept__condition =
      truth$random_slope_cor,
    sigma = truth$residual_sd
  )

  keep <- names(candidates) %in%
    posterior_variables

  data.frame(
    variable = names(candidates)[keep],
    truth = as.numeric(
      candidates[keep]
    ),
    stringsAsFactors = FALSE
  )
}

.gp3d_duration_sensitivity_specification <- function(
  fit,
  multiplier
) {
  original <- fit$specification
  priors <- original$priors

  intercept <- .gp3b_prior_row(
    priors,
    "Intercept"
  )
  coefficient <- .gp3b_prior_row(
    priors,
    "b"
  )
  group_sd <- .gp3b_prior_row(
    priors,
    "sd"
  )
  residual <- .gp3b_prior_row(
    priors,
    "sigma"
  )

  correlation_eta <- 2

  if (isTRUE(
    original$contract$random_slope
  )) {
    correlation_eta <- .gp3b_prior_row(
      priors,
      "cor"
    )$shape[[1L]]
  }

  specify_duration_model(
    original$prepared,
    baseline = priors$baseline,
    intercept_scale =
      intercept$scale[[1L]] *
      multiplier,
    coefficient_scale =
      coefficient$scale[[1L]] *
      multiplier,
    group_sd_scale =
      group_sd$scale[[1L]] *
      multiplier,
    residual_scale =
      residual$scale[[1L]] *
      multiplier,
    correlation_eta =
      correlation_eta,
    student_df =
      group_sd$df[[1L]]
  )
}

#' Diagnose a Fitted Duration Model
#'
#' Applies the package sampling-diagnostic contract to an approved hierarchical
#' lognormal duration fit.
#'
#' @inheritParams diagnose_binary_fit
#' @param fit A `gp3bayes_duration_fit`.
#'
#' @return A `gp3bayes_duration_diagnostics` object.
#'
#' @details
#' The returned status reports prespecified numerical sampling thresholds. It
#' does not automatically establish convergence or posterior adequacy.
#'
#' @export
diagnose_duration_fit <- function(
  fit,
  rhat_pass = 1.01,
  rhat_fail = 1.05,
  ess_per_chain_pass = 100,
  ess_per_chain_fail = 50,
  maximum_treedepth_fraction = 0.01,
  ebfmi_pass = 0.30,
  ebfmi_fail = 0.20
) {
  .gp3b_diagnose_fit(
    fit = fit,
    required_class =
      "gp3bayes_duration_fit",
    rhat_pass = rhat_pass,
    rhat_fail = rhat_fail,
    ess_per_chain_pass =
      ess_per_chain_pass,
    ess_per_chain_fail =
      ess_per_chain_fail,
    maximum_treedepth_fraction =
      maximum_treedepth_fraction,
    ebfmi_pass = ebfmi_pass,
    ebfmi_fail = ebfmi_fail
  )
}

#' Summarise a Duration Posterior
#'
#' Reports posterior location, uncertainty, diagnostics, and multiplicative
#' duration-ratio transforms for population-level coefficients.
#'
#' @param fit A `gp3bayes_duration_fit`.
#' @param probability Central posterior interval probability.
#' @param variables Optional supported posterior variable names.
#'
#' @return A `gp3bayes_duration_posterior_summary`.
#'
#' @details
#' Exponentiating a population-level coefficient gives its conditional
#' multiplicative effect on the median duration under the approved lognormal
#' model. This is not automatically a causal effect.
#'
#' @export
summarise_duration_posterior <- function(
  fit,
  probability = 0.95,
  variables = NULL
) {
  .gp3b_validate_gp3bayes_fit(
    fit,
    required_class =
      "gp3bayes_duration_fit"
  )

  .gp3b_require_namespace(
    "posterior",
    "summarise the duration posterior"
  )

  draws <- .gp3b_extract_draws(
    fit,
    variables = variables
  )

  table <- as.data.frame(
    .gp3b_summary_interval(
      draws,
      probability =
        probability
    ),
    stringsAsFactors = FALSE
  )

  population <- grepl(
    "^b_",
    table$variable
  )

  table$median_ratio <- NA_real_
  table$ratio_lower <- NA_real_
  table$ratio_upper <- NA_real_

  table$median_ratio[population] <-
    exp(
      table$median[population]
    )
  table$ratio_lower[population] <-
    exp(
      table$lower[population]
    )
  table$ratio_upper[population] <-
    exp(
      table$upper[population]
    )

  structure(
    list(
      summary_version = "0.1",
      family = "duration",
      outcome_unit = fit$outcome_unit,
      probability = probability,
      table = table,
      interpretation_scale = list(
        population_coefficients =
          "log duration and median ratio",
        group_standard_deviations =
          "log-duration standard deviation",
        sigma =
          "lognormal residual standard deviation",
        correlations = "correlation"
      ),
      posterior_summarised = TRUE,
      convergence_claim = FALSE,
      posterior_adequacy_established =
        FALSE
    ),
    class =
      "gp3bayes_duration_posterior_summary"
  )
}

#' Check Duration Posterior Predictive Behaviour
#'
#' Compares observed positive-duration summaries with replicated outcomes from
#' the fitted posterior predictive distribution.
#'
#' @param fit A `gp3bayes_duration_fit`.
#' @param draws Number of posterior predictive data sets.
#' @param seed Non-negative integer seed.
#' @param pass_probability Central predictive interval used for pass.
#' @param review_probability Wider predictive interval used for review.
#'
#' @return A `gp3bayes_duration_posterior_predictive_check`.
#'
#' @details
#' The check covers median, mean, upper-tail, dispersion, condition-ratio, and
#' grouping summaries. It does not prove global model adequacy.
#'
#' @export
check_duration_posterior_predictive <- function(
  fit,
  draws = 500L,
  seed = 1L,
  pass_probability = 0.80,
  review_probability = 0.95
) {
  .gp3b_validate_gp3bayes_fit(
    fit,
    required_class =
      "gp3bayes_duration_fit"
  )
  .gp3b_require_namespace(
    "posterior",
    "check duration posterior predictive behaviour"
  )

  draws <- .gp3b_assert_integer(
    draws,
    "draws",
    minimum = 50L
  )
  pass_probability <-
    .gp3b_validate_probability(
      pass_probability,
      "pass_probability",
      open = TRUE
    )
  review_probability <-
    .gp3b_validate_probability(
      review_probability,
      "review_probability",
      open = TRUE
    )

  if (
    pass_probability >=
      review_probability
  ) {
    .gp3b_stop(
      "`pass_probability` must be smaller than ",
      "`review_probability`."
    )
  }

  data <- fit$specification$prepared$data
  contract <- fit$specification$contract
  outcome <- contract$mappings$outcome
  participant <- factor(
    data[[contract$mappings$participant]]
  )
  condition <- if (
    is.null(contract$mappings$condition)
  ) {
    NULL
  } else {
    data[[contract$mappings$condition]]
  }
  item <- if (
    is.null(contract$mappings$item)
  ) {
    NULL
  } else {
    factor(
      data[[contract$mappings$item]]
    )
  }

  y <- as.numeric(
    data[[outcome]]
  )

  yrep <- .gp3b_with_seed(
    seed,
    brms::posterior_predict(
      fit$backend_fit,
      ndraws = draws
    )
  )

  yrep <- as.matrix(yrep)

  if (
    any(!is.finite(yrep)) ||
      any(yrep <= 0)
  ) {
    .gp3b_stop(
      "Posterior predictive draws must be finite and strictly positive."
    )
  }

  replicated <- t(
    apply(
      yrep,
      1L,
      .gp3d_duration_summary,
      condition = condition,
      participant = participant,
      item = item
    )
  )

  observed <- .gp3d_duration_summary(
    y,
    condition,
    participant,
    item
  )

  pass_alpha <- (
    1 - pass_probability
  ) / 2
  review_alpha <- (
    1 - review_probability
  ) / 2

  rows <- lapply(
    names(observed),
    function(statistic) {
      values <- replicated[
        ,
        statistic
      ]
      values <- values[
        is.finite(values)
      ]

      if (
        length(values) == 0L ||
          !is.finite(
            observed[[statistic]]
          )
      ) {
        return(
          data.frame(
            statistic = statistic,
            observed =
              observed[[statistic]],
            replicated_median =
              NA_real_,
            pass_lower = NA_real_,
            pass_upper = NA_real_,
            review_lower = NA_real_,
            review_upper = NA_real_,
            status =
              "not_applicable",
            stringsAsFactors = FALSE
          )
        )
      }

      pass_interval <- stats::quantile(
        values,
        probs = c(
          pass_alpha,
          1 - pass_alpha
        ),
        names = FALSE,
        type = 8
      )
      review_interval <- stats::quantile(
        values,
        probs = c(
          review_alpha,
          1 - review_alpha
        ),
        names = FALSE,
        type = 8
      )

      data.frame(
        statistic = statistic,
        observed =
          observed[[statistic]],
        replicated_median =
          stats::median(values),
        pass_lower =
          pass_interval[[1L]],
        pass_upper =
          pass_interval[[2L]],
        review_lower =
          review_interval[[1L]],
        review_upper =
          review_interval[[2L]],
        status =
          .gp3b_predictive_status(
            observed =
              observed[[statistic]],
            pass_lower =
              pass_interval[[1L]],
            pass_upper =
              pass_interval[[2L]],
            review_lower =
              review_interval[[1L]],
            review_upper =
              review_interval[[2L]]
          ),
        stringsAsFactors = FALSE
      )
    }
  )

  check_table <- do.call(
    rbind,
    rows
  )

  expected <- brms::posterior_epred(
    fit$backend_fit,
    draw_ids = seq_len(
      min(
        draws,
        posterior::ndraws(
          posterior::as_draws_array(
            fit$backend_fit
          )
        )
      )
    )
  )

  predicted_mean <- colMeans(
    as.matrix(expected)
  )
  log_rmse <- sqrt(
    mean(
      (
        log(y) -
          log(predicted_mean)
      )^2
    )
  )

  structure(
    list(
      check_version = "0.1",
      family = "duration",
      outcome_unit = fit$outcome_unit,
      draws = nrow(yrep),
      seed = as.integer(seed),
      observed = observed,
      replicated = replicated,
      checks = check_table,
      log_scale_rmse = log_rmse,
      status = .gp3b_worst_status(
        check_table$status
      ),
      posterior_predictive_performed =
        TRUE,
      adequacy_established = FALSE,
      interpretation = paste(
        "The status describes selected duration predictive",
        "summaries and is not a global declaration of model",
        "adequacy."
      )
    ),
    class = c(
      "gp3bayes_duration_posterior_predictive_check",
      "gp3bayes_posterior_predictive_check"
    )
  )
}

#' Assess Duration Prior Sensitivity
#'
#' Refits the approved duration model under prespecified tighter and wider prior
#' scales and compares population-level and residual posterior medians.
#'
#' @param fit A `gp3bayes_duration_fit`.
#' @param scale_multipliers Named positive prior-scale multipliers.
#' @param chains,iter,warmup,cores,seed,adapt_delta,max_treedepth,refresh
#'   Restricted sampling controls passed to [fit_duration_model()].
#' @param maximum_standardized_shift Maximum standardized median shift for
#'   pass.
#' @param review_standardized_shift Maximum standardized median shift for
#'   review.
#' @param retain_fits Whether alternative fits are retained.
#'
#' @return A `gp3bayes_duration_prior_sensitivity`.
#'
#' @export
assess_duration_prior_sensitivity <- function(
  fit,
  scale_multipliers = c(
    tighter = 0.5,
    wider = 2
  ),
  chains = fit$sampling$chains,
  iter = fit$sampling$iter,
  warmup = fit$sampling$warmup,
  cores = fit$sampling$cores,
  seed = fit$sampling$seed + 2000L,
  adapt_delta =
    fit$sampling$adapt_delta,
  max_treedepth =
    fit$sampling$max_treedepth,
  refresh = 0L,
  maximum_standardized_shift = 0.25,
  review_standardized_shift = 0.50,
  retain_fits = FALSE
) {
  .gp3b_validate_gp3bayes_fit(
    fit,
    required_class =
      "gp3bayes_duration_fit"
  )

  if (
    !is.numeric(scale_multipliers) ||
      length(scale_multipliers) < 1L ||
      anyNA(scale_multipliers) ||
      any(!is.finite(scale_multipliers)) ||
      any(scale_multipliers <= 0) ||
      is.null(names(scale_multipliers)) ||
      any(!nzchar(names(scale_multipliers))) ||
      anyDuplicated(names(scale_multipliers))
  ) {
    .gp3b_stop(
      "`scale_multipliers` must be named unique positive finite values."
    )
  }

  maximum_standardized_shift <-
    .gp3b_assert_numeric_scalar(
      maximum_standardized_shift,
      "maximum_standardized_shift",
      lower = 0
    )
  review_standardized_shift <-
    .gp3b_assert_numeric_scalar(
      review_standardized_shift,
      "review_standardized_shift",
      lower =
        maximum_standardized_shift
    )
  retain_fits <- .gp3b_assert_flag(
    retain_fits,
    "retain_fits"
  )

  reference <-
    summarise_duration_posterior(
      fit
    )$table

  reference <- reference[
    grepl(
      "^b_|^sigma$",
      reference$variable
    ),
    c(
      "variable",
      "median",
      "sd"
    ),
    drop = FALSE
  ]

  reference_diagnostics <-
    diagnose_duration_fit(
      fit
    )

  alternative_fits <- vector(
    "list",
    length(scale_multipliers)
  )
  names(alternative_fits) <-
    names(scale_multipliers)
  comparison_rows <- list()

  for (
    index in seq_along(
      scale_multipliers
    )
  ) {
    label <- names(
      scale_multipliers
    )[[index]]
    multiplier <-
      scale_multipliers[[index]]

    specification <-
      .gp3d_duration_sensitivity_specification(
        fit,
        multiplier
      )

    alternative_fit <-
      fit_duration_model(
        specification,
        chains = chains,
        iter = iter,
        warmup = warmup,
        cores = cores,
        seed =
          seed + index - 1L,
        adapt_delta =
          adapt_delta,
        max_treedepth =
          max_treedepth,
        refresh = refresh
      )

    alternative_diagnostics <-
      diagnose_duration_fit(
        alternative_fit
      )

    alternative <-
      summarise_duration_posterior(
        alternative_fit
      )$table

    alternative <- alternative[
      grepl(
        "^b_|^sigma$",
        alternative$variable
      ),
      c(
        "variable",
        "median"
      ),
      drop = FALSE
    ]
    names(alternative)[
      names(alternative) == "median"
    ] <- "alternative_median"

    merged <- merge(
      reference,
      alternative,
      by = "variable",
      all = FALSE,
      sort = FALSE
    )

    merged$scenario <- label
    merged$scale_multiplier <-
      multiplier
    merged$median_shift <-
      merged$alternative_median -
      merged$median
    merged$standardized_shift <-
      abs(merged$median_shift) /
      pmax(
        merged$sd,
        .Machine$double.eps
      )
    merged$shift_status <- vapply(
      merged$standardized_shift,
      .gp3b_classify_upper,
      character(1),
      pass =
        maximum_standardized_shift,
      review =
        review_standardized_shift
    )
    merged$diagnostic_status <-
      alternative_diagnostics$status
    merged$status <- vapply(
      merged$shift_status,
      function(status) {
        .gp3b_worst_status(
          c(
            status,
            alternative_diagnostics$status
          )
        )
      },
      character(1)
    )

    comparison_rows[[index]] <-
      merged[
        ,
        c(
          "scenario",
          "scale_multiplier",
          "variable",
          "median",
          "alternative_median",
          "median_shift",
          "standardized_shift",
          "shift_status",
          "diagnostic_status",
          "status"
        ),
        drop = FALSE
      ]

    if (retain_fits) {
      alternative_fits[[label]] <-
        alternative_fit
    }
  }

  comparison <- do.call(
    rbind,
    comparison_rows
  )
  rownames(comparison) <- NULL

  scenario_status <- do.call(
    rbind,
    lapply(
      split(
        comparison,
        comparison$scenario
      ),
      function(table) {
        data.frame(
          scenario =
            table$scenario[[1L]],
          scale_multiplier =
            table$scale_multiplier[[1L]],
          maximum_standardized_shift =
            max(
              table$standardized_shift,
              na.rm = TRUE
            ),
          diagnostic_status =
            table$diagnostic_status[[1L]],
          status =
            .gp3b_worst_status(
              table$status
            ),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  structure(
    list(
      sensitivity_version = "0.1",
      family = "duration",
      scale_multipliers =
        scale_multipliers,
      comparison = comparison,
      scenario_status =
        scenario_status,
      reference_diagnostic_status =
        reference_diagnostics$status,
      status = .gp3b_worst_status(
        c(
          reference_diagnostics$status,
          scenario_status$status
        )
      ),
      alternative_fits =
        if (retain_fits) {
          alternative_fits
        } else {
          NULL
        },
      sensitivity_assessed = TRUE,
      robustness_claim = FALSE,
      posterior_adequacy_established =
        FALSE
    ),
    class = c(
      "gp3bayes_duration_prior_sensitivity",
      "gp3bayes_prior_sensitivity"
    )
  )
}

#' Run Duration Parameter Recovery
#'
#' Repeatedly simulates and fits the approved hierarchical lognormal duration
#' model and compares posterior intervals with known generating values.
#'
#' @inheritParams run_binary_recovery
#' @param baseline_median Baseline synthetic duration median.
#' @param outcome_unit Synthetic duration unit.
#'
#' @return A `gp3bayes_duration_recovery`.
#'
#' @details
#' A small run is a smoke test. No result creates an automatic validation claim.
#'
#' @export
run_duration_recovery <- function(
  repetitions = 20L,
  n_participants = 30L,
  trials_per_participant = 16L,
  n_items = 12L,
  include_items = TRUE,
  random_slope = TRUE,
  baseline_median = 500,
  outcome_unit = "milliseconds",
  seed = 2001L,
  chains = 4L,
  iter = 1500L,
  warmup = 750L,
  cores = min(
    chains,
    .gp3b_default_cores(chains)
  ),
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L,
  interval_probability = 0.95,
  minimum_repetitions = 20L,
  maximum_standardized_bias = 0.25,
  minimum_coverage = 0.80,
  minimum_diagnostic_pass_fraction = 0.80,
  continue_on_error = TRUE
) {
  repetitions <- .gp3b_assert_integer(
    repetitions,
    "repetitions",
    minimum = 2L
  )
  n_participants <- .gp3b_assert_integer(
    n_participants,
    "n_participants",
    minimum = 2L
  )
  trials_per_participant <-
    .gp3b_assert_integer(
      trials_per_participant,
      "trials_per_participant",
      minimum = 2L
    )
  n_items <- .gp3b_assert_integer(
    n_items,
    "n_items",
    minimum = 2L
  )
  include_items <- .gp3b_assert_flag(
    include_items,
    "include_items"
  )
  random_slope <- .gp3b_assert_flag(
    random_slope,
    "random_slope"
  )
  baseline_median <-
    .gp3b_assert_numeric_scalar(
      baseline_median,
      "baseline_median",
      lower = 0,
      lower_open = TRUE
    )
  seed <- .gp3b_assert_integer(
    seed,
    "seed",
    minimum = 0L
  )
  interval_probability <-
    .gp3b_validate_probability(
      interval_probability,
      "interval_probability",
      open = TRUE
    )
  minimum_repetitions <-
    .gp3b_assert_integer(
      minimum_repetitions,
      "minimum_repetitions",
      minimum = 2L
    )
  maximum_standardized_bias <-
    .gp3b_assert_numeric_scalar(
      maximum_standardized_bias,
      "maximum_standardized_bias",
      lower = 0
    )
  minimum_coverage <-
    .gp3b_validate_probability(
      minimum_coverage,
      "minimum_coverage"
    )
  minimum_diagnostic_pass_fraction <-
    .gp3b_validate_probability(
      minimum_diagnostic_pass_fraction,
      "minimum_diagnostic_pass_fraction"
    )
  continue_on_error <- .gp3b_assert_flag(
    continue_on_error,
    "continue_on_error"
  )

  if (
    !is.character(outcome_unit) ||
      length(outcome_unit) != 1L ||
      is.na(outcome_unit) ||
      !nzchar(outcome_unit)
  ) {
    .gp3b_stop(
      "`outcome_unit` must be one non-empty character value."
    )
  }

  estimates_list <- list()
  fit_rows <- list()
  estimate_index <- 1L

  for (
    repetition in seq_len(repetitions)
  ) {
    repetition_seed <-
      seed + repetition - 1L

    result <- tryCatch(
      {
        simulation <-
          simulate_hierarchical_duration_data(
            n_participants =
              n_participants,
            trials_per_participant =
              trials_per_participant,
            n_items = n_items,
            include_items =
              include_items,
            random_slope_sd =
              if (random_slope) {
                0.15
              } else {
                0
              },
            baseline_median =
              baseline_median,
            outcome_unit =
              outcome_unit,
            seed = repetition_seed
          )

        contract <- create_model_contract(
          family = "duration",
          outcome_col = "duration",
          participant_col =
            "participant_id",
          item_col =
            if (include_items) {
              "item_id"
            } else {
              NULL
            },
          trial_col = "trial_id",
          condition_col =
            "condition",
          predictors = c(
            "participant_covariate",
            "trial_covariate"
          ),
          interaction = c(
            "condition",
            "participant_covariate"
          ),
          random_slope =
            random_slope,
          outcome_unit =
            outcome_unit
        )

        prepared <-
          prepare_hierarchical_duration_data(
            simulation$data,
            contract,
            condition_levels = c(
              "control",
              "treatment"
            )
          )

        specification <-
          specify_duration_model(
            prepared,
            baseline =
              baseline_median
          )

        fitted <- fit_duration_model(
          specification,
          chains = chains,
          iter = iter,
          warmup = warmup,
          cores = cores,
          seed =
            repetition_seed + 200000L,
          adapt_delta =
            adapt_delta,
          max_treedepth =
            max_treedepth,
          refresh = refresh
        )

        diagnostics <-
          diagnose_duration_fit(
            fitted
          )
        posterior <-
          summarise_duration_posterior(
            fitted,
            probability =
              interval_probability
          )$table
        truth_table <-
          .gp3d_duration_truth_table(
            simulation,
            posterior$variable
          )

        merged <- merge(
          truth_table,
          posterior[
            ,
            c(
              "variable",
              "median",
              "lower",
              "upper"
            ),
            drop = FALSE
          ],
          by = "variable",
          all = FALSE,
          sort = FALSE
        )

        merged$repetition <-
          repetition
        merged$seed <-
          repetition_seed
        merged$covered <-
          merged$truth >= merged$lower &
          merged$truth <= merged$upper
        merged$diagnostic_status <-
          diagnostics$status
        merged$error <- NA_character_

        list(
          estimates = merged,
          fit_status = data.frame(
            repetition = repetition,
            seed = repetition_seed,
            diagnostic_status =
              diagnostics$status,
            completed = TRUE,
            error = NA_character_,
            stringsAsFactors = FALSE
          )
        )
      },
      error = function(error) {
        if (!continue_on_error) {
          stop(error)
        }

        list(
          estimates = NULL,
          fit_status = data.frame(
            repetition = repetition,
            seed = repetition_seed,
            diagnostic_status =
              "fail",
            completed = FALSE,
            error =
              conditionMessage(error),
            stringsAsFactors = FALSE
          )
        )
      }
    )

    fit_rows[[repetition]] <-
      result$fit_status

    if (!is.null(result$estimates)) {
      estimates_list[[estimate_index]] <-
        result$estimates
      estimate_index <-
        estimate_index + 1L
    }
  }

  fits <- do.call(
    rbind,
    fit_rows
  )
  estimates <- if (
    length(estimates_list) > 0L
  ) {
    do.call(
      rbind,
      estimates_list
    )
  } else {
    data.frame()
  }

  completed <- sum(
    fits$completed
  )
  diagnostic_pass_fraction <-
    mean(
      fits$diagnostic_status == "pass"
    )

  parameter_summary <- if (
    nrow(estimates) > 0L
  ) {
    do.call(
      rbind,
      lapply(
        split(
          estimates,
          estimates$variable
        ),
        function(table) {
          bias <- mean(
            table$median - table$truth
          )
          empirical_sd <- stats::sd(
            table$median
          )
          standardized_bias <- if (
            is.finite(empirical_sd) &&
              empirical_sd > 0
          ) {
            bias / empirical_sd
          } else {
            NA_real_
          }
          coverage <- mean(
            table$covered
          )

          data.frame(
            variable =
              table$variable[[1L]],
            truth =
              mean(table$truth),
            repetitions =
              nrow(table),
            mean_estimate =
              mean(table$median),
            bias = bias,
            rmse = sqrt(
              mean(
                (
                  table$median -
                    table$truth
                )^2
              )
            ),
            standardized_bias =
              standardized_bias,
            coverage = coverage,
            mean_interval_width =
              mean(
                table$upper -
                  table$lower
              ),
            diagnostic_pass_fraction =
              diagnostic_pass_fraction,
            status =
              .gp3b_recovery_component_status(
                repetitions =
                  nrow(table),
                standardized_bias =
                  standardized_bias,
                coverage = coverage,
                diagnostic_pass_fraction =
                  diagnostic_pass_fraction,
                minimum_repetitions =
                  minimum_repetitions,
                maximum_standardized_bias =
                  maximum_standardized_bias,
                minimum_coverage =
                  minimum_coverage,
                minimum_diagnostic_pass_fraction =
                  minimum_diagnostic_pass_fraction
              ),
            stringsAsFactors = FALSE
          )
        }
      )
    )
  } else {
    data.frame()
  }

  overall_status <- if (
    completed == 0L ||
      nrow(parameter_summary) == 0L
  ) {
    "fail"
  } else {
    .gp3b_worst_status(
      parameter_summary$status
    )
  }

  if (
    completed < minimum_repetitions &&
      identical(
        overall_status,
        "pass"
      )
  ) {
    overall_status <- "review"
  }

  structure(
    list(
      recovery_version = "0.1",
      family = "duration",
      outcome_unit =
        outcome_unit,
      repetitions_requested =
        repetitions,
      repetitions_completed =
        completed,
      fit_status = fits,
      estimates = estimates,
      parameter_summary =
        parameter_summary,
      status = overall_status,
      thresholds = list(
        interval_probability =
          interval_probability,
        minimum_repetitions =
          minimum_repetitions,
        maximum_standardized_bias =
          maximum_standardized_bias,
        minimum_coverage =
          minimum_coverage,
        minimum_diagnostic_pass_fraction =
          minimum_diagnostic_pass_fraction
      ),
      recovery_assessed = TRUE,
      validation_claim = FALSE,
      posterior_adequacy_established =
        FALSE
    ),
    class = c(
      "gp3bayes_duration_recovery",
      "gp3bayes_recovery"
    )
  )
}

#' Create a Structured Duration Model Report
#'
#' Writes a conservative Markdown report for an approved fitted lognormal
#' duration model.
#'
#' @param fit A `gp3bayes_duration_fit`.
#' @param diagnostics Optional result from [diagnose_duration_fit()].
#' @param posterior_summary Optional result from
#'   [summarise_duration_posterior()].
#' @param posterior_predictive Optional result from
#'   [check_duration_posterior_predictive()].
#' @param prior_sensitivity Optional result from
#'   [assess_duration_prior_sensitivity()].
#' @param recovery Optional result from [run_duration_recovery()].
#' @param file Output Markdown path.
#' @param overwrite Whether an existing file may be replaced.
#'
#' @return A `gp3bayes_duration_model_report`.
#'
#' @export
create_duration_model_report <- function(
  fit,
  diagnostics = NULL,
  posterior_summary = NULL,
  posterior_predictive = NULL,
  prior_sensitivity = NULL,
  recovery = NULL,
  file = "gp3bayes-duration-model-report.md",
  overwrite = FALSE
) {
  .gp3b_validate_gp3bayes_fit(
    fit,
    required_class =
      "gp3bayes_duration_fit"
  )

  overwrite <- .gp3b_assert_flag(
    overwrite,
    "overwrite"
  )

  if (
    !is.character(file) ||
      length(file) != 1L ||
      is.na(file) ||
      !nzchar(file) ||
      !grepl(
        "\\.md$",
        file,
        ignore.case = TRUE
      )
  ) {
    .gp3b_stop(
      "`file` must be one non-empty Markdown path ending in `.md`."
    )
  }

  if (
    file.exists(file) &&
      !overwrite
  ) {
    .gp3b_stop(
      "The report file already exists. Use `overwrite = TRUE` to replace it."
    )
  }

  if (is.null(diagnostics)) {
    diagnostics <-
      diagnose_duration_fit(
        fit
      )
  }
  if (is.null(posterior_summary)) {
    posterior_summary <-
      summarise_duration_posterior(
        fit
      )
  }

  if (!inherits(
    diagnostics,
    "gp3bayes_duration_diagnostics"
  )) {
    .gp3b_stop(
      "`diagnostics` must be a duration diagnostic result."
    )
  }
  if (!inherits(
    posterior_summary,
    "gp3bayes_duration_posterior_summary"
  )) {
    .gp3b_stop(
      "`posterior_summary` must be a duration posterior summary."
    )
  }

  lines <- c(
    "# gp3bayes duration model report",
    "",
    paste0(
      "- Generated (UTC): ",
      format(
        Sys.time(),
        tz = "UTC",
        usetz = TRUE
      )
    ),
    paste0(
      "- Package version: ",
      .gp3b_current_package_version()
    ),
    paste0(
      "- Formula: `",
      fit$translation$formula_text,
      "`"
    ),
    "- Family: lognormal",
    paste0(
      "- Outcome unit: ",
      fit$outcome_unit
    ),
    "- Interface: brms",
    "- Sampling backend: rstan",
    paste0(
      "- Chains: ",
      fit$sampling$chains
    ),
    paste0(
      "- Iterations per chain: ",
      fit$sampling$iter
    ),
    paste0(
      "- Warmup per chain: ",
      fit$sampling$warmup
    ),
    "",
    "## Sampling diagnostics",
    "",
    paste0(
      "**Threshold status: ",
      diagnostics$status,
      "**"
    ),
    "",
    .gp3b_markdown_table(
      diagnostics$component_table
    ),
    "",
    paste(
      "Numerical thresholds were assessed, but no automatic",
      "convergence or posterior-adequacy claim is made."
    ),
    "",
    "## Posterior summaries",
    "",
    .gp3b_markdown_table(
      posterior_summary$table,
      max_rows = 30L
    )
  )

  registry <- data.frame(
    section = c(
      "sampling_diagnostics",
      "posterior_summary"
    ),
    status = c(
      diagnostics$status,
      "reported"
    ),
    stringsAsFactors = FALSE
  )

  if (!is.null(
    posterior_predictive
  )) {
    if (!inherits(
      posterior_predictive,
      "gp3bayes_duration_posterior_predictive_check"
    )) {
      .gp3b_stop(
        "`posterior_predictive` must be a duration posterior predictive check."
      )
    }

    lines <- c(
      lines,
      "",
      "## Posterior predictive checks",
      "",
      paste0(
        "**Threshold status: ",
        posterior_predictive$status,
        "**"
      ),
      "",
      .gp3b_markdown_table(
        posterior_predictive$checks
      ),
      "",
      paste0(
        "- Log-scale RMSE: ",
        formatC(
          posterior_predictive$log_scale_rmse,
          digits = 4,
          format = "fg"
        )
      ),
      "",
      paste(
        "This status covers selected predictive summaries only",
        "and does not establish global model adequacy."
      )
    )

    registry <- rbind(
      registry,
      data.frame(
        section =
          "posterior_predictive",
        status =
          posterior_predictive$status,
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(prior_sensitivity)) {
    if (!inherits(
      prior_sensitivity,
      "gp3bayes_duration_prior_sensitivity"
    )) {
      .gp3b_stop(
        "`prior_sensitivity` must be a duration prior-sensitivity result."
      )
    }

    lines <- c(
      lines,
      "",
      "## Prior sensitivity",
      "",
      paste0(
        "**Threshold status: ",
        prior_sensitivity$status,
        "**"
      ),
      "",
      .gp3b_markdown_table(
        prior_sensitivity$scenario_status
      ),
      "",
      paste(
        "The result applies only to the declared prior-scale",
        "multipliers and is not a universal robustness claim."
      )
    )

    registry <- rbind(
      registry,
      data.frame(
        section =
          "prior_sensitivity",
        status =
          prior_sensitivity$status,
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(recovery)) {
    if (!inherits(
      recovery,
      "gp3bayes_duration_recovery"
    )) {
      .gp3b_stop(
        "`recovery` must be a duration recovery result."
      )
    }

    lines <- c(
      lines,
      "",
      "## Simulation-based recovery",
      "",
      paste0(
        "**Threshold status: ",
        recovery$status,
        "**"
      ),
      "",
      .gp3b_markdown_table(
        recovery$parameter_summary
      ),
      "",
      paste(
        "Recovery applies only to the declared synthetic design.",
        "No automatic validation claim is made."
      )
    )

    registry <- rbind(
      registry,
      data.frame(
        section = "recovery",
        status = recovery$status,
        stringsAsFactors = FALSE
      )
    )
  }

  lines <- c(
    lines,
    "",
    "## Interpretation boundaries",
    "",
    paste(
      "Duration coefficients are conditional log-scale associations",
      "and median ratios under the approved model. They are not",
      "automatically causal and do not directly identify latent",
      "psychological or protected attributes."
    ),
    "",
    "## Overall reporting statement",
    "",
    paste(
      "Model fitting completed. Sampling and validation results are",
      "reported separately. This report does not automatically declare",
      "convergence, posterior adequacy, predictive validity, or",
      "substantive validity."
    )
  )

  directory <- dirname(file)

  if (
    !identical(directory, ".") &&
      !dir.exists(directory)
  ) {
    dir.create(
      directory,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  writeLines(
    lines,
    con = file,
    useBytes = TRUE
  )

  structure(
    list(
      report_version = "0.1",
      family = "duration",
      outcome_unit =
        fit$outcome_unit,
      file = normalizePath(
        file,
        winslash = "/",
        mustWork = TRUE
      ),
      sections = registry,
      report_created = TRUE,
      convergence_claim = FALSE,
      posterior_adequacy_established =
        FALSE
    ),
    class = c(
      "gp3bayes_duration_model_report",
      "gp3bayes_model_report"
    )
  )
}

#' @export
print.gp3bayes_duration_posterior_summary <- function(
  x,
  ...
) {
  cat("<gp3bayes_duration_posterior_summary>\n")
  cat(
    "  Parameters: ",
    nrow(x$table),
    "\n",
    sep = ""
  )
  cat(
    "  Outcome unit: ",
    x$outcome_unit,
    "\n",
    sep = ""
  )
  cat(
    "  Interval probability: ",
    x$probability,
    "\n",
    sep = ""
  )
  cat("  Automatic convergence claim: FALSE\n")
  cat("  Posterior adequacy established: FALSE\n")

  invisible(x)
}
