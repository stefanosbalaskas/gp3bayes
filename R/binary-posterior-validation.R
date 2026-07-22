.gp3b_binary_observed_summary <- function(
  y,
  condition,
  participant,
  item = NULL
) {
  participant_rates <- tapply(
    y,
    participant,
    mean
  )

  condition_low_rate <- NA_real_
  condition_high_rate <- NA_real_
  condition_rate_contrast <- NA_real_

  if (!is.null(condition)) {
    levels <- sort(
      unique(condition)
    )

    if (length(levels) == 2L) {
      rates <- vapply(
        levels,
        function(level) {
          mean(
            y[condition == level]
          )
        },
        numeric(1)
      )

      condition_low_rate <- rates[[1L]]
      condition_high_rate <- rates[[2L]]
      condition_rate_contrast <-
        rates[[2L]] - rates[[1L]]
    }
  }

  item_rate_sd <- NA_real_

  if (!is.null(item)) {
    item_rates <- tapply(
      y,
      item,
      mean
    )

    if (length(item_rates) > 1L) {
      item_rate_sd <- stats::sd(
        item_rates
      )
    }
  }

  c(
    overall_rate = mean(y),
    condition_low_rate = condition_low_rate,
    condition_high_rate = condition_high_rate,
    condition_rate_contrast =
      condition_rate_contrast,
    participant_rate_sd = if (
      length(participant_rates) > 1L
    ) {
      stats::sd(participant_rates)
    } else {
      NA_real_
    },
    item_rate_sd = item_rate_sd
  )
}

.gp3b_predictive_status <- function(
  observed,
  pass_lower,
  pass_upper,
  review_lower,
  review_upper
) {
  if (
    anyNA(
      c(
        observed,
        pass_lower,
        pass_upper,
        review_lower,
        review_upper
      )
    )
  ) {
    return("not_applicable")
  }

  if (
    observed >= pass_lower &&
      observed <= pass_upper
  ) {
    "pass"
  } else if (
    observed >= review_lower &&
      observed <= review_upper
  ) {
    "review"
  } else {
    "fail"
  }
}

.gp3b_binary_truth_table <- function(
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
      truth$random_slope_cor
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

.gp3b_recovery_component_status <- function(
  repetitions,
  standardized_bias,
  coverage,
  diagnostic_pass_fraction,
  minimum_repetitions,
  maximum_standardized_bias,
  minimum_coverage,
  minimum_diagnostic_pass_fraction
) {
  if (
    anyNA(
      c(
        repetitions,
        standardized_bias,
        coverage,
        diagnostic_pass_fraction
      )
    )
  ) {
    return("review")
  }

  fail <- abs(standardized_bias) >
    2 * maximum_standardized_bias ||
    coverage < minimum_coverage - 0.15 ||
    diagnostic_pass_fraction <
      minimum_diagnostic_pass_fraction - 0.20

  if (fail) {
    return("fail")
  }

  pass <- repetitions >= minimum_repetitions &&
    abs(standardized_bias) <=
      maximum_standardized_bias &&
    coverage >= minimum_coverage &&
    diagnostic_pass_fraction >=
      minimum_diagnostic_pass_fraction

  if (pass) {
    "pass"
  } else {
    "review"
  }
}

#' Diagnose a Fitted Binary Model
#'
#' Computes rank-normalized R-hat, bulk and tail effective sample sizes,
#' divergent-transition counts, maximum-treedepth saturation, and chain-level
#' energy diagnostics for an approved binary fit.
#'
#' @inheritParams plot_sampling_diagnostics
#' @param rhat_pass R-hat value at or below which the component passes.
#' @param rhat_fail R-hat value above which the component fails.
#' @param ess_per_chain_pass Bulk or tail ESS per chain at or above which the
#'   component passes.
#' @param ess_per_chain_fail Bulk or tail ESS per chain below which the
#'   component fails.
#' @param maximum_treedepth_fraction Maximum fraction of post-warmup draws that
#'   may reach the configured maximum treedepth before the component fails.
#' @param ebfmi_pass E-BFMI value at or above which the energy component passes.
#' @param ebfmi_fail E-BFMI value below which the energy component fails.
#'
#' @return A `gp3bayes_binary_diagnostics` object containing parameter,
#'   component, and chain-level diagnostic tables.
#'
#' @details
#' The overall status is `"fail"` when any component fails, `"review"` when
#' any component requires review or cannot be assessed, and `"pass"` only when
#' every component passes. A pass does not automatically establish convergence
#' or posterior adequacy.
#'
#' @export
diagnose_binary_fit <- function(
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
      "gp3bayes_binary_fit",
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

#' Summarise a Binary Posterior
#'
#' Reports posterior location, uncertainty intervals, R-hat, effective sample
#' sizes, probability of a positive coefficient, and odds-ratio transforms for
#' population-level coefficients.
#'
#' @param fit A `gp3bayes_binary_fit`.
#' @param probability Central posterior interval probability.
#' @param variables Optional supported posterior variable names.
#'
#' @return A `gp3bayes_binary_posterior_summary`.
#'
#' @details
#' Probability-positive values and intervals are descriptive posterior
#' summaries. They are not frequentist significance tests and do not establish
#' causal or substantive validity.
#'
#' @export
summarise_binary_posterior <- function(
  fit,
  probability = 0.95,
  variables = NULL
) {
  .gp3b_validate_gp3bayes_fit(
    fit,
    required_class =
      "gp3bayes_binary_fit"
  )

  .gp3b_require_namespace(
    "posterior",
    "summarise the binary posterior"
  )

  draws <- .gp3b_extract_draws(
    fit,
    variables = variables
  )

  table <- as.data.frame(
    .gp3b_summary_interval(
      draws,
      probability = probability
    ),
    stringsAsFactors = FALSE
  )

  population <- grepl(
    "^b_",
    table$variable
  )

  table$odds_ratio_median <- NA_real_
  table$odds_ratio_lower <- NA_real_
  table$odds_ratio_upper <- NA_real_

  table$odds_ratio_median[population] <-
    exp(table$median[population])
  table$odds_ratio_lower[population] <-
    exp(table$lower[population])
  table$odds_ratio_upper[population] <-
    exp(table$upper[population])

  structure(
    list(
      summary_version = "0.1",
      family = "binary",
      probability = probability,
      table = table,
      interpretation_scale = list(
        population_coefficients =
          "log-odds and odds ratio",
        group_standard_deviations =
          "log-odds standard deviation",
        correlations = "correlation"
      ),
      posterior_summarised = TRUE,
      convergence_claim = FALSE,
      posterior_adequacy_established = FALSE
    ),
    class = "gp3bayes_binary_posterior_summary"
  )
}

#' Check Binary Posterior Predictive Behaviour
#'
#' Compares observed binary summaries with replicated outcomes from the fitted
#' posterior predictive distribution.
#'
#' @param fit A `gp3bayes_binary_fit`.
#' @param draws Number of posterior predictive replications.
#' @param seed Non-negative integer seed used to select predictive draws.
#' @param pass_probability Central predictive interval used for a pass.
#' @param review_probability Wider central predictive interval used for review.
#'
#' @return A `gp3bayes_binary_posterior_predictive_check`.
#'
#' @details
#' The check evaluates prespecified descriptive summaries. It does not prove
#' that the likelihood, link, random-effects structure, or substantive model is
#' adequate.
#'
#' @export
check_binary_posterior_predictive <- function(
  fit,
  draws = 500,
  seed = 1,
  pass_probability = 0.80,
  review_probability = 0.95
) {
  .gp3b_validate_gp3bayes_fit(
    fit,
    required_class =
      "gp3bayes_binary_fit"
  )
  .gp3b_require_namespace(
    "posterior",
    "check binary posterior predictive behaviour"
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

  y <- as.integer(
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

  replicated <- t(
    apply(
      yrep,
      1L,
      .gp3b_binary_observed_summary,
      condition = condition,
      participant = participant,
      item = item
    )
  )

  observed <- .gp3b_binary_observed_summary(
    y,
    condition = condition,
    participant = participant,
    item = item
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
            status = "not_applicable",
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

  expected_probability <- colMeans(
    as.matrix(expected)
  )

  brier_score <- mean(
    (
      y - expected_probability
    )^2
  )

  structure(
    list(
      check_version = "0.1",
      family = "binary",
      draws = nrow(yrep),
      seed = as.integer(seed),
      observed = observed,
      replicated = replicated,
      checks = check_table,
      brier_score = brier_score,
      status = .gp3b_worst_status(
        check_table$status
      ),
      posterior_predictive_performed =
        TRUE,
      adequacy_established = FALSE,
      interpretation = paste(
        "The status describes selected posterior predictive",
        "summaries and is not a global declaration of model",
        "adequacy."
      )
    ),
    class = c(
      "gp3bayes_binary_posterior_predictive_check",
      "gp3bayes_posterior_predictive_check"
    )
  )
}

.gp3b_binary_sensitivity_specification <- function(
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

  correlation_eta <- 2

  if (isTRUE(original$contract$random_slope)) {
    correlation_eta <- .gp3b_prior_row(
      priors,
      "cor"
    )$shape[[1L]]
  }

  specify_binary_model(
    original$prepared,
    baseline = priors$baseline,
    intercept_scale =
      intercept$scale[[1L]] * multiplier,
    coefficient_scale =
      coefficient$scale[[1L]] * multiplier,
    group_sd_scale =
      group_sd$scale[[1L]] * multiplier,
    correlation_eta =
      correlation_eta,
    student_df =
      group_sd$df[[1L]]
  )
}

#' Assess Binary Prior Sensitivity
#'
#' Refits the approved binary model under prespecified tighter and wider prior
#' scales and compares population-level posterior medians.
#'
#' @param fit A `gp3bayes_binary_fit`.
#' @param scale_multipliers Named positive numeric multipliers applied to the
#'   intercept, coefficient, and group-scale priors.
#' @param chains,iter,warmup,cores,seed,adapt_delta,max_treedepth,refresh
#'   Restricted sampling controls passed to [fit_binary_model()].
#' @param maximum_standardized_shift Maximum absolute posterior-median shift,
#'   divided by the reference posterior standard deviation, for a pass.
#' @param review_standardized_shift Maximum standardized shift for review.
#' @param retain_fits Whether alternative fitted objects should be retained.
#'
#' @return A `gp3bayes_binary_prior_sensitivity` object.
#'
#' @details
#' This function is computationally expensive. Sensitivity status describes
#' stability under the declared scale changes only; it does not prove prior
#' robustness under all defensible priors.
#'
#' @export
assess_binary_prior_sensitivity <- function(
  fit,
  scale_multipliers = c(
    tighter = 0.5,
    wider = 2
  ),
  chains = fit$sampling$chains,
  iter = fit$sampling$iter,
  warmup = fit$sampling$warmup,
  cores = fit$sampling$cores,
  seed = fit$sampling$seed + 1000L,
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
      "gp3bayes_binary_fit"
  )

  if (
    !is.numeric(scale_multipliers) ||
      length(scale_multipliers) < 1L ||
      anyNA(scale_multipliers) ||
      any(!is.finite(scale_multipliers)) ||
      any(scale_multipliers <= 0)
  ) {
    .gp3b_stop(
      "`scale_multipliers` must contain one or more ",
      "strictly positive finite numeric values."
    )
  }

  if (
    is.null(names(scale_multipliers)) ||
      any(!nzchar(names(scale_multipliers))) ||
      anyDuplicated(names(scale_multipliers))
  ) {
    .gp3b_stop(
      "`scale_multipliers` must have unique non-empty names."
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
      lower = maximum_standardized_shift
    )
  retain_fits <- .gp3b_assert_flag(
    retain_fits,
    "retain_fits"
  )

  reference <- summarise_binary_posterior(
    fit
  )$table

  reference <- reference[
    grepl(
      "^b_",
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
    diagnose_binary_fit(
      fit
    )

  alternative_fits <- vector(
    "list",
    length(scale_multipliers)
  )
  names(alternative_fits) <-
    names(scale_multipliers)

  comparison_rows <- list()
  row_index <- 1L

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

    alternative_specification <-
      .gp3b_binary_sensitivity_specification(
        fit,
        multiplier
      )

    alternative_fit <- fit_binary_model(
      alternative_specification,
      chains = chains,
      iter = iter,
      warmup = warmup,
      cores = cores,
      seed = seed + index - 1L,
      adapt_delta = adapt_delta,
      max_treedepth =
        max_treedepth,
      refresh = refresh
    )

    alternative_diagnostics <-
      diagnose_binary_fit(
        alternative_fit
      )

    alternative <- summarise_binary_posterior(
      alternative_fit
    )$table

    alternative <- alternative[
      grepl(
        "^b_",
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

    comparison_rows[[row_index]] <-
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

    row_index <- row_index + 1L

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
      family = "binary",
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
      "gp3bayes_binary_prior_sensitivity",
      "gp3bayes_prior_sensitivity"
    )
  )
}

#' Run Binary Parameter Recovery
#'
#' Repeatedly simulates from the approved hierarchical Bernoulli-logit
#' generator, fits the restricted model, and compares posterior intervals with
#' known generating values.
#'
#' @param repetitions Number of simulation-fit repetitions.
#' @param n_participants,trials_per_participant,n_items Synthetic design sizes.
#' @param include_items Whether crossed item effects are included.
#' @param random_slope Whether a participant condition slope is generated and
#'   fitted.
#' @param seed First simulation seed.
#' @param chains,iter,warmup,cores,adapt_delta,max_treedepth,refresh Restricted
#'   sampling controls.
#' @param interval_probability Central posterior interval probability.
#' @param minimum_repetitions Repetitions required before an overall pass is
#'   possible.
#' @param maximum_standardized_bias Maximum absolute bias divided by the
#'   empirical standard deviation of estimates for a pass.
#' @param minimum_coverage Minimum empirical interval coverage for a pass.
#' @param minimum_diagnostic_pass_fraction Minimum fraction of fits with a
#'   diagnostic pass.
#' @param continue_on_error Whether failed repetitions are recorded instead of
#'   stopping.
#'
#' @return A `gp3bayes_binary_recovery` object.
#'
#' @details
#' A small recovery run is a smoke test, not validation. Even when all declared
#' thresholds pass, the object records no automatic validation claim.
#'
#' @export
run_binary_recovery <- function(
  repetitions = 20L,
  n_participants = 30L,
  trials_per_participant = 16L,
  n_items = 12L,
  include_items = TRUE,
  random_slope = TRUE,
  seed = 1001L,
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

  result_rows <- list()
  fit_rows <- list()
  result_index <- 1L

  for (
    repetition in seq_len(repetitions)
  ) {
    repetition_seed <-
      seed + repetition - 1L

    result <- tryCatch(
      {
        simulation <-
          simulate_hierarchical_binary_data(
            n_participants =
              n_participants,
            trials_per_participant =
              trials_per_participant,
            n_items = n_items,
            include_items =
              include_items,
            random_slope_sd =
              if (random_slope) {
                0.3
              } else {
                0
              },
            seed = repetition_seed
          )

        contract <- create_model_contract(
          family = "binary",
          outcome_col = "selected",
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
            random_slope
        )

        prepared <-
          prepare_hierarchical_binary_data(
            simulation$data,
            contract,
            condition_levels = c(
              "control",
              "treatment"
            )
          )

        specification <-
          specify_binary_model(
            prepared,
            baseline =
              simulation$truth$
                baseline_probability
          )

        fitted <- fit_binary_model(
          specification,
          chains = chains,
          iter = iter,
          warmup = warmup,
          cores = cores,
          seed =
            repetition_seed + 100000L,
          adapt_delta = adapt_delta,
          max_treedepth =
            max_treedepth,
          refresh = refresh
        )

        diagnostics <-
          diagnose_binary_fit(
            fitted
          )
        posterior <-
          summarise_binary_posterior(
            fitted,
            probability =
              interval_probability
          )$table

        truth_table <-
          .gp3b_binary_truth_table(
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
      result_rows[[result_index]] <-
        result$estimates
      result_index <- result_index + 1L
    }
  }

  fits <- do.call(
    rbind,
    fit_rows
  )

  estimates <- if (
    length(result_rows) > 0L
  ) {
    do.call(
      rbind,
      result_rows
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
    rows <- lapply(
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
          truth = mean(table$truth),
          repetitions = nrow(table),
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
              repetitions = nrow(table),
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

    do.call(
      rbind,
      rows
    )
  } else {
    data.frame()
  }

  overall_status <- if (
    completed == 0L
  ) {
    "fail"
  } else if (
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
      family = "binary",
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
      "gp3bayes_binary_recovery",
      "gp3bayes_recovery"
    )
  )
}

#' Create a Structured Binary Model Report
#'
#' Writes a conservative Markdown report for an approved fitted binary model.
#'
#' @param fit A `gp3bayes_binary_fit`.
#' @param diagnostics Optional result from [diagnose_binary_fit()].
#' @param posterior_summary Optional result from
#'   [summarise_binary_posterior()].
#' @param posterior_predictive Optional result from
#'   [check_binary_posterior_predictive()].
#' @param prior_sensitivity Optional result from
#'   [assess_binary_prior_sensitivity()].
#' @param recovery Optional result from [run_binary_recovery()].
#' @param file Output Markdown file.
#' @param overwrite Whether an existing file may be replaced.
#'
#' @return A `gp3bayes_binary_model_report` containing the normalized path and
#'   section-status registry.
#'
#' @details
#' The report never converts diagnostic or predictive statuses into an
#' automatic statement that the model converged or is substantively valid.
#'
#' @export
create_binary_model_report <- function(
  fit,
  diagnostics = NULL,
  posterior_summary = NULL,
  posterior_predictive = NULL,
  prior_sensitivity = NULL,
  recovery = NULL,
  file = "gp3bayes-binary-model-report.md",
  overwrite = FALSE
) {
  .gp3b_validate_gp3bayes_fit(
    fit,
    required_class =
      "gp3bayes_binary_fit"
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
      "The report file already exists. Use `overwrite = TRUE` ",
      "to replace it."
    )
  }

  if (is.null(diagnostics)) {
    diagnostics <- diagnose_binary_fit(
      fit
    )
  }

  if (is.null(posterior_summary)) {
    posterior_summary <-
      summarise_binary_posterior(
        fit
      )
  }

  if (
    !inherits(
      diagnostics,
      "gp3bayes_binary_diagnostics"
    )
  ) {
    .gp3b_stop(
      "`diagnostics` must be a binary diagnostic result."
    )
  }

  if (
    !inherits(
      posterior_summary,
      "gp3bayes_binary_posterior_summary"
    )
  ) {
    .gp3b_stop(
      "`posterior_summary` must be a binary posterior summary."
    )
  }

  lines <- c(
    "# gp3bayes binary model report",
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
    "- Family: Bernoulli-logit",
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
      "**Interpretation boundary:** Numerical thresholds were assessed,",
      "but no automatic convergence or posterior-adequacy claim is made."
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

  if (!is.null(posterior_predictive)) {
    if (
      !inherits(
        posterior_predictive,
        "gp3bayes_binary_posterior_predictive_check"
      )
    ) {
      .gp3b_stop(
        "`posterior_predictive` must be a binary posterior ",
        "predictive check."
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
        "- Brier score: ",
        formatC(
          posterior_predictive$brier_score,
          digits = 4,
          format = "fg"
        )
      ),
      "",
      paste(
        "This status applies only to the reported predictive summaries",
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
    if (
      !inherits(
        prior_sensitivity,
        "gp3bayes_binary_prior_sensitivity"
      )
    ) {
      .gp3b_stop(
        "`prior_sensitivity` must be a binary prior-sensitivity result."
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
        "The assessment covers only the declared prior-scale",
        "multipliers and is not a universal robustness claim."
      )
    )

    registry <- rbind(
      registry,
      data.frame(
        section = "prior_sensitivity",
        status =
          prior_sensitivity$status,
        stringsAsFactors = FALSE
      )
    )
  }

  if (!is.null(recovery)) {
    if (
      !inherits(
        recovery,
        "gp3bayes_binary_recovery"
      )
    ) {
      .gp3b_stop(
        "`recovery` must be a binary recovery result."
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
        "Recovery results apply only to the declared synthetic",
        "data-generating design. No automatic validation claim is made."
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
      "The fitted associations are conditional model summaries.",
      "They are not automatically causal and do not directly measure",
      "emotion, cognition, diagnosis, deception, personality, or any",
      "other latent psychological or protected attribute."
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
      family = "binary",
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
      "gp3bayes_binary_model_report",
      "gp3bayes_model_report"
    )
  )
}

#' @export
print.gp3bayes_binary_posterior_summary <- function(
  x,
  ...
) {
  cat("<gp3bayes_binary_posterior_summary>\n")
  cat(
    "  Parameters: ",
    nrow(x$table),
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

#' @export
print.gp3bayes_posterior_predictive_check <- function(
  x,
  ...
) {
  cat("<gp3bayes_posterior_predictive_check>\n")
  cat(
    "  Family: ",
    x$family,
    "\n",
    sep = ""
  )
  cat(
    "  Threshold status: ",
    x$status,
    "\n",
    sep = ""
  )
  cat(
    "  Replicated data sets: ",
    x$draws,
    "\n",
    sep = ""
  )
  cat("  Adequacy established: FALSE\n")

  invisible(x)
}

#' @export
print.gp3bayes_prior_sensitivity <- function(
  x,
  ...
) {
  cat("<gp3bayes_prior_sensitivity>\n")
  cat(
    "  Family: ",
    x$family,
    "\n",
    sep = ""
  )
  cat(
    "  Threshold status: ",
    x$status,
    "\n",
    sep = ""
  )
  cat("  Universal robustness claim: FALSE\n")
  cat("  Posterior adequacy established: FALSE\n")

  invisible(x)
}

#' @export
print.gp3bayes_recovery <- function(
  x,
  ...
) {
  cat("<gp3bayes_recovery>\n")
  cat(
    "  Family: ",
    x$family,
    "\n",
    sep = ""
  )
  cat(
    "  Requested repetitions: ",
    x$repetitions_requested,
    "\n",
    sep = ""
  )
  cat(
    "  Completed repetitions: ",
    x$repetitions_completed,
    "\n",
    sep = ""
  )
  cat(
    "  Threshold status: ",
    x$status,
    "\n",
    sep = ""
  )
  cat("  Automatic validation claim: FALSE\n")

  invisible(x)
}

#' @export
print.gp3bayes_model_report <- function(
  x,
  ...
) {
  cat("<gp3bayes_model_report>\n")
  cat(
    "  Family: ",
    x$family,
    "\n",
    sep = ""
  )
  cat(
    "  File: ",
    x$file,
    "\n",
    sep = ""
  )
  cat("  Report created: TRUE\n")
  cat("  Automatic convergence claim: FALSE\n")
  cat("  Posterior adequacy established: FALSE\n")

  invisible(x)
}
