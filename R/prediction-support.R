# Prediction, calibration, scoring, and support diagnostics

.gp3pr_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .gp3p_stop("`", name, "` must be TRUE or FALSE.")
  }
  isTRUE(x)
}

.gp3pr_ndraws <- function(ndraws) {
  if (is.null(ndraws)) return(NULL)
  if (!is.numeric(ndraws) || length(ndraws) != 1L || is.na(ndraws) ||
      !is.finite(ndraws) || ndraws < 1 || ndraws != floor(ndraws)) {
    .gp3p_stop("`ndraws` must be NULL or one positive integer.")
  }
  as.integer(ndraws)
}

.gp3pr_seed <- function(seed) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed != floor(seed)) {
    .gp3p_stop("`seed` must be one non-negative integer.")
  }
  as.integer(seed)
}

.gp3pr_prediction_summary <- function(draws, probs, observed = NULL) {
  probs <- .gp3p_probs(probs)
  q <- apply(
    draws,
    2L,
    stats::quantile,
    probs = probs,
    names = FALSE
  )
  if (is.null(dim(q))) q <- matrix(q, ncol = 1L)
  out <- data.frame(
    observation = seq_len(ncol(draws)),
    predicted_mean = colMeans(draws),
    predicted_sd = apply(draws, 2L, stats::sd),
    lower = q[1L, ],
    predicted_median = q[2L, ],
    upper = q[3L, ],
    stringsAsFactors = FALSE
  )
  if (!is.null(observed)) out$observed <- observed
  out
}

.gp3pr_predict_matrix <- function(
  fit,
  newdata,
  type,
  include_group_effects,
  allow_new_levels,
  ndraws,
  seed
) {
  .gp3p_validate_fit(fit)
  .gp3p_require("brms", "generate posterior predictions")

  re_formula <- if (include_group_effects) NULL else NA
  common <- list(
    object = fit$backend_fit,
    newdata = newdata,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws
  )
  common <- common[!vapply(common, is.null, logical(1L))]

  if (identical(type, "expected")) {
    out <- .gp3p_call_supported(brms::posterior_epred, common)
  } else if (identical(type, "predictive")) {
    out <- withr::with_seed(
      seed,
      .gp3p_call_supported(brms::posterior_predict, common)
    )
  } else if (identical(type, "linear")) {
    common$transform <- FALSE
    out <- .gp3p_call_supported(brms::posterior_linpred, common)
  } else if (identical(type, "median")) {
    if (!identical(fit$family, "duration")) {
      .gp3p_stop("`type = \"median\"` is only defined for duration fits.")
    }
    common$transform <- FALSE
    out <- exp(.gp3p_call_supported(brms::posterior_linpred, common))
  } else {
    .gp3p_stop("Unsupported prediction type.")
  }

  out <- as.matrix(out)
  if (nrow(out) < 1L || ncol(out) < 1L || anyNA(out) || any(!is.finite(out))) {
    .gp3p_stop("Posterior predictions were not returned as a finite matrix.")
  }
  out
}

.gp3pr_object_parts <- function(x) {
  if (inherits(x, "gp3bayes_fit")) {
    return(list(
      data = x$specification$prepared$data,
      formula = x$specification$formula,
      contract = .gp3p_contract(x),
      family = x$family
    ))
  }
  if (inherits(x, "gp3bayes_model_specification") ||
      inherits(x, "gp3bayes_binary_model_specification") ||
      inherits(x, "gp3bayes_duration_model_specification")) {
    return(list(
      data = x$prepared$data,
      formula = x$formula,
      contract = x$contract,
      family = x$family
    ))
  }
  .gp3p_stop("`x` must be a gp3bayes fit or model specification.")
}

.gp3pr_contract_mapping <- function(contract, name) {
  if (!is.null(contract$mappings) && name %in% names(contract$mappings)) {
    return(contract$mappings[[name]])
  }
  legacy <- paste0(name, "_col")
  if (legacy %in% names(contract)) return(contract[[legacy]])
  NULL
}

.gp3pr_restore_type <- function(values, template) {
  if (is.factor(template)) {
    return(factor(values, levels = levels(template), ordered = is.ordered(template)))
  }
  if (is.integer(template)) return(as.integer(values))
  if (is.logical(template)) return(as.logical(values))
  if (is.character(template)) return(as.character(values))
  as.numeric(values)
}

#' Create a Governed Prediction Grid
#'
#' Creates a Cartesian prediction grid from declared model predictors. Numeric
#' covariates are held at an observed typical value unless values are supplied
#' explicitly through `at`.
#'
#' @param x A gp3bayes fit or approved model specification.
#' @param variables Optional variables to vary. By default the declared
#'   condition and non-identifier predictors are considered.
#' @param at Named list of explicit values for selected variables.
#' @param numeric_at One of `"median"` or `"mean"` for numeric covariates not
#'   supplied in `at`.
#' @param max_rows Maximum permitted grid size.
#'
#' @return A data frame suitable for `predict_model()`.
#' @export
create_prediction_grid <- function(
  x,
  variables = NULL,
  at = list(),
  numeric_at = c("median", "mean"),
  max_rows = 5000L
) {
  parts <- .gp3pr_object_parts(x)
  numeric_at <- match.arg(numeric_at)
  data <- parts$data
  contract <- parts$contract

  if (!is.list(at) || (length(at) && is.null(names(at)))) {
    .gp3p_stop("`at` must be a named list.")
  }
  if (!is.numeric(max_rows) || length(max_rows) != 1L || is.na(max_rows) ||
      max_rows < 1 || max_rows != floor(max_rows)) {
    .gp3p_stop("`max_rows` must be one positive integer.")
  }

  outcome <- .gp3pr_contract_mapping(contract, "outcome")
  group_ids <- Filter(
    Negate(is.null),
    lapply(c("participant", "item", "trial", "time"), function(z) {
      .gp3pr_contract_mapping(contract, z)
    })
  )
  group_ids <- unlist(group_ids, use.names = FALSE)

  declared <- unique(c(
    .gp3pr_contract_mapping(contract, "condition"),
    contract$predictors,
    setdiff(all.vars(parts$formula), c(outcome, group_ids))
  ))
  declared <- declared[declared %in% names(data)]

  if (is.null(variables)) variables <- declared
  if (!is.character(variables) || any(!variables %in% names(data))) {
    .gp3p_stop("`variables` must identify columns in the prepared model data.")
  }
  variables <- unique(variables)

  unknown_at <- setdiff(names(at), names(data))
  if (length(unknown_at)) {
    .gp3p_stop("Unknown `at` variables: ", paste(unknown_at, collapse = ", "), ".")
  }

  value_list <- lapply(variables, function(v) {
    template <- data[[v]]
    if (v %in% names(at)) {
      values <- at[[v]]
    } else if (is.factor(template)) {
      values <- levels(template)
    } else if (is.character(template)) {
      values <- unique(template)
    } else if (is.logical(template)) {
      values <- unique(template)
    } else if (is.numeric(template)) {
      values <- if (identical(numeric_at, "median")) {
        stats::median(template, na.rm = TRUE)
      } else {
        mean(template, na.rm = TRUE)
      }
    } else {
      .gp3p_stop("Unsupported predictor type for `", v, "`.")
    }
    if (!length(values)) .gp3p_stop("No prediction values available for `", v, "`.")
    .gp3pr_restore_type(values, template)
  })
  names(value_list) <- variables

  sizes <- vapply(value_list, length, integer(1L))
  grid_n <- prod(sizes)
  if (!is.finite(grid_n) || grid_n > max_rows) {
    .gp3p_stop(
      "Prediction grid would contain ", format(grid_n, scientific = FALSE),
      " rows; reduce `variables`/`at` or increase `max_rows` explicitly."
    )
  }

  grid <- do.call(
    expand.grid,
    c(value_list, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  )

  # Add fixed values required by the fitted formula.
  required <- setdiff(all.vars(parts$formula), outcome)
  missing_required <- setdiff(required, names(grid))
  for (v in missing_required) {
    if (!v %in% names(data)) next
    template <- data[[v]]
    value <- if (v %in% names(at)) {
      at[[v]][[1L]]
    } else if (is.factor(template)) {
      levels(template)[[1L]]
    } else if (is.character(template)) {
      unique(template)[[1L]]
    } else if (is.logical(template)) {
      unique(template)[[1L]]
    } else if (is.numeric(template)) {
      if (identical(numeric_at, "median")) {
        stats::median(template, na.rm = TRUE)
      } else {
        mean(template, na.rm = TRUE)
      }
    } else {
      template[[1L]]
    }
    grid[[v]] <- .gp3pr_restore_type(rep(value, nrow(grid)), template)
  }

  rownames(grid) <- NULL
  grid
}

#' Audit Prediction Support
#'
#' Compares requested prediction rows with the observed model-building support.
#' It reports extrapolation and novel levels but never removes prediction rows.
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param newdata Data to audit.
#'
#' @return A `gp3bayes_prediction_support` object.
#' @export
audit_prediction_support <- function(fit, newdata) {
  .gp3p_validate_fit(fit)
  if (!is.data.frame(newdata) || nrow(newdata) < 1L) {
    .gp3p_stop("`newdata` must be a non-empty data frame.")
  }
  train <- .gp3p_training_data(fit)
  outcome <- .gp3p_outcome_col(fit)
  required <- setdiff(all.vars(fit$specification$formula), outcome)

  rows <- list()
  k <- 0L
  for (v in required) {
    k <- k + 1L
    if (!v %in% names(newdata)) {
      rows[[k]] <- data.frame(
        variable = v,
        type = "missing",
        training_min = NA_real_,
        training_max = NA_real_,
        outside_support = nrow(newdata),
        novel_levels = NA_integer_,
        missing_values = NA_integer_,
        detail = "required variable absent from newdata",
        stringsAsFactors = FALSE
      )
      next
    }
    tr <- train[[v]]
    nw <- newdata[[v]]
    if (is.numeric(tr)) {
      lo <- min(tr, na.rm = TRUE)
      hi <- max(tr, na.rm = TRUE)
      outside <- sum(!is.na(nw) & (nw < lo | nw > hi))
      rows[[k]] <- data.frame(
        variable = v,
        type = "numeric",
        training_min = lo,
        training_max = hi,
        outside_support = outside,
        novel_levels = NA_integer_,
        missing_values = sum(is.na(nw)),
        detail = if (outside) "values extend beyond observed range" else "within observed range",
        stringsAsFactors = FALSE
      )
    } else {
      tr_levels <- unique(as.character(tr))
      nw_levels <- unique(as.character(nw[!is.na(nw)]))
      novel <- setdiff(nw_levels, tr_levels)
      rows[[k]] <- data.frame(
        variable = v,
        type = "categorical",
        training_min = NA_real_,
        training_max = NA_real_,
        outside_support = 0L,
        novel_levels = length(novel),
        missing_values = sum(is.na(nw)),
        detail = if (length(novel)) {
          paste("novel:", paste(novel, collapse = ", "))
        } else {
          "all levels observed in training data"
        },
        stringsAsFactors = FALSE
      )
    }
  }

  table <- if (length(rows)) do.call(rbind, rows) else data.frame()
  structure(
    list(
      table = table,
      rows = nrow(newdata),
      has_extrapolation = if (nrow(table)) any(table$outside_support > 0) else FALSE,
      has_novel_levels = if (nrow(table)) {
        any(table$novel_levels > 0, na.rm = TRUE)
      } else FALSE,
      has_missing_required = if (nrow(table)) any(table$type == "missing") else FALSE,
      automatic_rejection = FALSE,
      interpretation = paste(
        "Support flags identify extrapolation or novel factor levels.",
        "Rows are not excluded automatically."
      )
    ),
    class = "gp3bayes_prediction_support"
  )
}

#' @export
print.gp3bayes_prediction_support <- function(x, ...) {
  cat("\ngp3bayes prediction-support audit\n")
  cat(" Rows: ", x$rows, "\n", sep = "")
  cat(" Extrapolation: ", x$has_extrapolation, "\n", sep = "")
  cat(" Novel levels: ", x$has_novel_levels, "\n", sep = "")
  cat(" Missing required variables: ", x$has_missing_required, "\n", sep = "")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_prediction_support <- function(x, ...) x$table

#' Prediction-Support Table
#'
#' @param x A `gp3bayes_prediction_support` object.
#'
#' @return The underlying support audit table.
#' @export
prediction_support_table <- function(x) {
  if (!inherits(x, "gp3bayes_prediction_support")) {
    .gp3p_stop("`x` must be a gp3bayes prediction-support audit.")
  }
  x$table
}

#' Posterior Prediction for Approved gp3bayes Models
#'
#' Distinguishes conditional expectations, new-outcome posterior predictions,
#' linear-predictor draws, and the conditional median for lognormal duration
#' models.
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param newdata Optional data frame. `NULL` uses the fitted prepared data.
#' @param type Prediction quantity: `"expected"`, `"predictive"`, `"linear"`,
#'   or `"median"`.
#' @param include_group_effects Whether fitted group-level effects are included.
#' @param allow_new_levels Whether new grouping levels are permitted by brms.
#' @param ndraws Optional number of posterior draws.
#' @param probs Three probabilities used to summarise predictions.
#' @param seed Non-negative seed used for posterior predictive simulation.
#'
#' @return A `gp3bayes_prediction` containing draws, summaries, prediction data,
#'   and interpretation metadata.
#' @export
predict_model <- function(
  fit,
  newdata = NULL,
  type = c("expected", "predictive", "linear", "median"),
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL,
  probs = c(0.025, 0.5, 0.975),
  seed = 1L
) {
  .gp3p_validate_fit(fit)
  type <- match.arg(type)
  include_group_effects <- .gp3pr_flag(include_group_effects, "include_group_effects")
  allow_new_levels <- .gp3pr_flag(allow_new_levels, "allow_new_levels")
  ndraws <- .gp3pr_ndraws(ndraws)
  seed <- .gp3pr_seed(seed)
  probs <- .gp3p_probs(probs)

  if (identical(type, "median") && !identical(fit$family, "duration")) {
    .gp3p_stop("`type = \"median\"` is available only for duration models.")
  }

  prediction_data <- if (is.null(newdata)) .gp3p_training_data(fit) else newdata
  if (!is.data.frame(prediction_data) || nrow(prediction_data) < 1L) {
    .gp3p_stop("Prediction data must be a non-empty data frame.")
  }

  support <- audit_prediction_support(fit, prediction_data)
  outcome <- .gp3p_outcome_col(fit)
  observed <- if (outcome %in% names(prediction_data)) {
    prediction_data[[outcome]]
  } else {
    NULL
  }

  draws <- .gp3pr_predict_matrix(
    fit = fit,
    newdata = prediction_data,
    type = type,
    include_group_effects = include_group_effects,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws,
    seed = seed
  )
  summary <- .gp3pr_prediction_summary(draws, probs, observed)

  structure(
    list(
      family = fit$family,
      type = type,
      scale = if (type == "linear") {
        if (fit$family == "binary") "log_odds" else "log_duration_location"
      } else if (type == "median") {
        "duration_median"
      } else {
        "response"
      },
      draws = draws,
      summary = summary,
      newdata = prediction_data,
      observed = observed,
      support = support,
      include_group_effects = include_group_effects,
      allow_new_levels = allow_new_levels,
      probs = probs,
      seed = seed,
      interpretation = paste(
        "Predictions are descriptive posterior quantities under the fitted model.",
        "They do not establish causal effects or out-of-sample adequacy."
      )
    ),
    class = "gp3bayes_prediction"
  )
}

#' @export
print.gp3bayes_prediction <- function(x, ...) {
  cat("\ngp3bayes posterior prediction\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Type: ", x$type, "\n", sep = "")
  cat(" Rows: ", nrow(x$summary), "\n", sep = "")
  cat(" Draws: ", nrow(x$draws), "\n", sep = "")
  cat(" Include group effects: ", x$include_group_effects, "\n", sep = "")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_prediction <- function(x, ...) x$summary

#' Prediction Summary Table
#'
#' @param x A `gp3bayes_prediction`.
#'
#' @return The observation-level posterior prediction summary.
#' @export
prediction_table <- function(x) {
  if (!inherits(x, "gp3bayes_prediction")) {
    .gp3p_stop("`x` must be a gp3bayes prediction.")
  }
  x$summary
}

#' Extract Expected Posterior Predictions
#'
#' @inheritParams predict_model
#'
#' @return A numeric matrix of conditional expected-response draws.
#' @export
extract_expected_predictions <- function(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL
) {
  predict_model(
    fit,
    newdata = newdata,
    type = "expected",
    include_group_effects = include_group_effects,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws
  )$draws
}

#' Extract Posterior Predictive Draws
#'
#' @inheritParams predict_model
#'
#' @return A numeric matrix of new-outcome posterior predictive draws.
#' @export
extract_posterior_predictions <- function(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL,
  seed = 1L
) {
  predict_model(
    fit,
    newdata = newdata,
    type = "predictive",
    include_group_effects = include_group_effects,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws,
    seed = seed
  )$draws
}

#' Extract Linear-Predictor Draws
#'
#' @inheritParams predict_model
#'
#' @return A numeric matrix on the model linear-predictor scale.
#' @export
extract_linear_predictions <- function(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL
) {
  predict_model(
    fit,
    newdata = newdata,
    type = "linear",
    include_group_effects = include_group_effects,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws
  )$draws
}

#' Binary Event-Probability Predictions
#'
#' @inheritParams predict_model
#'
#' @return A `gp3bayes_prediction` of event probabilities.
#' @export
predict_binary_probability <- function(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL,
  probs = c(0.025, 0.5, 0.975)
) {
  .gp3p_validate_fit(fit, "binary")
  predict_model(
    fit,
    newdata = newdata,
    type = "expected",
    include_group_effects = include_group_effects,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws,
    probs = probs
  )
}

#' Duration Predictions
#'
#' @inheritParams predict_model
#' @param type One of `"median"`, `"expected"`, or `"predictive"`.
#'
#' @return A `gp3bayes_prediction` on the recorded duration scale.
#' @export
predict_duration <- function(
  fit,
  newdata = NULL,
  type = c("median", "expected", "predictive"),
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL,
  probs = c(0.025, 0.5, 0.975),
  seed = 1L
) {
  .gp3p_validate_fit(fit, "duration")
  type <- match.arg(type)
  predict_model(
    fit,
    newdata = newdata,
    type = type,
    include_group_effects = include_group_effects,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws,
    probs = probs,
    seed = seed
  )
}

#' Posterior Prediction Contrast
#'
#' @param x A `gp3bayes_prediction`.
#' @param row1,row2 Two prediction rows to compare.
#' @param measure `"difference"`, `"ratio"`, or `"odds_ratio"`.
#' @param probs Posterior interval probabilities.
#'
#' @return A one-row posterior contrast summary.
#' @export
prediction_contrast <- function(
  x,
  row1,
  row2,
  measure = c("difference", "ratio", "odds_ratio"),
  probs = c(0.025, 0.5, 0.975)
) {
  if (!inherits(x, "gp3bayes_prediction")) {
    .gp3p_stop("`x` must be a gp3bayes prediction.")
  }
  measure <- match.arg(measure)
  if (length(row1) != 1L || length(row2) != 1L) {
    .gp3p_stop("`row1` and `row2` must each identify one prediction row.")
  }
  rows <- c(row1, row2)
  if (!is.numeric(rows) || anyNA(rows) || any(!is.finite(rows)) ||
      any(rows != floor(rows)) || any(rows < 1 | rows > ncol(x$draws))) {
    .gp3p_stop("`row1` and `row2` must identify prediction rows.")
  }
  a <- x$draws[, as.integer(row1)]
  b <- x$draws[, as.integer(row2)]

  value <- if (measure == "difference") {
    b - a
  } else if (measure == "ratio") {
    if (any(a <= 0)) .gp3p_stop("Ratio contrasts require positive denominator draws.")
    b / a
  } else {
    if (!identical(x$family, "binary") || !identical(x$type, "expected")) {
      .gp3p_stop("Odds-ratio contrasts require binary expected probabilities.")
    }
    eps <- sqrt(.Machine$double.eps)
    a <- pmin(pmax(a, eps), 1 - eps)
    b <- pmin(pmax(b, eps), 1 - eps)
    (b / (1 - b)) / (a / (1 - a))
  }

  q <- stats::quantile(value, probs = .gp3p_probs(probs), names = FALSE)
  data.frame(
    row1 = as.integer(row1),
    row2 = as.integer(row2),
    measure = measure,
    mean = mean(value),
    lower = q[[1L]],
    median = q[[2L]],
    upper = q[[3L]],
    probability_gt_reference = mean(value > if (measure == "difference") 0 else 1),
    automatic_decision = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Posterior Exceedance Probabilities
#'
#' @param x A `gp3bayes_prediction`.
#' @param threshold Finite response-scale threshold.
#' @param direction Whether to evaluate values above or below the threshold.
#'
#' @return Observation-level posterior exceedance probabilities.
#' @export
prediction_exceedance_probability <- function(
  x,
  threshold,
  direction = c("above", "below")
) {
  if (!inherits(x, "gp3bayes_prediction")) {
    .gp3p_stop("`x` must be a gp3bayes prediction.")
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      !is.finite(threshold)) {
    .gp3p_stop("`threshold` must be one finite number.")
  }
  direction <- match.arg(direction)
  p <- if (direction == "above") {
    colMeans(x$draws > threshold)
  } else {
    colMeans(x$draws < threshold)
  }
  data.frame(
    observation = seq_along(p),
    threshold = threshold,
    direction = direction,
    probability = p,
    automatic_decision = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Decompose Prediction Uncertainty Descriptively
#'
#' Separates variability in conditional expected-response draws from total
#' posterior predictive variability. The difference is a descriptive Monte
#' Carlo decomposition and is not a causal variance decomposition.
#'
#' @inheritParams predict_model
#'
#' @return A `gp3bayes_prediction_uncertainty` object.
#' @export
prediction_uncertainty_decomposition <- function(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = 1000L,
  seed = 1L
) {
  expected <- predict_model(
    fit,
    newdata = newdata,
    type = "expected",
    include_group_effects = include_group_effects,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws
  )
  predictive <- predict_model(
    fit,
    newdata = expected$newdata,
    type = "predictive",
    include_group_effects = include_group_effects,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws,
    seed = seed
  )

  epistemic <- apply(expected$draws, 2L, stats::var)
  total <- apply(predictive$draws, 2L, stats::var)
  residual <- pmax(total - epistemic, 0)
  table <- data.frame(
    observation = seq_along(total),
    expected_response_variance = epistemic,
    total_predictive_variance = total,
    residual_component = residual,
    expected_fraction = ifelse(total > 0, epistemic / total, NA_real_),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      table = table,
      expected = expected,
      predictive = predictive,
      interpretation = paste(
        "Components are descriptive posterior Monte Carlo variances.",
        "They are not a causal variance decomposition."
      )
    ),
    class = "gp3bayes_prediction_uncertainty"
  )
}

#' @export
print.gp3bayes_prediction_uncertainty <- function(x, ...) {
  cat("\ngp3bayes prediction-uncertainty decomposition\n")
  cat(" Rows: ", nrow(x$table), "\n", sep = "")
  cat(" ", x$interpretation, "\n", sep = "")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_prediction_uncertainty <- function(x, ...) x$table

#' Grouped Posterior Predictive Check
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param group Name of a grouping column in the prepared model data.
#' @param ndraws Number of posterior predictive draws.
#' @param probs Posterior interval probabilities.
#' @param seed Non-negative seed.
#'
#' @return A `gp3bayes_group_prediction_check`.
#' @export
grouped_prediction_check <- function(
  fit,
  group,
  ndraws = 1000L,
  probs = c(0.025, 0.5, 0.975),
  seed = 1L
) {
  .gp3p_validate_fit(fit)
  data <- .gp3p_training_data(fit)
  outcome <- .gp3p_outcome_col(fit)
  if (!is.character(group) || length(group) != 1L || !group %in% names(data)) {
    .gp3p_stop("`group` must name one column in the prepared model data.")
  }
  pred <- predict_model(
    fit,
    type = "predictive",
    include_group_effects = TRUE,
    ndraws = ndraws,
    probs = probs,
    seed = seed
  )
  split_index <- split(seq_len(nrow(data)), as.character(data[[group]]))
  group_draws <- vapply(
    split_index,
    function(idx) rowMeans(pred$draws[, idx, drop = FALSE]),
    numeric(nrow(pred$draws))
  )
  if (is.null(dim(group_draws))) {
    group_draws <- matrix(group_draws, ncol = 1L)
    colnames(group_draws) <- names(split_index)
  }
  q <- apply(
    group_draws,
    2L,
    stats::quantile,
    probs = .gp3p_probs(probs),
    names = FALSE
  )
  if (is.null(dim(q))) q <- matrix(q, ncol = 1L)
  observed <- vapply(
    split_index,
    function(idx) mean(data[[outcome]][idx]),
    numeric(1L)
  )
  table <- data.frame(
    group = names(split_index),
    n = vapply(split_index, length, integer(1L)),
    observed = unname(observed),
    predicted_mean = colMeans(group_draws),
    lower = q[1L, ],
    predicted_median = q[2L, ],
    upper = q[3L, ],
    stringsAsFactors = FALSE
  )
  structure(
    list(
      family = fit$family,
      group_column = group,
      table = table,
      draws = group_draws,
      automatic_exclusion = FALSE,
      interpretation = paste(
        "Observed group summaries are compared with posterior predictive summaries.",
        "Large discrepancies request review; groups are not excluded automatically."
      )
    ),
    class = "gp3bayes_group_prediction_check"
  )
}

#' @export
print.gp3bayes_group_prediction_check <- function(x, ...) {
  cat("\ngp3bayes grouped posterior predictive check\n")
  cat(" Group column: ", x$group_column, "\n", sep = "")
  cat(" Groups: ", nrow(x$table), "\n", sep = "")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_group_prediction_check <- function(x, ...) x$table

#' Posterior Predictive Residuals
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param type Residual type. Binary models support `"raw"` and `"pearson"`;
#'   duration models support `"raw"`, `"log"`, and `"relative"`.
#' @param ndraws Optional number of expected-response draws.
#'
#' @return A data frame of observed values, posterior expected values, and
#'   descriptive residuals.
#' @export
predictive_residuals <- function(
  fit,
  type = NULL,
  ndraws = 1000L
) {
  .gp3p_validate_fit(fit)
  type <- if (fit$family == "binary") {
    if (is.null(type)) "raw" else match.arg(type, c("raw", "pearson"))
  } else {
    if (is.null(type)) "log" else match.arg(type, c("raw", "log", "relative"))
  }
  pred <- predict_model(
    fit,
    type = "expected",
    include_group_effects = TRUE,
    ndraws = ndraws
  )
  y <- pred$observed
  if (is.null(y)) .gp3p_stop("Observed outcomes are unavailable.")
  mu <- pred$summary$predicted_mean

  residual <- if (type == "raw") {
    y - mu
  } else if (type == "pearson") {
    denom <- sqrt(pmax(mu * (1 - mu), .Machine$double.eps))
    (y - mu) / denom
  } else if (type == "log") {
    log(y) - log(mu)
  } else {
    (y - mu) / mu
  }

  data.frame(
    observation = seq_along(y),
    observed = y,
    expected = mu,
    residual = residual,
    type = type,
    stringsAsFactors = FALSE
  )
}

.gp3pr_prediction_inputs <- function(x, observed = NULL) {
  if (inherits(x, "gp3bayes_prediction")) {
    if (is.null(x$observed)) .gp3p_stop("The prediction object has no observed outcome.")
    return(list(
      probability_or_value = x$summary$predicted_mean,
      observed = x$observed,
      draws = x$draws
    ))
  }
  if (!is.numeric(x) || is.null(observed) || !is.numeric(observed) ||
      length(x) != length(observed)) {
    .gp3p_stop(
      "Supply a gp3bayes prediction, or numeric predictions plus `observed`."
    )
  }
  list(probability_or_value = as.numeric(x), observed = observed, draws = NULL)
}

#' Binary Prediction Scores
#'
#' @param x A binary expected-response `gp3bayes_prediction` or numeric event
#'   probabilities.
#' @param observed Optional binary outcomes when `x` is numeric.
#' @param threshold Classification threshold used only for threshold summaries.
#' @param epsilon Probability truncation used for finite log loss.
#'
#' @return A one-row data frame of descriptive predictive scores.
#' @examples
#' binary_prediction_scores(c(0.1, 0.8, 0.7, 0.2), c(0, 1, 1, 0))
#' @export
binary_prediction_scores <- function(
  x,
  observed = NULL,
  threshold = 0.5,
  epsilon = 1e-12
) {
  z <- .gp3pr_prediction_inputs(x, observed)
  p <- z$probability_or_value
  y <- z$observed
  if (any(!y %in% c(0, 1)) || anyNA(y) ||
      any(!is.finite(p)) || any(p < 0 | p > 1)) {
    .gp3p_stop("Binary scores require outcomes in {0,1} and probabilities in [0,1].")
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || !is.finite(threshold) ||
      threshold < 0 || threshold > 1) {
    .gp3p_stop("`threshold` must lie in [0, 1].")
  }
  if (!is.numeric(epsilon) || length(epsilon) != 1L || is.na(epsilon) ||
      !is.finite(epsilon) || epsilon <= 0 || epsilon >= 0.5) {
    .gp3p_stop("`epsilon` must be one finite number strictly inside (0, 0.5).")
  }

  pc <- pmin(pmax(p, epsilon), 1 - epsilon)
  predicted <- as.integer(p >= threshold)
  tp <- sum(predicted == 1 & y == 1)
  tn <- sum(predicted == 0 & y == 0)
  fp <- sum(predicted == 1 & y == 0)
  fn <- sum(predicted == 0 & y == 1)
  sensitivity <- if (tp + fn > 0) tp / (tp + fn) else NA_real_
  specificity <- if (tn + fp > 0) tn / (tn + fp) else NA_real_

  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  auc <- if (n1 > 0 && n0 > 0) {
    r <- rank(p, ties.method = "average")
    (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  } else {
    NA_real_
  }

  data.frame(
    n = length(y),
    brier = mean((p - y)^2),
    log_loss = -mean(y * log(pc) + (1 - y) * log(1 - pc)),
    auc = auc,
    threshold = threshold,
    accuracy = mean(predicted == y),
    sensitivity = sensitivity,
    specificity = specificity,
    balanced_accuracy = mean(c(sensitivity, specificity), na.rm = TRUE),
    automatic_decision = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Binary Threshold-Metric Curve
#'
#' @param x A binary expected-response prediction or numeric probabilities.
#' @param observed Optional observed binary outcomes.
#' @param thresholds Numeric thresholds between 0 and 1, inclusive.
#'
#' @return A data frame with accuracy, sensitivity, specificity, and balanced
#'   accuracy over the supplied thresholds.
#' @examples
#' binary_threshold_metrics(
#'   c(0.1, 0.8, 0.7, 0.2),
#'   c(0, 1, 1, 0),
#'   thresholds = c(0.3, 0.5, 0.7)
#' )
#' @export
binary_threshold_metrics <- function(
  x,
  observed = NULL,
  thresholds = seq(0.1, 0.9, by = 0.05)
) {
  if (!is.numeric(thresholds) || !length(thresholds) || anyNA(thresholds) ||
      any(!is.finite(thresholds)) || any(thresholds < 0 | thresholds > 1)) {
    .gp3p_stop("`thresholds` must contain values in [0,1].")
  }
  rows <- lapply(
    thresholds,
    function(th) binary_prediction_scores(x, observed, threshold = th)
  )
  do.call(rbind, rows)
}

#' Binary Calibration Table
#'
#' @param x A binary expected-response `gp3bayes_prediction`.
#' @param bins Number of equal-frequency calibration bins.
#' @param probs Posterior interval probabilities.
#'
#' @return A data frame comparing observed event rates with posterior mean
#'   event probabilities by bin.
#' @export
binary_calibration_table <- function(
  x,
  bins = 10L,
  probs = c(0.025, 0.5, 0.975)
) {
  if (!inherits(x, "gp3bayes_prediction") ||
      !identical(x$family, "binary") ||
      !identical(x$type, "expected") ||
      is.null(x$observed)) {
    .gp3p_stop("`x` must be a binary expected-response prediction with outcomes.")
  }
  if (!is.numeric(bins) || length(bins) != 1L || is.na(bins) ||
      bins < 2 || bins != floor(bins)) {
    .gp3p_stop("`bins` must be one integer >= 2.")
  }
  pmean <- x$summary$predicted_mean
  breaks <- unique(stats::quantile(
    pmean,
    probs = seq(0, 1, length.out = as.integer(bins) + 1L),
    na.rm = TRUE,
    names = FALSE
  ))
  if (length(breaks) < 3L) {
    bin <- rep(1L, length(pmean))
  } else {
    bin <- cut(pmean, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  }
  idx <- split(seq_along(bin), bin)
  p3 <- .gp3p_probs(probs)
  rows <- lapply(names(idx), function(b) {
    j <- idx[[b]]
    draw_mean <- rowMeans(x$draws[, j, drop = FALSE])
    q <- stats::quantile(draw_mean, probs = p3, names = FALSE)
    data.frame(
      bin = as.integer(b),
      n = length(j),
      mean_predicted_probability = mean(pmean[j]),
      observed_rate = mean(x$observed[j]),
      posterior_lower = q[[1L]],
      posterior_median = q[[2L]],
      posterior_upper = q[[3L]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Duration Prediction Scores
#'
#' @param x A duration prediction object or positive numeric predictions.
#' @param observed Optional positive observed durations.
#'
#' @return A one-row table of absolute and squared prediction errors on the
#'   response and log scales.
#' @examples
#' duration_prediction_scores(c(100, 120, 90), c(110, 115, 100))
#' @export
duration_prediction_scores <- function(x, observed = NULL) {
  z <- .gp3pr_prediction_inputs(x, observed)
  mu <- z$probability_or_value
  y <- z$observed
  if (anyNA(mu) || anyNA(y) || any(!is.finite(mu)) || any(!is.finite(y)) ||
      any(mu <= 0) || any(y <= 0)) {
    .gp3p_stop("Duration scores require finite positive predictions and outcomes.")
  }
  err <- mu - y
  log_err <- log(mu) - log(y)
  data.frame(
    n = length(y),
    mae = mean(abs(err)),
    rmse = sqrt(mean(err^2)),
    median_absolute_error = stats::median(abs(err)),
    log_mae = mean(abs(log_err)),
    log_rmse = sqrt(mean(log_err^2)),
    mean_log_error = mean(log_err),
    automatic_decision = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Duration Quantile Calibration
#'
#' @param x A duration posterior predictive `gp3bayes_prediction`.
#' @param quantiles Predictive quantiles to assess.
#'
#' @return A table comparing nominal predictive quantiles with empirical
#'   coverage below those quantiles.
#' @export
duration_quantile_calibration <- function(
  x,
  quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9)
) {
  if (!inherits(x, "gp3bayes_prediction") ||
      !identical(x$family, "duration") ||
      !identical(x$type, "predictive") ||
      is.null(x$observed)) {
    .gp3p_stop("`x` must be a duration posterior predictive object with outcomes.")
  }
  if (!is.numeric(quantiles) || !length(quantiles) || anyNA(quantiles) ||
      any(quantiles <= 0 | quantiles >= 1)) {
    .gp3p_stop("`quantiles` must contain probabilities strictly inside (0,1).")
  }
  rows <- lapply(quantiles, function(p) {
    q_i <- apply(x$draws, 2L, stats::quantile, probs = p, names = FALSE)
    empirical <- mean(x$observed <= q_i)
    data.frame(
      nominal = p,
      empirical = empirical,
      calibration_gap = empirical - p,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Duration Probability-Integral-Transform Table
#'
#' @param x A duration posterior predictive `gp3bayes_prediction`.
#'
#' @return Observation-level empirical posterior predictive PIT values.
#' @export
duration_pit_table <- function(x) {
  if (!inherits(x, "gp3bayes_prediction") ||
      !identical(x$family, "duration") ||
      !identical(x$type, "predictive") ||
      is.null(x$observed)) {
    .gp3p_stop("`x` must be a duration posterior predictive object with outcomes.")
  }
  pit <- vapply(
    seq_along(x$observed),
    function(i) mean(x$draws[, i] <= x$observed[[i]]),
    numeric(1L)
  )
  data.frame(
    observation = seq_along(pit),
    pit = pit,
    stringsAsFactors = FALSE
  )
}

#' Posterior Predictive Coverage Table
#'
#' @param x A posterior predictive `gp3bayes_prediction`.
#' @param levels Central predictive interval levels.
#'
#' @return A table of empirical coverage and mean interval width.
#' @export
predictive_coverage_table <- function(
  x,
  levels = c(0.5, 0.8, 0.9, 0.95)
) {
  if (!inherits(x, "gp3bayes_prediction") ||
      !identical(x$type, "predictive") ||
      is.null(x$observed)) {
    .gp3p_stop("`x` must be a posterior predictive object with observed outcomes.")
  }
  if (!is.numeric(levels) || !length(levels) || anyNA(levels) ||
      any(levels <= 0 | levels >= 1)) {
    .gp3p_stop("`levels` must contain probabilities strictly inside (0,1).")
  }
  rows <- lapply(levels, function(level) {
    alpha <- (1 - level) / 2
    lo <- apply(x$draws, 2L, stats::quantile, probs = alpha, names = FALSE)
    hi <- apply(x$draws, 2L, stats::quantile, probs = 1 - alpha, names = FALSE)
    data.frame(
      nominal_coverage = level,
      empirical_coverage = mean(x$observed >= lo & x$observed <= hi),
      mean_interval_width = mean(hi - lo),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Posterior Predictive Summary Table
#'
#' @param x A posterior predictive `gp3bayes_prediction`.
#' @param probs Three summary probabilities.
#'
#' @return Observation-level predictive summaries.
#' @export
posterior_predictive_summary_table <- function(
  x,
  probs = c(0.025, 0.5, 0.975)
) {
  if (!inherits(x, "gp3bayes_prediction") || !identical(x$type, "predictive")) {
    .gp3p_stop("`x` must be a posterior predictive gp3bayes prediction.")
  }
  .gp3pr_prediction_summary(x$draws, probs, x$observed)
}
