# Governed predictive model comparison for gp3bayes 0.5.

#' Create a named set of fitted pupil models
#'
#' @param ... Fitted advanced or compatible brms-backed pupil models, or one
#'   named list of models.
#' @param predictive_target Declared target for interpreting comparison.
#' @return A `gp3bayes_pupil_model_set` object.
#' @export
create_pupil_model_set <- function(
    ...,
    predictive_target = c("new_trial_known_participant", "new_participant", "future_segment", "new_sample_known_trial")) {

  predictive_target <- match.arg(predictive_target)
  models <- list(...)
  if (length(models) == 1L && is.list(models[[1L]]) && !inherits(models[[1L]], c("gp3bayes_pupil_advanced_fit", "brmsfit"))) models <- models[[1L]]
  if (length(models) < 2L) stop("A model set requires at least two fitted models.", call. = FALSE)
  if (is.null(names(models)) || any(!nzchar(names(models)))) stop("All models must be explicitly named.", call. = FALSE)
  if (anyDuplicated(names(models))) stop("Model names must be unique.", call. = FALSE)
  invisible(lapply(models, .p05_underlying_fit))

  structure(
    list(
      models = models,
      predictive_target = predictive_target,
      automatic_winner = FALSE,
      interpretation = paste(
        "Comparison quantifies predictive performance under the declared target.",
        "gp3bayes never promotes a model automatically to a substantive or causal winner."
      )
    ),
    class = "gp3bayes_pupil_model_set"
  )
}

#' Compare fitted pupil models predictively
#'
#' @param model_set A named model set.
#' @param criterion `"loo"` or exact `"kfold"`. Leave-future-out is handled by
#'   explicit plans via [create_pupil_lfo_plan()] and [validate_pupil_leave_future_out()].
#' @param K Number of folds for exact K-fold CV.
#' @param group Optional grouping column passed to brms K-fold.
#' @param moment_match Use brms/loo moment matching for PSIS-LOO where supported.
#' @param save_psis Save PSIS objects.
#' @return A `gp3bayes_pupil_model_comparison` object.
#' @export
compare_pupil_models <- function(
    model_set,
    criterion = c("loo", "kfold"),
    K = 10L,
    group = NULL,
    moment_match = FALSE,
    save_psis = TRUE) {

  if (!inherits(model_set, "gp3bayes_pupil_model_set")) stop("`model_set` must come from create_pupil_model_set().", call. = FALSE)
  criterion <- match.arg(criterion)
  .p05_assert_flag(moment_match, "moment_match")
  .p05_assert_flag(save_psis, "save_psis")
  .p05_require("loo", "for predictive model comparison")
  .p05_require("brms", "for brms model comparison methods")

  bfits <- lapply(model_set$models, .p05_underlying_fit)

  results <- if (criterion == "loo") {
    lapply(
      bfits,
      function(f) brms::loo(f, moment_match = moment_match, save_psis = save_psis)
    )
  } else {
    K <- .p05_assert_integerish(K, "K", 2L, 20L)
    lapply(
      bfits,
      function(f) brms::kfold(f, K = K, group = group, save_fits = FALSE)
    )
  }
  names(results) <- names(model_set$models)

  comparison <- do.call(loo::loo_compare, results)
  tab <- as.data.frame(comparison)
  tab$model <- rownames(tab)
  rownames(tab) <- NULL
  tab <- tab[, c("model", setdiff(names(tab), "model")), drop = FALSE]

  structure(
    list(
      criterion = criterion,
      predictive_target = model_set$predictive_target,
      criteria = results,
      comparison = comparison,
      table = tab,
      model_set = model_set,
      interpretation = paste(
        "Higher expected predictive performance is not an automatic adequacy or substantive-selection claim.",
        "Inspect uncertainty, Pareto diagnostics or fold stability, scientific interpretability, and the declared predictive target."
      )
    ),
    class = "gp3bayes_pupil_model_comparison"
  )
}

#' Tabulate model comparison
#' @param x A model-comparison object.
#' @return A data frame.
#' @export
pupil_model_comparison_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_model_comparison")) stop("Expected a pupil model comparison.", call. = FALSE)
  x$table
}

#' Compute explicit predictive model weights
#'
#' @param x A LOO-based model comparison or a model set.
#' @param method `"stacking"` or `"pseudobma"`.
#' @param BB Bayesian bootstrap for pseudo-BMA where applicable.
#' @return A data frame of weights. Weights are not used automatically for
#'   prediction or model selection.
#' @export
pupil_model_weights <- function(x, method = c("stacking", "pseudobma"), BB = TRUE) {
  method <- match.arg(method)
  .p05_assert_flag(BB, "BB")
  .p05_require("loo", "for model weights")

  if (inherits(x, "gp3bayes_pupil_model_set")) {
    x <- compare_pupil_models(x, criterion = "loo")
  }
  if (!inherits(x, "gp3bayes_pupil_model_comparison") || x$criterion != "loo") {
    stop("Model weights currently require a LOO-based pupil model comparison.", call. = FALSE)
  }

  w <- loo::loo_model_weights(x$criteria, method = method, BB = BB)
  data.frame(
    model = names(w),
    weight = as.numeric(w),
    method = method,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Create an explicit leave-future-out validation plan
#'
#' The plan defines sequential training cut-points within a single ordered
#' series. Execution is deliberately separate because it requires refitting.
#'
#' @param fit An advanced fitted model.
#' @param initial_fraction Initial fraction of each series available for the
#'   earliest training set.
#' @param horizon Number of future samples scored at each refit.
#' @param step Number of samples by which the training cut moves.
#' @param max_refits Maximum refits per model.
#' @return A `gp3bayes_pupil_lfo_plan` object.
#' @export
create_pupil_lfo_plan <- function(
    fit,
    initial_fraction = 0.6,
    horizon = 5L,
    step = 5L,
    max_refits = 8L) {

  if (!inherits(fit, "gp3bayes_pupil_advanced_fit")) stop("LFO plans currently require a gp3bayes 0.5 advanced fit.", call. = FALSE)
  .p05_assert_probability(initial_fraction, "initial_fraction")
  horizon <- .p05_assert_integerish(horizon, "horizon", 1L, 100000L)
  step <- .p05_assert_integerish(step, "step", 1L, 100000L)
  max_refits <- .p05_assert_integerish(max_refits, "max_refits", 1L, 20L)

  data <- fit$translation$data
  # LFO is defined on globally ordered normalized within-trial sample index. We
  # score all series at the same relative cut, preserving information-flow direction.
  max_index <- max(data$.gp3bayes_time_index)
  start <- max(2L, floor(initial_fraction * max_index))
  cuts <- seq.int(start, max_index - horizon, by = step)
  if (!length(cuts)) stop("No valid LFO cut-points remain for the requested horizon.", call. = FALSE)
  if (length(cuts) > max_refits) {
    cuts <- unique(round(seq(min(cuts), max(cuts), length.out = max_refits)))
  }
  tab <- data.frame(
    refit = seq_along(cuts),
    train_through_index = cuts,
    test_from_index = cuts + 1L,
    test_through_index = pmin(cuts + horizon, max_index),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      table = tab,
      initial_fraction = initial_fraction,
      horizon = horizon,
      step = step,
      max_refits = max_refits,
      original_head = fit,
      execute_default = FALSE,
      interpretation = paste(
        "LFO restricts information flow to earlier time points.",
        "This plan evaluates within-series future prediction and does not by itself assess generalization to new participants."
      )
    ),
    class = "gp3bayes_pupil_lfo_plan"
  )
}

#' Execute or materialize leave-future-out validation
#'
#' @param fit An advanced fitted model.
#' @param plan An LFO plan.
#' @param execute If FALSE, returns the plan without refitting. TRUE performs
#'   sequential model refits and future-block log scoring.
#' @param cores Maximum cores passed to brms update; restricted to 2.
#' @param seed Base seed for refits.
#' @return A `gp3bayes_pupil_lfo_validation` object.
#' @export
validate_pupil_leave_future_out <- function(fit, plan, execute = FALSE, cores = 1L, seed = 2026) {
  if (!inherits(fit, "gp3bayes_pupil_advanced_fit")) stop("Expected an advanced fit.", call. = FALSE)
  if (!inherits(plan, "gp3bayes_pupil_lfo_plan")) stop("Expected an LFO plan.", call. = FALSE)
  .p05_assert_flag(execute, "execute")
  if (!execute) {
    return(structure(
      list(plan = plan$table, executed = FALSE, scores = NULL, interpretation = plan$interpretation),
      class = "gp3bayes_pupil_lfo_validation"
    ))
  }
  .p05_require("brms", "to execute LFO refits")
  cores <- .p05_assert_integerish(cores, "cores", 1L, 2L)

  original <- .p05_underlying_fit(fit)
  data <- fit$translation$data
  rows <- vector("list", nrow(plan$table))

  for (i in seq_len(nrow(plan$table))) {
    cut <- plan$table$train_through_index[[i]]
    lo <- plan$table$test_from_index[[i]]
    hi <- plan$table$test_through_index[[i]]
    train <- data[data$.gp3bayes_time_index <= cut, , drop = FALSE]
    test <- data[data$.gp3bayes_time_index >= lo & data$.gp3bayes_time_index <= hi, , drop = FALSE]
    if (!nrow(train) || !nrow(test)) stop("An LFO split produced an empty train/test set.", call. = FALSE)

    refit <- stats::update(
      original,
      newdata = train,
      recompile = FALSE,
      cores = cores,
      seed = seed + i,
      refresh = 0
    )
    ll <- brms::log_lik(refit, newdata = test, allow_new_levels = TRUE)
    point <- apply(ll, 2L, .p05_log_mean_exp)
    rows[[i]] <- data.frame(
      refit = i,
      train_rows = nrow(train),
      test_rows = nrow(test),
      elpd_future = sum(point),
      mean_log_score = mean(point),
      stringsAsFactors = FALSE
    )
  }

  scores <- do.call(rbind, rows)
  structure(
    list(
      plan = plan$table,
      executed = TRUE,
      scores = scores,
      total_elpd_future = sum(scores$elpd_future),
      interpretation = plan$interpretation
    ),
    class = "gp3bayes_pupil_lfo_validation"
  )
}

#' Compare executed leave-future-out validations
#'
#' @param ... Two or more named executed LFO validation objects, or one named list.
#' @return A `gp3bayes_pupil_lfo_comparison` object.
#' @export
compare_pupil_lfo <- function(...) {
  xs <- list(...)
  if (length(xs) == 1L && is.list(xs[[1L]]) && !inherits(xs[[1L]], "gp3bayes_pupil_lfo_validation")) xs <- xs[[1L]]
  if (length(xs) < 2L || is.null(names(xs)) || any(!nzchar(names(xs)))) stop("Provide at least two named LFO validation objects.", call. = FALSE)
  bad <- !vapply(xs, function(x) inherits(x, "gp3bayes_pupil_lfo_validation") && isTRUE(x$executed), logical(1L))
  if (any(bad)) stop("All LFO validations must be executed before comparison.", call. = FALSE)
  tab <- do.call(
    rbind,
    lapply(seq_along(xs), function(i) {
      s <- xs[[i]]$scores
      data.frame(
        model = names(xs)[[i]],
        total_elpd_future = sum(s$elpd_future),
        mean_log_score = stats::weighted.mean(s$mean_log_score, s$test_rows),
        refits = nrow(s),
        future_rows = sum(s$test_rows),
        stringsAsFactors = FALSE
      )
    })
  )
  rownames(tab) <- NULL
  structure(
    list(table = tab, interpretation = "Future-block predictive scores are comparison evidence, not an automatic model-selection command."),
    class = "gp3bayes_pupil_lfo_comparison"
  )
}

#' @export
print.gp3bayes_pupil_model_set <- function(x, ...) {
  cat("<gp3bayes_pupil_model_set>\n")
  cat("  Models:", paste(names(x$models), collapse = ", "), "\n")
  cat("  Predictive target:", x$predictive_target, "\n")
  invisible(x)
}

#' @export
print.gp3bayes_pupil_model_comparison <- function(x, ...) {
  cat("<gp3bayes_pupil_model_comparison>\n")
  cat("  Criterion:", x$criterion, "\n")
  cat("  Predictive target:", x$predictive_target, "\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_model_comparison <- function(x, row.names = NULL, optional = FALSE, ...) x$table

#' @export
print.gp3bayes_pupil_lfo_plan <- function(x, ...) {
  cat("<gp3bayes_pupil_lfo_plan>\n")
  cat("  Planned refits:", nrow(x$table), "\n")
  cat("  Horizon:", x$horizon, " samples\n")
  cat("  Execute default: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_lfo_plan <- function(x, row.names = NULL, optional = FALSE, ...) x$table

#' @export
print.gp3bayes_pupil_lfo_validation <- function(x, ...) {
  cat("<gp3bayes_pupil_lfo_validation>\n")
  cat("  Executed:", x$executed, "\n")
  if (x$executed) cat("  Total future ELPD:", format(x$total_elpd_future, digits = 5), "\n")
  invisible(x)
}

#' @export
print.gp3bayes_pupil_lfo_comparison <- function(x, ...) {
  cat("<gp3bayes_pupil_lfo_comparison>\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_lfo_comparison <- function(x, row.names = NULL, optional = FALSE, ...) x$table
