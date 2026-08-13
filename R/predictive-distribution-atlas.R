# Posterior-predictive distribution atlases and score uncertainty

.gp3pa_stop <- function(...) stop(paste0(...), call. = FALSE)

.gp3pa_atlas <- function(x) {
  if (!inherits(x, "gp3bayes_predictive_distribution_atlas")) {
    .gp3pa_stop("`x` must be a gp3bayes predictive distribution atlas.")
  }
  invisible(x)
}

.gp3pa_stat <- function(z) {
  c(
    mean = mean(z),
    sd = stats::sd(z),
    median = stats::median(z),
    q10 = stats::quantile(z, 0.10, names = FALSE),
    q90 = stats::quantile(z, 0.90, names = FALSE)
  )
}

#' Create a Posterior-Predictive Distribution Atlas
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param ndraws Number of posterior predictive draws.
#' @param include_group_effects Whether fitted group effects are included.
#' @param seed Predictive seed.
#'
#' @return A `gp3bayes_predictive_distribution_atlas`.
#' @export
create_predictive_distribution_atlas <- function(
  fit,
  ndraws = 500L,
  include_group_effects = TRUE,
  seed = 1L
) {
  .gp3p_validate_fit(fit)
  pred <- predict_model(
    fit,
    type = "predictive",
    include_group_effects = include_group_effects,
    allow_new_levels = FALSE,
    ndraws = ndraws,
    seed = seed
  )
  observed <- .gp3p_training_data(fit)[[.gp3p_outcome_col(fit)]]
  stats <- as.data.frame(t(apply(pred$draws, 1L, .gp3pa_stat)))
  stats$draw <- seq_len(nrow(stats))
  stats <- stats[, c("draw", "mean", "sd", "median", "q10", "q90")]

  structure(
    list(
      family = fit$family,
      prediction = pred,
      observed = observed,
      observed_statistics = .gp3pa_stat(observed),
      draw_statistics = stats,
      include_group_effects = include_group_effects,
      automatic_adequacy_decision = FALSE,
      interpretation = paste(
        "Observed distribution summaries are compared with posterior predictive",
        "replicates without an automatic adequacy verdict."
      )
    ),
    class = "gp3bayes_predictive_distribution_atlas"
  )
}

#' Predictive Distribution Atlas Table
#'
#' @param x A predictive distribution atlas.
#' @return Draw-level distribution summaries.
#' @export
predictive_distribution_atlas_table <- function(x) {
  .gp3pa_atlas(x)
  x$draw_statistics
}

.gp3pa_get <- function(x, ndraws, include_group_effects, seed) {
  if (inherits(x, "gp3bayes_predictive_distribution_atlas")) return(x)
  if (inherits(x, "gp3bayes_fit")) {
    return(create_predictive_distribution_atlas(
      x,
      ndraws = ndraws,
      include_group_effects = include_group_effects,
      seed = seed
    ))
  }
  .gp3pa_stop("`x` must be a fitted gp3bayes model or predictive atlas.")
}

#' Posterior-Predictive Quantile Envelope
#'
#' @param x A fitted model or predictive atlas.
#' @param probabilities Outcome quantile probabilities.
#' @param probs Posterior interval probabilities for each replicated quantile.
#' @param ndraws Predictive draws when `x` is a fit.
#' @param include_group_effects Whether fitted group effects are included.
#' @param seed Predictive seed.
#'
#' @return A quantile-envelope table.
#' @export
predictive_quantile_envelope <- function(
  x,
  probabilities = seq(0.05, 0.95, by = 0.05),
  probs = c(0.025, 0.5, 0.975),
  ndraws = 500L,
  include_group_effects = TRUE,
  seed = 1L
) {
  if (!is.numeric(probabilities) || !length(probabilities) ||
      anyNA(probabilities) || any(probabilities <= 0 | probabilities >= 1)) {
    .gp3pa_stop("`probabilities` must lie strictly between 0 and 1.")
  }
  probs <- .gp3p_probs(probs)
  atlas <- .gp3pa_get(x, ndraws, include_group_effects, seed)

  rows <- lapply(sort(unique(probabilities)), function(p) {
    replicated <- apply(
      atlas$prediction$draws,
      1L,
      stats::quantile,
      probs = p,
      names = FALSE
    )
    q <- stats::quantile(replicated, probs = probs, names = FALSE)
    data.frame(
      probability = p,
      observed_quantile = stats::quantile(
        atlas$observed,
        probs = p,
        names = FALSE
      ),
      predictive_mean = mean(replicated),
      predictive_lower = q[[1L]],
      predictive_median = q[[2L]],
      predictive_upper = q[[3L]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Posterior Uncertainty in Prediction Scores
#'
#' Binary fits use Brier and logarithmic scores; duration fits use RMSE and MAE.
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param newdata Optional data containing observed outcomes.
#' @param include_group_effects Whether fitted group effects are included.
#' @param ndraws Expected-response posterior draws.
#' @param probs Three interval probabilities.
#'
#' @return A `gp3bayes_prediction_score_uncertainty`.
#' @export
prediction_score_uncertainty <- function(
  fit,
  newdata = NULL,
  include_group_effects = FALSE,
  ndraws = 1000L,
  probs = c(0.025, 0.5, 0.975)
) {
  .gp3p_validate_fit(fit)
  probs <- .gp3p_probs(probs)
  data <- if (is.null(newdata)) .gp3p_training_data(fit) else newdata
  outcome <- .gp3p_outcome_col(fit)

  if (!is.data.frame(data) || !outcome %in% names(data)) {
    .gp3pa_stop("Prediction-score uncertainty requires observed outcomes.")
  }
  observed <- data[[outcome]]

  pred <- predict_model(
    fit,
    newdata = data,
    type = "expected",
    include_group_effects = include_group_effects,
    allow_new_levels = FALSE,
    ndraws = ndraws
  )

  obs_matrix <- matrix(
    observed,
    nrow = nrow(pred$draws),
    ncol = length(observed),
    byrow = TRUE
  )

  metric_draws <- if (identical(fit$family, "binary")) {
    eps <- sqrt(.Machine$double.eps)
    p <- pmin(pmax(pred$draws, eps), 1 - eps)
    cbind(
      brier = rowMeans((p - obs_matrix)^2),
      log_loss = -rowMeans(
        obs_matrix * log(p) + (1 - obs_matrix) * log(1 - p)
      )
    )
  } else {
    error <- pred$draws - obs_matrix
    cbind(
      rmse = sqrt(rowMeans(error^2)),
      mae = rowMeans(abs(error))
    )
  }

  long <- data.frame(
    draw = rep(seq_len(nrow(metric_draws)), times = ncol(metric_draws)),
    metric = rep(colnames(metric_draws), each = nrow(metric_draws)),
    value = as.vector(metric_draws),
    stringsAsFactors = FALSE
  )

  summary <- do.call(
    rbind,
    lapply(colnames(metric_draws), function(metric_name) {
      z <- metric_draws[, metric_name]
      q <- stats::quantile(z, probs = probs, names = FALSE)
      data.frame(
        metric = metric_name,
        mean = mean(z),
        lower = q[[1L]],
        median = q[[2L]],
        upper = q[[3L]],
        stringsAsFactors = FALSE
      )
    })
  )

  structure(
    list(
      family = fit$family,
      scope = if (is.null(newdata)) "fitted_prepared_data" else "supplied_data",
      draws = long,
      summary = summary,
      prediction = pred,
      automatic_model_ranking = FALSE,
      interpretation = paste(
        "Score distributions propagate posterior uncertainty on the supplied",
        "evaluation data and are not automatically out-of-sample estimates."
      )
    ),
    class = "gp3bayes_prediction_score_uncertainty"
  )
}

#' Prediction Score-Uncertainty Table
#'
#' @param x A prediction-score uncertainty object.
#' @return Metric summaries.
#' @export
prediction_score_uncertainty_table <- function(x) {
  if (!inherits(x, "gp3bayes_prediction_score_uncertainty")) {
    .gp3pa_stop("`x` must be gp3bayes prediction-score uncertainty.")
  }
  x$summary
}

#' Binary Calibration Uncertainty
#'
#' Equal-width bins are defined from posterior-mean predicted probabilities.
#'
#' @param fit A fitted binary `gp3bayes_fit`.
#' @param newdata Optional data containing observed outcomes.
#' @param bins Number of equal-width probability bins.
#' @param include_group_effects Whether fitted group effects are included.
#' @param ndraws Expected-probability posterior draws.
#' @param probs Three interval probabilities.
#'
#' @return A `gp3bayes_binary_calibration_uncertainty`.
#' @export
binary_calibration_uncertainty <- function(
  fit,
  newdata = NULL,
  bins = 10L,
  include_group_effects = FALSE,
  ndraws = 1000L,
  probs = c(0.025, 0.5, 0.975)
) {
  .gp3p_validate_fit(fit, "binary")
  bins <- .gp3ha_int(bins, "bins", 2L)
  probs <- .gp3p_probs(probs)

  data <- if (is.null(newdata)) .gp3p_training_data(fit) else newdata
  outcome <- .gp3p_outcome_col(fit)
  if (!is.data.frame(data) || !outcome %in% names(data)) {
    .gp3pa_stop("Calibration uncertainty requires observed binary outcomes.")
  }

  pred <- predict_model(
    fit,
    newdata = data,
    type = "expected",
    include_group_effects = include_group_effects,
    allow_new_levels = FALSE,
    ndraws = ndraws
  )

  mean_p <- colMeans(pred$draws)
  breaks <- seq(0, 1, length.out = bins + 1L)
  bin <- cut(
    mean_p,
    breaks = breaks,
    include.lowest = TRUE,
    labels = FALSE
  )

  rows <- lapply(sort(unique(bin)), function(b) {
    idx <- which(bin == b)
    pd <- rowMeans(pred$draws[, idx, drop = FALSE])
    q <- stats::quantile(pd, probs = probs, names = FALSE)
    data.frame(
      bin = b,
      n = length(idx),
      probability_lower_bound = breaks[[b]],
      probability_upper_bound = breaks[[b + 1L]],
      observed_rate = mean(data[[outcome]][idx]),
      predicted_mean = mean(pd),
      predicted_lower = q[[1L]],
      predicted_median = q[[2L]],
      predicted_upper = q[[3L]],
      stringsAsFactors = FALSE
    )
  })

  structure(
    list(
      table = do.call(rbind, rows),
      bins_requested = bins,
      prediction = pred,
      scope = if (is.null(newdata)) "fitted_prepared_data" else "supplied_data",
      automatic_calibration_decision = FALSE,
      interpretation = paste(
        "Observed bin rates are compared with posterior uncertainty in predicted",
        "probabilities without automatic calibration certification."
      )
    ),
    class = "gp3bayes_binary_calibration_uncertainty"
  )
}

#' Binary Calibration-Uncertainty Table
#'
#' @param x A binary calibration-uncertainty object.
#' @return Bin-level summaries.
#' @export
binary_calibration_uncertainty_table <- function(x) {
  if (!inherits(x, "gp3bayes_binary_calibration_uncertainty")) {
    .gp3pa_stop("`x` must be gp3bayes binary calibration uncertainty.")
  }
  x$table
}

#' Plot Predictive Atlas Statistics
#'
#' @param x A predictive distribution atlas.
#' @return A faceted `ggplot`.
#' @export
plot_predictive_atlas_statistics <- function(x) {
  .gp3g_require("ggplot2", "plot predictive atlas statistics")
  .gp3pa_atlas(x)
  metrics <- c("mean", "sd", "median", "q10", "q90")

  long <- do.call(
    rbind,
    lapply(metrics, function(metric_name) {
      data.frame(
        statistic = metric_name,
        value = x$draw_statistics[[metric_name]],
        observed_statistic = unname(x$observed_statistics[[metric_name]]),
        stringsAsFactors = FALSE
      )
    })
  )

  ggplot2::ggplot(long, ggplot2::aes(x = value)) +
    ggplot2::geom_density() +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = observed_statistic),
      linetype = 2
    ) +
    ggplot2::facet_wrap(~statistic, scales = "free") +
    ggplot2::labs(
      x = "Posterior predictive statistic",
      y = "Density",
      title = "Posterior-predictive distribution atlas",
      subtitle = "Dashed lines show observed statistics."
    ) +
    theme_gp3bayes()
}

#' Plot Posterior-Predictive Quantile Envelope
#'
#' @param x A quantile-envelope table.
#' @return A `ggplot`.
#' @export
plot_predictive_quantile_envelope <- function(x) {
  .gp3g_require("ggplot2", "plot predictive quantile envelopes")
  required <- c(
    "probability", "observed_quantile", "predictive_median",
    "predictive_lower", "predictive_upper"
  )
  if (!is.data.frame(x) || !all(required %in% names(x))) {
    .gp3pa_stop("`x` must be a predictive quantile-envelope table.")
  }

  ggplot2::ggplot(x, ggplot2::aes(x = probability, y = predictive_median)) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = predictive_lower, ymax = predictive_upper),
      alpha = 0.2
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(y = observed_quantile), shape = 4) +
    ggplot2::labs(
      x = "Quantile probability",
      y = "Outcome quantile",
      title = "Posterior-predictive quantile envelope",
      subtitle = "Crosses show observed quantiles."
    ) +
    theme_gp3bayes()
}

#' Plot Prediction Score Uncertainty
#'
#' @param x A score-uncertainty object.
#' @return A faceted `ggplot`.
#' @export
plot_prediction_score_uncertainty <- function(x) {
  .gp3g_require("ggplot2", "plot prediction-score uncertainty")
  if (!inherits(x, "gp3bayes_prediction_score_uncertainty")) {
    .gp3pa_stop("`x` must be gp3bayes prediction-score uncertainty.")
  }

  ggplot2::ggplot(x$draws, ggplot2::aes(x = value)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~metric, scales = "free") +
    ggplot2::labs(
      x = "Score",
      y = "Density",
      title = "Posterior uncertainty in prediction scores",
      subtitle = x$scope
    ) +
    theme_gp3bayes()
}

#' Plot Binary Calibration Uncertainty
#'
#' @param x A binary calibration-uncertainty object or table.
#' @return A `ggplot`.
#' @export
plot_binary_calibration_uncertainty <- function(x) {
  .gp3g_require("ggplot2", "plot binary calibration uncertainty")
  d <- if (inherits(x, "gp3bayes_binary_calibration_uncertainty")) {
    binary_calibration_uncertainty_table(x)
  } else {
    x
  }
  required <- c(
    "observed_rate", "predicted_median", "predicted_lower", "predicted_upper"
  )
  if (!is.data.frame(d) || !all(required %in% names(d))) {
    .gp3pa_stop("`x` does not contain calibration-uncertainty summaries.")
  }

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = predicted_median, y = observed_rate)
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = predicted_lower, xmax = predicted_upper),
      orientation = "y",
      width = 0
    ) +
    ggplot2::geom_point() +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      x = "Posterior predicted probability",
      y = "Observed event rate",
      title = "Binary calibration with posterior uncertainty"
    ) +
    theme_gp3bayes()
}

#' @export
print.gp3bayes_predictive_distribution_atlas <- function(x, ...) {
  cat("\ngp3bayes posterior-predictive distribution atlas\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Predictive draws: ", nrow(x$prediction$draws), "\n", sep = "")
  cat(" Automatic adequacy decision: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_prediction_score_uncertainty <- function(x, ...) {
  cat("\ngp3bayes prediction-score uncertainty\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Scope: ", x$scope, "\n", sep = "")
  cat(" Automatic model ranking: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_binary_calibration_uncertainty <- function(x, ...) {
  cat("\ngp3bayes binary calibration uncertainty\n")
  cat(" Non-empty bins: ", nrow(x$table), "\n", sep = "")
  cat(" Scope: ", x$scope, "\n", sep = "")
  cat(" Automatic calibration decision: FALSE\n")
  invisible(x)
}
