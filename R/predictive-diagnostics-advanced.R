# Advanced predictive diagnostics for gp3bayes development

.gp3x_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.gp3x_prediction <- function(x, type = NULL, family = NULL, observed = FALSE) {
  if (!inherits(x, "gp3bayes_prediction")) {
    .gp3x_stop("`x` must be a `gp3bayes_prediction`.")
  }
  if (!is.null(type) && !x$type %in% type) {
    .gp3x_stop(
      "`x$type` must be one of: ", paste(type, collapse = ", "), "."
    )
  }
  if (!is.null(family) && !identical(x$family, family)) {
    .gp3x_stop("`x` must use the `", family, "` family.")
  }
  if (observed && is.null(x$observed)) {
    .gp3x_stop("Observed outcomes are required for this diagnostic.")
  }
  invisible(x)
}

.gp3x_prob_vector <- function(x, observed = NULL) {
  if (inherits(x, "gp3bayes_prediction")) {
    .gp3x_prediction(x, family = "binary", observed = TRUE)
    if (!identical(x$type, "expected")) {
      .gp3x_stop("Binary probability diagnostics require `type = \"expected\"`.")
    }
    p <- x$summary$predicted_mean
    y <- x$observed
  } else {
    if (!is.numeric(x) || !is.numeric(observed) || length(x) != length(observed)) {
      .gp3x_stop(
        "Supply a binary expected-response prediction or numeric probabilities ",
        "plus numeric observed outcomes."
      )
    }
    p <- as.numeric(x)
    y <- as.numeric(observed)
  }

  if (
    anyNA(p) || anyNA(y) ||
      any(!is.finite(p)) || any(!is.finite(y)) ||
      any(p < 0 | p > 1) ||
      any(!y %in% c(0, 1))
  ) {
    .gp3x_stop(
      "Binary diagnostics require finite probabilities from 0 to 1 and ",
      "observed outcomes coded 0 or 1."
    )
  }

  list(p = p, y = y)
}

.gp3x_validate_bins <- function(bins) {
  if (
    !is.numeric(bins) || length(bins) != 1L || is.na(bins) ||
      !is.finite(bins) || bins < 2 || bins != floor(bins)
  ) {
    .gp3x_stop("`bins` must be one integer greater than or equal to 2.")
  }
  as.integer(bins)
}

.gp3x_validate_probs <- function(probs) {
  if (
    !is.numeric(probs) || !length(probs) || anyNA(probs) ||
      any(!is.finite(probs)) || any(probs <= 0 | probs >= 1)
  ) {
    .gp3x_stop("`probs` must contain finite probabilities strictly inside (0, 1).")
  }
  sort(unique(as.numeric(probs)))
}

#' Convert Prediction Draws to Long Form
#'
#' @param x A `gp3bayes_prediction`.
#' @param max_draws Optional maximum number of posterior draws retained.
#' @param seed Non-negative integer used only if draws are subsampled.
#'
#' @return A long data frame with draw, observation, and predicted value.
#' @export
prediction_draws_long <- function(x, max_draws = NULL, seed = 1L) {
  .gp3x_prediction(x)
  draws <- x$draws

  if (!is.null(max_draws)) {
    if (
      !is.numeric(max_draws) || length(max_draws) != 1L ||
        is.na(max_draws) || !is.finite(max_draws) ||
        max_draws < 1 || max_draws != floor(max_draws)
    ) {
      .gp3x_stop("`max_draws` must be NULL or one positive integer.")
    }
    max_draws <- as.integer(max_draws)
    if (nrow(draws) > max_draws) {
      if (
        !is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
          !is.finite(seed) || seed < 0 || seed != floor(seed)
      ) {
        .gp3x_stop("`seed` must be one non-negative integer.")
      }
      keep <- withr::with_seed(
        as.integer(seed),
        sample.int(nrow(draws), max_draws, replace = FALSE)
      )
      draws <- draws[sort(keep), , drop = FALSE]
    }
  }

  data.frame(
    draw = rep(seq_len(nrow(draws)), times = ncol(draws)),
    observation = rep(seq_len(ncol(draws)), each = nrow(draws)),
    value = as.vector(draws),
    stringsAsFactors = FALSE
  )
}

.gp3x_stat_fun <- function(statistic, threshold = NULL) {
  statistic <- match.arg(
    statistic,
    c("mean", "sd", "median", "q90", "q95", "max", "tail_rate")
  )

  if (identical(statistic, "tail_rate")) {
    if (
      !is.numeric(threshold) || length(threshold) != 1L ||
        is.na(threshold) || !is.finite(threshold)
    ) {
      .gp3x_stop(
        "`threshold` must be one finite number when `statistic = \"tail_rate\"`."
      )
    }
  }

  fun <- switch(
    statistic,
    mean = function(z) mean(z),
    sd = function(z) stats::sd(z),
    median = function(z) stats::median(z),
    q90 = function(z) stats::quantile(z, 0.90, names = FALSE),
    q95 = function(z) stats::quantile(z, 0.95, names = FALSE),
    max = function(z) max(z),
    tail_rate = function(z) mean(z > threshold)
  )

  list(name = statistic, fun = fun, threshold = threshold)
}

#' Posterior Predictive Statistic Check
#'
#' Computes one scalar discrepancy statistic for every posterior predictive draw
#' and compares that distribution with the same statistic in the observed data.
#' The returned probability is descriptive and is not an automatic model verdict.
#'
#' @param x A posterior predictive `gp3bayes_prediction`.
#' @param statistic Built-in statistic: `"mean"`, `"sd"`, `"median"`, `"q90"`,
#'   `"q95"`, `"max"`, or `"tail_rate"`.
#' @param threshold Required for `"tail_rate"`.
#'
#' @return A `gp3bayes_ppc_statistic` object.
#' @export
posterior_predictive_statistic <- function(
  x,
  statistic = c("mean", "sd", "median", "q90", "q95", "max", "tail_rate"),
  threshold = NULL
) {
  .gp3x_prediction(x, type = "predictive", observed = TRUE)
  specification <- .gp3x_stat_fun(statistic, threshold)
  statistic <- specification$name
  fun <- specification$fun

  observed_value <- fun(x$observed)
  replicated <- apply(x$draws, 1L, fun)

  if (anyNA(replicated) || any(!is.finite(replicated)) || !is.finite(observed_value)) {
    .gp3x_stop("The selected predictive statistic produced non-finite values.")
  }

  upper <- mean(replicated >= observed_value)
  lower <- mean(replicated <= observed_value)
  two_sided <- min(1, 2 * min(upper, lower))

  structure(
    list(
      family = x$family,
      statistic = statistic,
      threshold = threshold,
      observed = observed_value,
      replicated = replicated,
      posterior_mean = mean(replicated),
      posterior_sd = stats::sd(replicated),
      lower_tail_probability = lower,
      upper_tail_probability = upper,
      two_sided_tail_probability = two_sided,
      automatic_adequacy_verdict = FALSE,
      interpretation = paste(
        "The tail probability is a descriptive posterior predictive discrepancy",
        "measure. It is not an automatic model-adequacy test."
      )
    ),
    class = "gp3bayes_ppc_statistic"
  )
}

#' @export
print.gp3bayes_ppc_statistic <- function(x, ...) {
  cat("\ngp3bayes posterior predictive statistic\n")
  cat(" Statistic: ", x$statistic, "\n", sep = "")
  cat(" Observed: ", format(x$observed), "\n", sep = "")
  cat(
    " Two-sided descriptive tail probability: ",
    format(x$two_sided_tail_probability),
    "\n",
    sep = ""
  )
  cat(" Automatic adequacy verdict: FALSE\n")
  invisible(x)
}

#' Posterior Predictive Statistic Table
#'
#' @param x A `gp3bayes_ppc_statistic`.
#'
#' @return A one-row data frame.
#' @export
ppc_statistic_table <- function(x) {
  if (!inherits(x, "gp3bayes_ppc_statistic")) {
    .gp3x_stop("`x` must be a `gp3bayes_ppc_statistic`.")
  }
  data.frame(
    statistic = x$statistic,
    threshold = if (is.null(x$threshold)) NA_real_ else x$threshold,
    observed = x$observed,
    posterior_mean = x$posterior_mean,
    posterior_sd = x$posterior_sd,
    lower_tail_probability = x$lower_tail_probability,
    upper_tail_probability = x$upper_tail_probability,
    two_sided_tail_probability = x$two_sided_tail_probability,
    automatic_adequacy_verdict = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Binary Confusion Table
#'
#' @param x A binary expected prediction or numeric probabilities.
#' @param observed Optional observed binary outcomes.
#' @param threshold Classification threshold from 0 to 1.
#'
#' @return A four-row confusion table plus rates as attributes.
#' @examples
#' binary_confusion_table(c(0.1, 0.8, 0.7, 0.2), c(0, 1, 1, 0))
#' @export
binary_confusion_table <- function(x, observed = NULL, threshold = 0.5) {
  z <- .gp3x_prob_vector(x, observed)
  if (
    !is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || !is.finite(threshold) ||
      threshold < 0 || threshold > 1
  ) {
    .gp3x_stop("`threshold` must be one finite number from 0 to 1.")
  }

  pred <- as.integer(z$p >= threshold)
  table <- data.frame(
    observed = c(0L, 0L, 1L, 1L),
    predicted = c(0L, 1L, 0L, 1L),
    count = c(
      sum(z$y == 0 & pred == 0),
      sum(z$y == 0 & pred == 1),
      sum(z$y == 1 & pred == 0),
      sum(z$y == 1 & pred == 1)
    ),
    threshold = threshold,
    stringsAsFactors = FALSE
  )
  table
}

#' Binary ROC Curve
#'
#' @param x A binary expected prediction or numeric probabilities.
#' @param observed Optional observed binary outcomes.
#' @param thresholds Optional thresholds. By default all finite empirical
#'   breakpoints are used.
#'
#' @return A data frame containing false-positive and true-positive rates.
#' @examples
#' binary_roc_curve(c(0.1, 0.8, 0.7, 0.2), c(0, 1, 1, 0))
#' @export
binary_roc_curve <- function(x, observed = NULL, thresholds = NULL) {
  z <- .gp3x_prob_vector(x, observed)
  p <- z$p
  y <- z$y

  if (is.null(thresholds)) {
    thresholds <- sort(unique(c(Inf, p, -Inf)), decreasing = TRUE)
  } else {
    if (!is.numeric(thresholds) || anyNA(thresholds)) {
      .gp3x_stop("`thresholds` must be NULL or a numeric vector without missing values.")
    }
    thresholds <- sort(unique(thresholds), decreasing = TRUE)
  }

  positives <- sum(y == 1)
  negatives <- sum(y == 0)

  rows <- lapply(thresholds, function(th) {
    pred <- as.integer(p >= th)
    tp <- sum(pred == 1 & y == 1)
    fp <- sum(pred == 1 & y == 0)
    data.frame(
      threshold = th,
      false_positive_rate = if (negatives) fp / negatives else NA_real_,
      true_positive_rate = if (positives) tp / positives else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out <- out[order(out$false_positive_rate, out$true_positive_rate), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Binary Precision-Recall Curve
#'
#' @inheritParams binary_roc_curve
#'
#' @return A data frame containing recall and precision.
#' @examples
#' binary_precision_recall_curve(
#'   c(0.1, 0.8, 0.7, 0.2),
#'   c(0, 1, 1, 0)
#' )
#' @export
binary_precision_recall_curve <- function(
  x,
  observed = NULL,
  thresholds = NULL
) {
  z <- .gp3x_prob_vector(x, observed)
  p <- z$p
  y <- z$y

  if (is.null(thresholds)) {
    thresholds <- sort(unique(c(Inf, p, -Inf)), decreasing = TRUE)
  } else {
    if (!is.numeric(thresholds) || anyNA(thresholds)) {
      .gp3x_stop("`thresholds` must be NULL or numeric without missing values.")
    }
    thresholds <- sort(unique(thresholds), decreasing = TRUE)
  }

  positives <- sum(y == 1)

  rows <- lapply(thresholds, function(th) {
    pred <- as.integer(p >= th)
    tp <- sum(pred == 1 & y == 1)
    fp <- sum(pred == 1 & y == 0)
    data.frame(
      threshold = th,
      recall = if (positives) tp / positives else NA_real_,
      precision = if (tp + fp) tp / (tp + fp) else 1,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out <- out[order(out$recall, out$precision), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Binary Calibration Error
#'
#' @param x A binary expected prediction or numeric probabilities.
#' @param observed Optional observed outcomes.
#' @param bins Number of equal-frequency bins.
#'
#' @return A one-row table with expected and maximum absolute calibration error.
#' @examples
#' binary_calibration_error(c(0.1, 0.8, 0.7, 0.2), c(0, 1, 1, 0), bins = 2)
#' @export
binary_calibration_error <- function(
  x,
  observed = NULL,
  bins = 10L
) {
  z <- .gp3x_prob_vector(x, observed)
  bins <- .gp3x_validate_bins(bins)

  p <- z$p
  y <- z$y
  breaks <- unique(stats::quantile(
    p,
    probs = seq(0, 1, length.out = bins + 1L),
    names = FALSE,
    type = 8
  ))

  if (length(breaks) < 3L) {
    bin <- rep(1L, length(p))
  } else {
    bin <- cut(p, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  }

  idx <- split(seq_along(p), bin)
  weight <- vapply(idx, length, integer(1L)) / length(p)
  pred_mean <- vapply(idx, function(i) mean(p[i]), numeric(1L))
  obs_mean <- vapply(idx, function(i) mean(y[i]), numeric(1L))
  gaps <- abs(pred_mean - obs_mean)

  data.frame(
    n = length(p),
    bins_requested = bins,
    bins_used = length(idx),
    expected_calibration_error = sum(weight * gaps),
    maximum_calibration_error = max(gaps),
    automatic_adequacy_verdict = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Grouped Binary Calibration
#'
#' @param x A binary expected-response `gp3bayes_prediction` with observed
#'   outcomes.
#' @param group Name of a column in `x$newdata`.
#'
#' @return A group-level calibration summary.
#' @export
binary_group_calibration <- function(x, group) {
  .gp3x_prediction(x, type = "expected", family = "binary", observed = TRUE)

  if (
    !is.character(group) || length(group) != 1L ||
      is.na(group) || !group %in% names(x$newdata)
  ) {
    .gp3x_stop("`group` must name one column in `x$newdata`.")
  }

  values <- as.character(x$newdata[[group]])
  idx <- split(seq_along(values), values)
  rows <- lapply(names(idx), function(label) {
    i <- idx[[label]]
    data.frame(
      group = label,
      n = length(i),
      predicted_probability = mean(x$summary$predicted_mean[i]),
      observed_rate = mean(x$observed[i]),
      calibration_gap = mean(x$observed[i]) -
        mean(x$summary$predicted_mean[i]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Duration Predictive Q-Q Table
#'
#' @param x A duration posterior predictive `gp3bayes_prediction`.
#' @param probs Quantile probabilities.
#'
#' @return A quantile-comparison table.
#' @export
duration_qq_table <- function(
  x,
  probs = seq(0.05, 0.95, by = 0.05)
) {
  .gp3x_prediction(x, type = "predictive", family = "duration", observed = TRUE)
  probs <- .gp3x_validate_probs(probs)

  observed_q <- stats::quantile(
    x$observed,
    probs = probs,
    names = FALSE
  )
  predictive_q <- apply(
    x$draws,
    1L,
    stats::quantile,
    probs = probs,
    names = FALSE
  )
  if (is.null(dim(predictive_q))) {
    predictive_q <- matrix(predictive_q, nrow = length(probs))
  }

  data.frame(
    probability = probs,
    observed_quantile = observed_q,
    predictive_mean_quantile = rowMeans(predictive_q),
    predictive_lower_quantile = apply(
      predictive_q, 1L, stats::quantile, probs = 0.025, names = FALSE
    ),
    predictive_upper_quantile = apply(
      predictive_q, 1L, stats::quantile, probs = 0.975, names = FALSE
    ),
    stringsAsFactors = FALSE
  )
}

#' Duration Tail Check
#'
#' @param x A duration posterior predictive `gp3bayes_prediction`.
#' @param threshold Positive duration threshold.
#'
#' @return A one-row table comparing observed and posterior predictive tail rates.
#' @export
duration_tail_check <- function(x, threshold) {
  .gp3x_prediction(x, type = "predictive", family = "duration", observed = TRUE)
  if (
    !is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || !is.finite(threshold) || threshold <= 0
  ) {
    .gp3x_stop("`threshold` must be one finite positive duration.")
  }

  replicated_rates <- rowMeans(x$draws > threshold)
  observed_rate <- mean(x$observed > threshold)

  data.frame(
    threshold = threshold,
    observed_tail_rate = observed_rate,
    predictive_mean_tail_rate = mean(replicated_rates),
    predictive_lower_tail_rate = stats::quantile(
      replicated_rates, 0.025, names = FALSE
    ),
    predictive_upper_tail_rate = stats::quantile(
      replicated_rates, 0.975, names = FALSE
    ),
    posterior_probability_rate_ge_observed = mean(
      replicated_rates >= observed_rate
    ),
    automatic_adequacy_verdict = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Group Prediction Summary
#'
#' Aggregates posterior prediction draws over one or more columns already present
#' in the prediction data.
#'
#' @param x A `gp3bayes_prediction`.
#' @param by Character vector naming grouping columns in `x$newdata`.
#' @param probs Three probabilities for group-level posterior intervals.
#'
#' @return A group-level posterior prediction table.
#' @export
group_prediction_summary <- function(
  x,
  by,
  probs = c(0.025, 0.5, 0.975)
) {
  .gp3x_prediction(x)
  if (
    !is.character(by) || !length(by) || anyNA(by) ||
      any(!by %in% names(x$newdata))
  ) {
    .gp3x_stop("`by` must name one or more columns in `x$newdata`.")
  }
  probs <- .gp3p_probs(probs)

  key_data <- x$newdata[, by, drop = FALSE]
  key <- interaction(key_data, drop = TRUE, lex.order = TRUE)
  idx <- split(seq_len(nrow(x$newdata)), key)

  rows <- lapply(idx, function(i) {
    gd <- rowMeans(x$draws[, i, drop = FALSE])
    q <- stats::quantile(gd, probs = probs, names = FALSE)
    identity <- key_data[i[[1L]], , drop = FALSE]
    cbind(
      identity,
      data.frame(
        n = length(i),
        predicted_mean = mean(gd),
        lower = q[[1L]],
        predicted_median = q[[2L]],
        upper = q[[3L]],
        observed = if (is.null(x$observed)) {
          NA_real_
        } else {
          mean(x$observed[i])
        },
        stringsAsFactors = FALSE
      )
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Pairwise Prediction Contrasts
#'
#' @param x A `gp3bayes_prediction`.
#' @param rows Optional prediction-row indices. At most `max_rows` rows may be
#'   compared.
#' @param measure Difference or ratio.
#' @param max_rows Maximum number of prediction rows allowed.
#' @param probs Three posterior interval probabilities.
#'
#' @return A data frame containing every unique pairwise contrast.
#' @export
prediction_pairwise_contrasts <- function(
  x,
  rows = NULL,
  measure = c("difference", "ratio"),
  max_rows = 20L,
  probs = c(0.025, 0.5, 0.975)
) {
  .gp3x_prediction(x)
  measure <- match.arg(measure)

  if (is.null(rows)) rows <- seq_len(ncol(x$draws))
  if (
    !is.numeric(rows) || anyNA(rows) || any(!is.finite(rows)) ||
      any(rows != floor(rows)) ||
      any(rows < 1 | rows > ncol(x$draws))
  ) {
    .gp3x_stop("`rows` must contain valid prediction-row indices.")
  }
  rows <- unique(as.integer(rows))

  if (
    !is.numeric(max_rows) || length(max_rows) != 1L ||
      is.na(max_rows) || max_rows < 2 || max_rows != floor(max_rows)
  ) {
    .gp3x_stop("`max_rows` must be one integer greater than or equal to 2.")
  }
  if (length(rows) > max_rows) {
    .gp3x_stop(
      "Requested ", length(rows), " prediction rows; the explicit maximum is ",
      max_rows, "."
    )
  }

  pairs <- utils::combn(rows, 2L)
  results <- lapply(seq_len(ncol(pairs)), function(j) {
    prediction_contrast(
      x,
      row1 = pairs[1L, j],
      row2 = pairs[2L, j],
      measure = measure,
      probs = probs
    )
  })
  do.call(rbind, results)
}

#' Prediction Interval Width Table
#'
#' @param x A `gp3bayes_prediction`.
#'
#' @return A table of posterior interval width by prediction row.
#' @export
prediction_interval_width <- function(x) {
  .gp3x_prediction(x)
  d <- x$summary
  data.frame(
    observation = d$observation,
    lower = d$lower,
    upper = d$upper,
    interval_width = d$upper - d$lower,
    predicted_mean = d$predicted_mean,
    stringsAsFactors = FALSE
  )
}

#' Posterior Ranking Probabilities for Prediction Rows
#'
#' Summarises relative ordering among a small, explicitly supplied set of
#' prediction rows. No row is automatically selected or declared superior.
#'
#' @param x A `gp3bayes_prediction`.
#' @param rows Optional prediction rows.
#' @param direction Whether larger or smaller values receive rank 1.
#' @param max_rows Maximum rows that may be ranked.
#'
#' @return A descriptive ranking-probability table.
#' @export
prediction_rank_probabilities <- function(
  x,
  rows = NULL,
  direction = c("higher", "lower"),
  max_rows = 20L
) {
  .gp3x_prediction(x)
  direction <- match.arg(direction)
  if (is.null(rows)) rows <- seq_len(ncol(x$draws))
  if (
    !is.numeric(rows) || anyNA(rows) || any(rows != floor(rows)) ||
      any(rows < 1 | rows > ncol(x$draws))
  ) {
    .gp3x_stop("`rows` must contain valid prediction-row indices.")
  }
  rows <- unique(as.integer(rows))
  if (length(rows) > max_rows) {
    .gp3x_stop("Too many rows requested for ranking; increase `max_rows` explicitly.")
  }

  selected <- x$draws[, rows, drop = FALSE]
  ranks <- t(apply(
    selected,
    1L,
    function(z) {
      if (direction == "higher") rank(-z, ties.method = "average") else
        rank(z, ties.method = "average")
    }
  ))

  data.frame(
    observation = rows,
    probability_rank_1 = colMeans(ranks == 1),
    mean_rank = colMeans(ranks),
    median_rank = apply(ranks, 2L, stats::median),
    automatic_selection = FALSE,
    stringsAsFactors = FALSE
  )
}
