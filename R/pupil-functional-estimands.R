# Functional posterior estimands for gp3bayes 0.5 advanced pupil trajectories.

.p05_functional_input <- function(x) {
  if (inherits(x, "gp3bayes_pupil_advanced_trajectory")) {
    return(list(grid = x$grid, draws = x$draws, specification = x$specification, derivative_order = 0L))
  }
  if (inherits(x, "gp3bayes_pupil_trajectory_derivative")) {
    return(list(grid = x$grid, draws = x$draws, specification = x$specification, derivative_order = x$order))
  }
  stop("Expected an advanced trajectory prediction or derivative object.", call. = FALSE)
}

.p05_derivative_once <- function(grid, draws, time_col, condition_col = NULL) {
  if (!is.matrix(draws)) draws <- as.matrix(draws)
  groups <- if (is.null(condition_col)) {
    rep(".all", nrow(grid))
  } else {
    as.character(grid[[condition_col]])
  }
  keep_grid <- list()
  keep_draws <- list()
  cursor <- 1L
  for (g in unique(groups)) {
    idx <- which(groups == g)
    ord <- order(grid[[time_col]][idx])
    idx <- idx[ord]
    tt <- as.numeric(grid[[time_col]][idx])
    if (length(tt) < 2L) next
    dt <- diff(tt)
    if (any(!is.finite(dt) | dt <= 0)) {
      stop("Derivative estimation requires strictly increasing unique time points within condition.", call. = FALSE)
    }
    dd <- sweep(draws[, idx[-1L], drop = FALSE] - draws[, idx[-length(idx)], drop = FALSE], 2L, dt, "/")
    mid <- (tt[-1L] + tt[-length(tt)]) / 2
    ggrid <- grid[idx[-1L], , drop = FALSE]
    ggrid[[time_col]] <- mid
    keep_grid[[cursor]] <- ggrid
    keep_draws[[cursor]] <- dd
    cursor <- cursor + 1L
  }
  if (!length(keep_grid)) stop("Not enough distinct time points to estimate a derivative.", call. = FALSE)
  out_grid <- do.call(rbind, keep_grid)
  rownames(out_grid) <- NULL
  out_draws <- do.call(cbind, keep_draws)
  list(grid = out_grid, draws = out_draws)
}

#' Estimate posterior temporal derivatives of a pupil trajectory
#'
#' Computes finite-difference posterior derivatives on the prediction grid.
#' This is a descriptive functional estimand: it does not automatically define
#' physiological onset, changepoints, or cognitively meaningful phases.
#'
#' @param prediction A `gp3bayes_pupil_advanced_trajectory` object.
#' @param order Derivative order, 1 (velocity/slope) or 2 (acceleration/curvature).
#' @param probability Central posterior interval probability.
#' @return A `gp3bayes_pupil_trajectory_derivative` object.
#' @export
estimate_pupil_trajectory_derivative <- function(prediction, order = 1L, probability = 0.95) {
  if (!inherits(prediction, "gp3bayes_pupil_advanced_trajectory")) {
    stop("`prediction` must come from predict_advanced_pupil_trajectory().", call. = FALSE)
  }
  order <- as.integer(order)
  if (!order %in% c(1L, 2L)) stop("`order` must be 1 or 2.", call. = FALSE)
  .p05_assert_probability(probability, "probability")
  spec <- prediction$specification
  time_col <- spec$mapping$time
  condition_col <- spec$mapping$condition
  cur <- list(grid = prediction$grid, draws = prediction$draws)
  for (i in seq_len(order)) {
    cur <- .p05_derivative_once(cur$grid, cur$draws, time_col, condition_col)
  }
  structure(
    list(
      grid = cur$grid,
      draws = cur$draws,
      order = order,
      probability = probability,
      specification = spec,
      governance = paste(
        "Posterior temporal derivatives are numerical functional estimands.",
        "They do not automatically identify response onset, a changepoint, or a cognitive event."
      )
    ),
    class = "gp3bayes_pupil_trajectory_derivative"
  )
}

#' Tabulate posterior pupil trajectory derivatives
#' @param x A trajectory-derivative object.
#' @param probability Optional interval probability overriding the stored value.
#' @return A data frame.
#' @export
pupil_trajectory_derivative_table <- function(x, probability = x$probability) {
  if (!inherits(x, "gp3bayes_pupil_trajectory_derivative")) stop("Expected a trajectory derivative object.", call. = FALSE)
  .p05_assert_probability(probability, "probability")
  alpha <- (1 - probability) / 2
  s <- .p05_quantile_summary(x$draws, c(alpha, 0.5, 1 - alpha))
  out <- cbind(x$grid, derivative_order = x$order, s, row.names = NULL)
  out
}

#' Estimate a dynamic posterior contrast between two pupil conditions
#'
#' The contrast is evaluated pointwise on an explicitly supplied posterior
#' trajectory (or its derivative). No favorable time window is selected.
#'
#' @param prediction An advanced trajectory or derivative object.
#' @param contrast Character vector of exactly two condition levels: first minus second.
#' @param threshold Prespecified scientifically meaningful contrast threshold.
#' @param probability Central posterior interval probability.
#' @return A `gp3bayes_pupil_dynamic_contrast` object.
#' @export
estimate_pupil_dynamic_contrast <- function(prediction, contrast, threshold = 0, probability = 0.95) {
  z <- .p05_functional_input(prediction)
  .p05_assert_probability(probability, "probability")
  if (!is.character(contrast) || length(contrast) != 2L || anyNA(contrast) || any(!nzchar(contrast))) {
    stop("`contrast` must contain exactly two non-missing condition labels.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold)) stop("`threshold` must be one finite number.", call. = FALSE)
  spec <- z$specification
  cond_col <- spec$mapping$condition
  time_col <- spec$mapping$time
  if (is.null(cond_col)) stop("A condition column is required for a dynamic contrast.", call. = FALSE)
  cond <- as.character(z$grid[[cond_col]])
  if (!all(contrast %in% unique(cond))) stop("Requested contrast levels are not both present in the prediction grid.", call. = FALSE)
  idx1 <- which(cond == contrast[[1L]])
  idx2 <- which(cond == contrast[[2L]])
  t1 <- as.numeric(z$grid[[time_col]][idx1])
  t2 <- as.numeric(z$grid[[time_col]][idx2])
  common <- intersect(t1, t2)
  if (!length(common)) stop("The two conditions have no common prediction times.", call. = FALSE)
  common <- sort(common)
  p1 <- match(common, t1)
  p2 <- match(common, t2)
  d <- z$draws[, idx1[p1], drop = FALSE] - z$draws[, idx2[p2], drop = FALSE]
  alpha <- (1 - probability) / 2
  s <- .p05_quantile_summary(d, c(alpha, 0.5, 1 - alpha))
  prob_above <- colMeans(d > threshold)
  prob_below <- colMeans(d < -abs(threshold))
  grid <- data.frame(time = common, stringsAsFactors = FALSE)
  names(grid)[[1L]] <- time_col
  tab <- cbind(
    grid,
    contrast = paste0(contrast[[1L]], " - ", contrast[[2L]]),
    derivative_order = z$derivative_order,
    threshold = threshold,
    s,
    probability_above_threshold = prob_above,
    probability_below_negative_threshold = prob_below,
    row.names = NULL
  )
  structure(
    list(
      table = tab,
      grid = grid,
      draws = d,
      contrast = contrast,
      threshold = threshold,
      derivative_order = z$derivative_order,
      probability = probability,
      specification = spec,
      governance = paste(
        "The contrast is pointwise and threshold-prespecified.",
        "gp3bayes does not search for a favorable time window or automatically label periods as substantively meaningful."
      )
    ),
    class = "gp3bayes_pupil_dynamic_contrast"
  )
}

#' Tabulate a dynamic pupil contrast
#' @param x A dynamic-contrast object.
#' @return A data frame.
#' @export
pupil_dynamic_contrast_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_dynamic_contrast")) stop("Expected a dynamic pupil contrast.", call. = FALSE)
  x$table
}

#' Estimate posterior duration above a prespecified dynamic threshold
#'
#' Duration is computed draw-by-draw from the dynamic contrast on its existing
#' time grid. The threshold and direction must be supplied before interpretation;
#' the function performs no threshold or window optimization.
#'
#' @param contrast A dynamic-contrast object.
#' @param direction `"above"`, `"below"`, or `"absolute"`.
#' @param threshold Optional threshold overriding the contrast's stored threshold.
#' @param probability Central interval probability.
#' @return A `gp3bayes_pupil_threshold_duration` object.
#' @export
estimate_pupil_threshold_duration <- function(
    contrast,
    direction = c("above", "below", "absolute"),
    threshold = contrast$threshold,
    probability = 0.95) {
  if (!inherits(contrast, "gp3bayes_pupil_dynamic_contrast")) stop("Expected a dynamic pupil contrast.", call. = FALSE)
  direction <- match.arg(direction)
  .p05_assert_probability(probability, "probability")
  if (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold)) stop("`threshold` must be finite.", call. = FALSE)
  time_col <- contrast$specification$mapping$time
  tt <- as.numeric(contrast$grid[[time_col]])
  if (length(tt) < 2L) stop("At least two contrast time points are required.", call. = FALSE)
  ord <- order(tt)
  tt <- tt[ord]
  d <- contrast$draws[, ord, drop = FALSE]
  dt <- diff(tt)
  # Midpoint rule on the contrast grid: an interval counts when both adjacent
  # endpoint values satisfy the prespecified criterion. This is deliberately
  # conservative and deterministic.
  criterion <- switch(
    direction,
    above = function(x) x > threshold,
    below = function(x) x < threshold,
    absolute = function(x) abs(x) > abs(threshold)
  )
  hit <- criterion(d)
  interval_hit <- hit[, -1L, drop = FALSE] & hit[, -ncol(hit), drop = FALSE]
  duration <- as.numeric(interval_hit %*% dt)
  alpha <- (1 - probability) / 2
  qs <- stats::quantile(duration, probs = c(alpha, 0.5, 1 - alpha), names = FALSE, na.rm = TRUE)
  structure(
    list(
      summary = data.frame(
        direction = direction,
        threshold = threshold,
        mean = mean(duration),
        sd = stats::sd(duration),
        q_low = qs[[1L]],
        median = qs[[2L]],
        q_high = qs[[3L]],
        stringsAsFactors = FALSE
      ),
      draws = duration,
      direction = direction,
      threshold = threshold,
      probability = probability,
      time_unit = "same unit as prediction time",
      governance = "Threshold duration is valid only for a prespecified threshold; no automatic threshold or window search is performed."
    ),
    class = "gp3bayes_pupil_threshold_duration"
  )
}

#' @export
print.gp3bayes_pupil_trajectory_derivative <- function(x, ...) {
  cat("<gp3bayes_pupil_trajectory_derivative>\n")
  cat("  Order:", x$order, "\n")
  cat("  Grid rows:", nrow(x$grid), "\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_trajectory_derivative <- function(x, row.names = NULL, optional = FALSE, ...) {
  pupil_trajectory_derivative_table(x)
}

#' @export
print.gp3bayes_pupil_dynamic_contrast <- function(x, ...) {
  cat("<gp3bayes_pupil_dynamic_contrast>\n")
  cat("  Contrast:", paste(x$contrast, collapse = " - "), "\n")
  cat("  Derivative order:", x$derivative_order, "\n")
  cat("  Threshold:", x$threshold, "\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_dynamic_contrast <- function(x, row.names = NULL, optional = FALSE, ...) x$table

#' @export
print.gp3bayes_pupil_threshold_duration <- function(x, ...) {
  cat("<gp3bayes_pupil_threshold_duration>\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_threshold_duration <- function(x, row.names = NULL, optional = FALSE, ...) x$summary
