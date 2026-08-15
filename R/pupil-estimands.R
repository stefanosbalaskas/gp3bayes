.gp3p_validate_draw_matrix <- function(draws, n_grid = NULL, max_cells = 5000000L) {
  if (!is.matrix(draws) || !is.numeric(draws) || !nrow(draws) || !ncol(draws)) {
    .gp3p_stop("`draws` must be a non-empty numeric matrix with draws in rows.")
  }
  if (any(!is.finite(draws))) .gp3p_stop("Prediction draws must be finite.")
  if (!is.null(n_grid) && ncol(draws) != n_grid) {
    .gp3p_stop("Prediction draw columns must equal the number of grid rows.")
  }
  if (length(draws) > max_cells) {
    .gp3p_stop("Prediction object exceeds the allowed draw-by-grid cell count.")
  }
  draws
}

#' Create a lightweight pupil prediction object from frozen draws
#'
#' Wraps already-computed posterior prediction draws for examples, reporting,
#' and reproducible post-fit analysis without pretending that fitting occurred
#' in the current session.
#'
#' @param draws Numeric matrix with posterior draws in rows and grid points in
#'   columns.
#' @param grid Data frame with one row per draw column. It should contain
#'   `.event_time` and may contain `.condition`, `.participant`, and `.item`.
#' @param unit Declared pupil unit.
#' @param type Prediction type label.
#' @param max_cells Maximum draw-by-grid cells.
#' @return A `gp3bayes_pupil_prediction`.
#' @examples
#' grid <- expand.grid(
#'   .event_time = seq(0, 1, length.out = 5),
#'   .condition = factor(c("control", "treatment"))
#' )
#' draws <- matrix(rnorm(1000), nrow = 100, ncol = nrow(grid))
#' prediction <- as_pupil_prediction_draws(
#'   draws, grid, unit = "millimetres"
#' )
#' @export
as_pupil_prediction_draws <- function(
    draws, grid, unit,
    type = c("expected", "posterior_predictive", "linear"),
    max_cells = 5000000L) {
  type <- match.arg(type)
  unit <- .gp3p_match_unit(unit)
  if (!is.data.frame(grid) || !nrow(grid)) .gp3p_stop("`grid` must be a non-empty data frame.")
  if (!".event_time" %in% names(grid)) .gp3p_stop("`grid` must contain `.event_time`.")
  max_cells <- .gp3p_positive(max_cells, "max_cells", TRUE)
  draws <- .gp3p_validate_draw_matrix(draws, nrow(grid), max_cells)
  structure(
    list(
      prediction_version = "0.4-pupil-1",
      family = "pupil",
      type = type,
      draws = draws,
      grid = grid,
      unit = unit,
      ndraws = nrow(draws),
      n_grid = nrow(grid),
      source = "supplied_draws",
      fit_performed_here = FALSE
    ),
    class = "gp3bayes_pupil_prediction"
  )
}

.gp3p_prediction_grid <- function(fit, newdata = NULL, population_only = TRUE) {
  if (is.null(newdata)) {
    d <- fit$specification$prepared$data
    keep <- unique(c(
      ".event_time",
      if (".condition" %in% names(d)) ".condition",
      fit$specification$covariates
    ))
    grid <- unique(d[, keep, drop = FALSE])
    grid <- grid[order(grid$.event_time,
                       if (".condition" %in% names(grid)) grid$.condition else 1), ,
                 drop = FALSE]
    grid$.participant <- factor(levels(d$.participant)[1], levels = levels(d$.participant))
    grid$.series_id <- factor(levels(d$.series_id)[1], levels = levels(d$.series_id))
    grid$.sample_index <- stats::ave(grid$.event_time,
                              if (".condition" %in% names(grid)) grid$.condition else 1,
                              FUN = seq_along)
    if (".item" %in% names(d)) grid$.item <- factor(levels(d$.item)[1], levels = levels(d$.item))
  } else {
    if (!is.data.frame(newdata) || !nrow(newdata)) {
      .gp3p_stop("`newdata` must be NULL or a non-empty data frame.")
    }
    grid <- newdata
    .gp3p_check_columns(grid, ".event_time")
  }
  grid
}

#' Predict governed pupil trajectories
#'
#' Obtains expected, posterior-predictive, or linear-predictor draws from an
#' approved pupil fit with explicit draw and grid-size guards.
#'
#' @param fit A `gp3bayes_pupil_fit`.
#' @param newdata Optional prepared prediction grid. When omitted, a compact
#'   population grid is built from observed event times and conditions.
#'   Participant-conditioned prediction requires explicit `newdata`.
#' @param type `"expected"`, `"posterior_predictive"`, or `"linear"`.
#' @param ndraws Maximum posterior draws to retain.
#' @param population_only If `TRUE`, group-level effects are excluded from the
#'   prediction via `re_formula = NA`.
#' @param allow_new_levels Passed conservatively to brms prediction methods.
#' @param max_grid Maximum grid rows.
#' @param max_cells Maximum draw-by-grid cells.
#' @return A `gp3bayes_pupil_prediction`.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
predict_pupil_trajectory <- function(
    fit, newdata = NULL,
    type = c("expected", "posterior_predictive", "linear"),
    ndraws = 500L,
    population_only = TRUE,
    allow_new_levels = FALSE,
    max_grid = 5000L,
    max_cells = 5000000L) {

  if (!inherits(fit, "gp3bayes_pupil_fit") || !isTRUE(fit$fit_performed)) {
    .gp3p_stop("`fit` must be a fitted `gp3bayes_pupil_fit`.")
  }
  type <- match.arg(type)
  ndraws <- .gp3p_positive(ndraws, "ndraws", TRUE)
  population_only <- .gp3p_flag(population_only, "population_only")
  allow_new_levels <- .gp3p_flag(allow_new_levels, "allow_new_levels")
  max_grid <- .gp3p_positive(max_grid, "max_grid", TRUE)
  max_cells <- .gp3p_positive(max_cells, "max_cells", TRUE)
  if (!population_only && is.null(newdata)) {
    .gp3p_stop(
      "Participant-conditioned prediction requires explicit `newdata`; ",
      "gp3bayes does not silently choose a participant or item."
    )
  }
  grid <- .gp3p_prediction_grid(fit, newdata, population_only)
  if (nrow(grid) > max_grid) .gp3p_stop("Prediction grid exceeds `max_grid`.")
  if (nrow(grid) * ndraws > max_cells) {
    .gp3p_stop("Requested draw-by-grid expansion exceeds `max_cells`.")
  }

  re_formula <- if (population_only) NA else NULL
  fun <- switch(
    type,
    expected = brms::posterior_epred,
    posterior_predictive = brms::posterior_predict,
    linear = brms::posterior_linpred
  )
  draws <- fun(
    fit$backend_fit,
    newdata = grid,
    ndraws = ndraws,
    re_formula = re_formula,
    allow_new_levels = allow_new_levels,
    incl_autocor = !population_only
  )
  if (length(dim(draws)) != 2L) {
    draws <- matrix(draws, nrow = dim(draws)[1L])
  }
  as_pupil_prediction_draws(
    draws = draws,
    grid = grid,
    unit = fit$outcome_unit,
    type = type,
    max_cells = max_cells
  )
}

.gp3p_group_key <- function(grid, exclude = ".event_time") {
  candidates <- intersect(c(".condition", ".participant", ".item"), names(grid))
  setdiff(candidates, exclude)
}

.gp3p_quantiles <- function(x, probability) {
  alpha <- (1 - probability) / 2
  stats::quantile(x, probs = c(alpha, 0.5, 1 - alpha),
                  names = FALSE, type = 8)
}

.gp3p_trapezoid <- function(x, y) {
  o <- order(x)
  x <- x[o]; y <- y[o]
  if (length(x) < 2L) return(NA_real_)
  sum(diff(x) * (y[-length(y)] + y[-1L]) / 2)
}

#' Estimate posterior pupil trajectories
#'
#' Summarises a finite, declared prediction grid with pointwise or grid-wise
#' simultaneous posterior bands.
#'
#' @param prediction A `gp3bayes_pupil_prediction`.
#' @param probability Credible probability.
#' @param interval `"pointwise"` or `"simultaneous"`.
#' @return A `gp3bayes_pupil_trajectory`.
#' @section Uncertainty:
#' `"simultaneous"` constructs a grid-wise band from the empirical posterior
#' maximum standardized deviation over the supplied finite grid. It is not a
#' universal continuous-time confidence band.
#' @examples
#' grid <- data.frame(.event_time = seq(0, 1, length.out = 5))
#' draws <- matrix(rnorm(500), nrow = 100)
#' prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
#' estimate_pupil_trajectory(prediction)
#' @export
estimate_pupil_trajectory <- function(
    prediction, probability = 0.95,
    interval = c("pointwise", "simultaneous")) {
  if (!inherits(prediction, "gp3bayes_pupil_prediction")) {
    .gp3p_stop("`prediction` must be a pupil prediction object.")
  }
  probability <- .gp3p_probability(probability, "probability", open = TRUE)
  interval <- match.arg(interval)
  draws <- prediction$draws
  grid <- prediction$grid
  q <- apply(draws, 2, .gp3p_quantiles, probability = probability)
  tab <- grid
  tab$estimate <- colMeans(draws)
  tab$median <- q[2, ]
  tab$lower <- q[1, ]
  tab$upper <- q[3, ]

  if (identical(interval, "simultaneous")) {
    mu <- colMeans(draws)
    s <- apply(draws, 2, stats::sd)
    s[!is.finite(s) | s == 0] <- 1
    zmax <- apply(abs(sweep(sweep(draws, 2, mu, "-"), 2, s, "/")), 1, max)
    cval <- unname(stats::quantile(zmax, probability, type = 8))
    tab$lower <- mu - cval * s
    tab$upper <- mu + cval * s
  }
  structure(
    list(
      table = tab, unit = prediction$unit, probability = probability,
      interval = interval, prediction_type = prediction$type,
      finite_grid_qualification = identical(interval, "simultaneous")
    ),
    class = c("gp3bayes_pupil_trajectory", "gp3bayes_pupil_estimand")
  )
}

#' Convert a pupil trajectory to a table
#' @param x A pupil trajectory object.
#' @return A data frame.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
pupil_trajectory_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_trajectory")) .gp3p_stop("`x` must be a pupil trajectory.")
  x$table
}

.gp3p_prediction_groups <- function(prediction) {
  grid <- prediction$grid
  group_cols <- intersect(c(".condition", ".participant", ".item"), names(grid))
  if (!length(group_cols)) return(rep("all", nrow(grid)))
  interaction(grid[group_cols], drop = TRUE, lex.order = TRUE)
}

.gp3p_summary_by_draw <- function(prediction, window, statistic) {
  grid <- prediction$grid
  sel <- grid$.event_time >= window[1] & grid$.event_time <= window[2]
  if (!any(sel)) .gp3p_stop("The declared window contains no prediction-grid points.")
  groups <- .gp3p_prediction_groups(
    as_pupil_prediction_draws(prediction$draws[, sel, drop = FALSE],
                              grid[sel, , drop = FALSE], prediction$unit,
                              prediction$type)
  )
  lev <- unique(as.character(groups))
  out <- list()
  for (g in lev) {
    idx <- which(as.character(groups) == g)
    sub_grid <- grid[sel, , drop = FALSE][idx, , drop = FALSE]
    sub_draws <- prediction$draws[, sel, drop = FALSE][, idx, drop = FALSE]
    values <- switch(
      statistic,
      mean = rowMeans(sub_draws),
      auc = apply(sub_draws, 1, function(y) .gp3p_trapezoid(sub_grid$.event_time, y)),
      peak = apply(sub_draws, 1, max),
      peak_latency = apply(sub_draws, 1, function(y) sub_grid$.event_time[which.max(y)])
    )
    meta_cols <- intersect(c(".condition", ".participant", ".item"), names(sub_grid))
    meta <- if (length(meta_cols)) sub_grid[1, meta_cols, drop = FALSE] else data.frame(group = "all")
    out[[g]] <- list(meta = meta, draws = values)
  }
  out
}

.gp3p_estimand_table <- function(grouped, probability, estimand, unit, window) {
  rows <- lapply(grouped, function(z) {
    q <- .gp3p_quantiles(z$draws, probability)
    cbind(
      z$meta,
      data.frame(
        estimand = estimand,
        estimate = mean(z$draws),
        median = q[2], lower = q[1], upper = q[3],
        window_start = window[1], window_end = window[2],
        stringsAsFactors = FALSE
      )
    )
  })
  tab <- do.call(rbind, rows)
  rownames(tab) <- NULL
  structure(
    list(table = tab, draws = grouped, probability = probability,
         unit = unit, window = window, estimand = estimand,
         confirmatory_status = "requires_prespecified_window"),
    class = c("gp3bayes_pupil_estimand", paste0("gp3bayes_pupil_", estimand))
  )
}

.gp3p_validate_window <- function(window) {
  if (!is.numeric(window) || length(window) != 2L || anyNA(window) ||
      any(!is.finite(window)) || window[1] >= window[2]) {
    .gp3p_stop("`window` must contain two finite increasing values.")
  }
  as.numeric(window)
}

#' Estimate a declared-window mean pupil response
#' @param prediction A pupil prediction object.
#' @param window Prespecified event-relative time window.
#' @param probability Credible probability.
#' @return A `gp3bayes_pupil_estimand`.
#' @examples
#' grid <- data.frame(.event_time = seq(0, 1, length.out = 5))
#' draws <- matrix(rnorm(500), nrow = 100)
#' prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
#' estimate_pupil_window(prediction, c(0.2, 0.8))
#' @export
estimate_pupil_window <- function(prediction, window, probability = 0.95) {
  if (!inherits(prediction, "gp3bayes_pupil_prediction")) .gp3p_stop("Invalid pupil prediction.")
  window <- .gp3p_validate_window(window)
  probability <- .gp3p_probability(probability, "probability", TRUE)
  .gp3p_estimand_table(.gp3p_summary_by_draw(prediction, window, "mean"),
                       probability, "window_mean", prediction$unit, window)
}

#' Estimate area under a declared pupil-response window
#' @inheritParams estimate_pupil_window
#' @return A pupil estimand. AUC units are pupil-unit times time-unit.
#' @examples
#' grid <- data.frame(.event_time = seq(0, 1, length.out = 5))
#' draws <- matrix(rnorm(500), nrow = 100)
#' prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
#' estimate_pupil_auc(prediction, c(0.2, 0.8))
#' @export
estimate_pupil_auc <- function(prediction, window, probability = 0.95) {
  if (!inherits(prediction, "gp3bayes_pupil_prediction")) .gp3p_stop("Invalid pupil prediction.")
  window <- .gp3p_validate_window(window)
  probability <- .gp3p_probability(probability, "probability", TRUE)
  .gp3p_estimand_table(.gp3p_summary_by_draw(prediction, window, "auc"),
                       probability, "auc", prediction$unit, window)
}

#' Estimate posterior peak pupil response inside a declared window
#' @inheritParams estimate_pupil_window
#' @return A pupil estimand with posterior uncertainty in the peak.
#' @examples
#' grid <- data.frame(.event_time = seq(0, 1, length.out = 5))
#' draws <- matrix(rnorm(500), nrow = 100)
#' prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
#' estimate_pupil_peak(prediction, c(0.2, 0.8))
#' @export
estimate_pupil_peak <- function(prediction, window, probability = 0.95) {
  if (!inherits(prediction, "gp3bayes_pupil_prediction")) .gp3p_stop("Invalid pupil prediction.")
  window <- .gp3p_validate_window(window)
  probability <- .gp3p_probability(probability, "probability", TRUE)
  .gp3p_estimand_table(.gp3p_summary_by_draw(prediction, window, "peak"),
                       probability, "peak", prediction$unit, window)
}

#' Estimate posterior peak latency inside a declared window
#' @inheritParams estimate_pupil_window
#' @return A pupil estimand with posterior uncertainty in peak latency.
#' @examples
#' grid <- data.frame(.event_time = seq(0, 1, length.out = 5))
#' draws <- matrix(rnorm(500), nrow = 100)
#' prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
#' estimate_pupil_peak_latency(prediction, c(0.2, 0.8))
#' @export
estimate_pupil_peak_latency <- function(prediction, window, probability = 0.95) {
  if (!inherits(prediction, "gp3bayes_pupil_prediction")) .gp3p_stop("Invalid pupil prediction.")
  window <- .gp3p_validate_window(window)
  probability <- .gp3p_probability(probability, "probability", TRUE)
  .gp3p_estimand_table(.gp3p_summary_by_draw(prediction, window, "peak_latency"),
                       probability, "peak_latency", "event_time", window)
}

#' Estimate a posterior condition-difference trajectory
#'
#' @param prediction Pupil prediction with a `.condition` column.
#' @param contrast Character vector `c(level_a, level_b)` defining `a - b`.
#' @param threshold Scientifically declared threshold on the pupil scale.
#' @param probability Credible probability.
#' @return A pupil trajectory/contrast object with pointwise probabilities that
#'   the declared contrast exceeds `threshold`.
#' @examples
#' grid <- expand.grid(
#'   .event_time = seq(0, 1, length.out = 5),
#'   .condition = factor(c("control", "treatment"))
#' )
#' draws <- matrix(rnorm(1000), nrow = 100, ncol = nrow(grid))
#' prediction <- as_pupil_prediction_draws(draws, grid, "millimetres")
#' pupil_condition_contrast(
#'   prediction, contrast = c("treatment", "control"), threshold = 0.1
#' )
#' @export
pupil_condition_contrast <- function(
    prediction, contrast, threshold = 0, probability = 0.95) {
  if (!inherits(prediction, "gp3bayes_pupil_prediction")) .gp3p_stop("Invalid pupil prediction.")
  if (!".condition" %in% names(prediction$grid)) {
    .gp3p_stop("Condition contrast requires `.condition` in the prediction grid.")
  }
  contrast <- .gp3p_character_vector(contrast, "contrast")
  if (length(contrast) != 2L) .gp3p_stop("`contrast` must contain exactly two condition levels.")
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) ||
      !is.finite(threshold)) .gp3p_stop("`threshold` must be one finite number.")
  probability <- .gp3p_probability(probability, "probability", TRUE)

  g <- prediction$grid
  cond <- as.character(g$.condition)
  ia <- which(cond == contrast[1])
  ib <- which(cond == contrast[2])
  if (!length(ia) || !length(ib)) .gp3p_stop("Both requested contrast levels must occur.")
  key_cols <- setdiff(intersect(c(".event_time", ".participant", ".item"), names(g)), ".condition")
  key <- function(idx) do.call(paste, c(g[idx, key_cols, drop = FALSE], sep = "\r"))
  ka <- key(ia); kb <- key(ib)
  common <- intersect(ka, kb)
  if (!length(common)) .gp3p_stop("No matched prediction-grid points exist for the requested contrast.")
  ia <- ia[match(common, ka)]
  ib <- ib[match(common, kb)]
  diff_draws <- prediction$draws[, ia, drop = FALSE] -
    prediction$draws[, ib, drop = FALSE]
  q <- apply(diff_draws, 2, .gp3p_quantiles, probability = probability)
  tab <- g[ia, key_cols, drop = FALSE]
  tab$contrast <- paste0(contrast[1], " - ", contrast[2])
  tab$estimate <- colMeans(diff_draws)
  tab$median <- q[2, ]
  tab$lower <- q[1, ]
  tab$upper <- q[3, ]
  tab$threshold <- threshold
  tab$probability_gt_threshold <- colMeans(diff_draws > threshold)
  structure(
    list(table = tab, draws = diff_draws, contrast = contrast,
         threshold = threshold, probability = probability, unit = prediction$unit,
         exploratory_peak_search = FALSE),
    class = c("gp3bayes_pupil_condition_contrast",
              "gp3bayes_pupil_trajectory", "gp3bayes_pupil_estimand")
  )
}

#' Convert a pupil estimand to a data frame
#' @param x A `gp3bayes_pupil_estimand`.
#' @param ... Ignored.
#' @return The estimand table.
#' @export
as.data.frame.gp3bayes_pupil_estimand <- function(x, ...) x$table

#' @export
as.data.frame.gp3bayes_pupil_trajectory <- function(x, ...) x$table

#' @export
print.gp3bayes_pupil_estimand <- function(x, ...) {
  cat("<gp3bayes_pupil_estimand>\n")
  if (!is.null(x$estimand)) cat("  Estimand: ", x$estimand, "\n", sep = "")
  cat("  Rows: ", nrow(x$table), "\n", sep = "")
  if (!is.null(x$window)) cat("  Declared window: ", paste(x$window, collapse = " to "), "\n", sep = "")
  cat("  Automatic significance decision: FALSE\n")
  invisible(x)
}
