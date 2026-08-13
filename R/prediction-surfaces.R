# Prediction profiles, gradients, surfaces, and contrast profiles

.gp3ps_stop <- function(...) stop(paste0(...), call. = FALSE)

.gp3ps_numeric <- function(fit, variable) {
  .gp3p_validate_fit(fit)
  data <- .gp3p_training_data(fit)
  if (!is.character(variable) || length(variable) != 1L ||
      is.na(variable) || !variable %in% names(data)) {
    .gp3ps_stop("`variable` must name one prepared-data column.")
  }
  if (!is.numeric(data[[variable]])) {
    .gp3ps_stop("`", variable, "` must be numeric.")
  }
  data[[variable]]
}

.gp3ps_values <- function(template, values, n, variable) {
  if (is.null(values)) {
    n <- .gp3ha_int(n, "n", 2L)
    r <- range(template, na.rm = TRUE)
    if (!all(is.finite(r)) || r[[1L]] == r[[2L]]) {
      .gp3ps_stop("`", variable, "` has no usable numeric range.")
    }
    values <- seq(r[[1L]], r[[2L]], length.out = n)
  }
  if (!is.numeric(values) || length(values) < 2L ||
      anyNA(values) || any(!is.finite(values))) {
    .gp3ps_stop("Profile values must be finite numeric values.")
  }
  sort(unique(as.numeric(values)))
}

#' Create a Numeric Prediction Profile
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param variable Numeric predictor to vary.
#' @param values Optional explicit predictor values.
#' @param n Number of values when `values` is omitted.
#' @param at Named values holding other predictors fixed.
#' @param type Prediction quantity.
#' @param include_group_effects Whether group effects are included.
#' @param allow_new_levels Whether new grouping levels are permitted.
#' @param ndraws Optional posterior draws.
#' @param probs Three interval probabilities.
#' @param seed Predictive simulation seed where applicable.
#'
#' @return A `gp3bayes_prediction_profile`.
#' @export
create_prediction_profile <- function(
  fit,
  variable,
  values = NULL,
  n = 50L,
  at = list(),
  type = c("expected", "predictive", "linear", "median"),
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL,
  probs = c(0.025, 0.5, 0.975),
  seed = 1L
) {
  template <- .gp3ps_numeric(fit, variable)
  values <- .gp3ps_values(template, values, n, variable)
  type <- match.arg(type)

  if (!is.list(at) || (length(at) && is.null(names(at)))) {
    .gp3ps_stop("`at` must be a named list.")
  }
  at[[variable]] <- values

  grid <- create_prediction_grid(
    fit,
    variables = variable,
    at = at,
    max_rows = max(5000L, length(values))
  )
  grid <- grid[order(grid[[variable]]), , drop = FALSE]

  pred <- predict_model(
    fit,
    newdata = grid,
    type = type,
    include_group_effects = include_group_effects,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws,
    probs = probs,
    seed = seed
  )
  table <- pred$summary
  table$profile_x <- grid[[variable]]

  structure(
    list(
      variable = variable,
      table = table,
      prediction = pred,
      automatic_effect_interpretation = FALSE,
      interpretation = paste(
        "The profile is a fitted predictive description across requested values.",
        "It is not a causal response curve."
      )
    ),
    class = "gp3bayes_prediction_profile"
  )
}

#' Prediction Profile Table
#'
#' @param x A prediction profile.
#' @return Profile summaries.
#' @export
prediction_profile_table <- function(x) {
  if (!inherits(x, "gp3bayes_prediction_profile")) {
    .gp3ps_stop("`x` must be a gp3bayes prediction profile.")
  }
  x$table
}

#' Prediction-Profile Gradient Table
#'
#' @param x A prediction profile.
#' @param probs Three interval probabilities.
#' @return Finite-difference posterior predictive gradients.
#' @export
prediction_gradient_table <- function(
  x,
  probs = c(0.025, 0.5, 0.975)
) {
  if (!inherits(x, "gp3bayes_prediction_profile")) {
    .gp3ps_stop("`x` must be a gp3bayes prediction profile.")
  }
  probs <- .gp3p_probs(probs)
  values <- x$table$profile_x
  draws <- x$prediction$draws

  if (length(values) != ncol(draws) || any(diff(values) <= 0)) {
    .gp3ps_stop("Prediction-profile values must be strictly increasing.")
  }

  dx <- diff(values)
  slopes <- sweep(
    draws[, -1L, drop = FALSE] -
      draws[, -ncol(draws), drop = FALSE],
    2L,
    dx,
    "/"
  )
  q <- apply(slopes, 2L, stats::quantile, probs = probs, names = FALSE)
  if (is.null(dim(q))) q <- matrix(q, ncol = 1L)

  data.frame(
    variable = x$variable,
    lower_x = values[-length(values)],
    upper_x = values[-1L],
    gradient_midpoint = (values[-length(values)] + values[-1L]) / 2,
    gradient_mean = colMeans(slopes),
    gradient_lower = q[1L, ],
    gradient_median = q[2L, ],
    gradient_upper = q[3L, ],
    automatic_monotonicity_decision = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Plot a Prediction Profile
#'
#' @param x A prediction profile.
#' @return A `ggplot`.
#' @export
plot_prediction_profile <- function(x) {
  .gp3g_require("ggplot2", "plot prediction profiles")
  d <- prediction_profile_table(x)

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = profile_x, y = predicted_median)
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = lower, ymax = upper),
      alpha = 0.2
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = x$variable,
      y = "Posterior prediction",
      title = "Model-based prediction profile"
    ) +
    theme_gp3bayes()
}

#' Plot Prediction-Profile Gradients
#'
#' @param x A prediction profile or gradient table.
#' @return A `ggplot`.
#' @export
plot_prediction_gradient <- function(x) {
  .gp3g_require("ggplot2", "plot prediction gradients")
  d <- if (inherits(x, "gp3bayes_prediction_profile")) {
    prediction_gradient_table(x)
  } else {
    x
  }
  required <- c(
    "gradient_midpoint", "gradient_median", "gradient_lower", "gradient_upper"
  )
  if (!is.data.frame(d) || !all(required %in% names(d))) {
    .gp3ps_stop("`x` does not contain prediction-gradient summaries.")
  }

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = gradient_midpoint, y = gradient_median)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = gradient_lower, ymax = gradient_upper),
      alpha = 0.2
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = "Predictor midpoint",
      y = "Finite-difference predictive gradient",
      title = "Prediction-profile gradient"
    ) +
    theme_gp3bayes()
}

#' Create a Two-Dimensional Prediction Surface
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param x,y Numeric predictors.
#' @param x_values,y_values Optional explicit predictor values.
#' @param n Values per predictor when explicit values are omitted.
#' @param at Named values holding other predictors fixed.
#' @param type Prediction quantity.
#' @param include_group_effects Whether group effects are included.
#' @param allow_new_levels Whether new grouping levels are permitted.
#' @param ndraws Optional posterior draws.
#' @param probs Three interval probabilities.
#' @param seed Predictive simulation seed.
#' @param max_rows Maximum grid rows.
#'
#' @return A `gp3bayes_prediction_surface`.
#' @export
create_prediction_surface <- function(
  fit,
  x,
  y,
  x_values = NULL,
  y_values = NULL,
  n = 30L,
  at = list(),
  type = c("expected", "predictive", "linear", "median"),
  include_group_effects = FALSE,
  allow_new_levels = FALSE,
  ndraws = NULL,
  probs = c(0.025, 0.5, 0.975),
  seed = 1L,
  max_rows = 2500L
) {
  if (identical(x, y)) .gp3ps_stop("`x` and `y` must differ.")
  xv <- .gp3ps_values(.gp3ps_numeric(fit, x), x_values, n, x)
  yv <- .gp3ps_values(.gp3ps_numeric(fit, y), y_values, n, y)
  max_rows <- .gp3ha_int(max_rows, "max_rows", 4L)
  if (length(xv) * length(yv) > max_rows) {
    .gp3ps_stop("Prediction surface exceeds `max_rows`.")
  }
  type <- match.arg(type)

  if (!is.list(at) || (length(at) && is.null(names(at)))) {
    .gp3ps_stop("`at` must be a named list.")
  }
  at[[x]] <- xv
  at[[y]] <- yv

  grid <- create_prediction_grid(
    fit,
    variables = c(x, y),
    at = at,
    max_rows = max_rows
  )
  pred <- predict_model(
    fit,
    newdata = grid,
    type = type,
    include_group_effects = include_group_effects,
    allow_new_levels = allow_new_levels,
    ndraws = ndraws,
    probs = probs,
    seed = seed
  )
  table <- pred$summary
  table$surface_x <- grid[[x]]
  table$surface_y <- grid[[y]]
  table$interval_width <- table$upper - table$lower

  structure(
    list(
      x = x,
      y = y,
      table = table,
      prediction = pred,
      automatic_interaction_decision = FALSE,
      interpretation = paste(
        "The surface visualizes fitted predictive structure across a declared grid.",
        "It does not establish a causal interaction."
      )
    ),
    class = "gp3bayes_prediction_surface"
  )
}

#' Prediction Surface Table
#'
#' @param x A prediction surface.
#' @return Surface summaries.
#' @export
prediction_surface_table <- function(x) {
  if (!inherits(x, "gp3bayes_prediction_surface")) {
    .gp3ps_stop("`x` must be a gp3bayes prediction surface.")
  }
  x$table
}

#' Plot a Prediction Surface
#'
#' @param x A prediction surface.
#' @return A `ggplot`.
#' @export
plot_prediction_surface <- function(x) {
  .gp3g_require("ggplot2", "plot prediction surfaces")
  d <- prediction_surface_table(x)

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = surface_x, y = surface_y, fill = predicted_median)
  ) +
    ggplot2::geom_raster() +
    ggplot2::geom_contour(
      ggplot2::aes(z = predicted_median),
      colour = "black",
      alpha = 0.4
    ) +
    ggplot2::labs(
      x = x$x,
      y = x$y,
      fill = "Posterior median",
      title = "Model-based prediction surface"
    ) +
    theme_gp3bayes()
}

#' Plot Prediction-Surface Uncertainty
#'
#' @param x A prediction surface.
#' @return A `ggplot`.
#' @export
plot_prediction_surface_uncertainty <- function(x) {
  .gp3g_require("ggplot2", "plot prediction-surface uncertainty")
  d <- prediction_surface_table(x)

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = surface_x, y = surface_y, fill = interval_width)
  ) +
    ggplot2::geom_raster() +
    ggplot2::labs(
      x = x$x,
      y = x$y,
      fill = "Interval width",
      title = "Prediction-surface posterior uncertainty"
    ) +
    theme_gp3bayes()
}

#' Create a Prediction Contrast Profile
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param variable Numeric profile variable.
#' @param contrast_variable Variable defining two contrasted levels.
#' @param contrast_levels Optional two levels.
#' @param values Optional profile values.
#' @param n Number of values when `values` is omitted.
#' @param at Named values for other predictors.
#' @param measure `"difference"`, `"ratio"`, or `"odds_ratio"`.
#' @param include_group_effects Whether group effects are included.
#' @param ndraws Optional posterior draws.
#' @param probs Three interval probabilities.
#'
#' @return A `gp3bayes_prediction_contrast_profile`.
#' @export
create_prediction_contrast_profile <- function(
  fit,
  variable,
  contrast_variable,
  contrast_levels = NULL,
  values = NULL,
  n = 40L,
  at = list(),
  measure = c("difference", "ratio", "odds_ratio"),
  include_group_effects = FALSE,
  ndraws = NULL,
  probs = c(0.025, 0.5, 0.975)
) {
  template <- .gp3ps_numeric(fit, variable)
  data <- .gp3p_training_data(fit)
  if (!is.character(contrast_variable) || length(contrast_variable) != 1L ||
      is.na(contrast_variable) || !contrast_variable %in% names(data)) {
    .gp3ps_stop("`contrast_variable` must name one prepared-data column.")
  }

  values <- .gp3ps_values(template, values, n, variable)
  measure <- match.arg(measure)
  probs <- .gp3p_probs(probs)

  observed_levels <- if (is.factor(data[[contrast_variable]])) {
    levels(data[[contrast_variable]])
  } else {
    unique(as.character(data[[contrast_variable]]))
  }
  if (is.null(contrast_levels)) {
    if (length(observed_levels) != 2L) {
      .gp3ps_stop(
        "`contrast_levels` is required unless exactly two levels are observed."
      )
    }
    contrast_levels <- observed_levels
  }
  if (length(contrast_levels) != 2L || anyNA(contrast_levels)) {
    .gp3ps_stop("`contrast_levels` must contain exactly two values.")
  }

  if (!is.list(at) || (length(at) && is.null(names(at)))) {
    .gp3ps_stop("`at` must be a named list.")
  }
  at[[variable]] <- values
  at[[contrast_variable]] <- contrast_levels

  grid <- create_prediction_grid(
    fit,
    variables = c(variable, contrast_variable),
    at = at,
    max_rows = max(5000L, 2L * length(values))
  )
  pred <- predict_model(
    fit,
    newdata = grid,
    type = "expected",
    include_group_effects = include_group_effects,
    allow_new_levels = FALSE,
    ndraws = ndraws,
    probs = probs
  )

  contrast_text <- as.character(grid[[contrast_variable]])
  x_values <- grid[[variable]]
  contrast_draws <- matrix(
    NA_real_,
    nrow = nrow(pred$draws),
    ncol = length(values)
  )

  for (i in seq_along(values)) {
    a_row <- which(
      x_values == values[[i]] &
        contrast_text == as.character(contrast_levels[[1L]])
    )
    b_row <- which(
      x_values == values[[i]] &
        contrast_text == as.character(contrast_levels[[2L]])
    )
    if (length(a_row) != 1L || length(b_row) != 1L) {
      .gp3ps_stop("Contrast grid did not produce one row per level/value.")
    }

    a <- pred$draws[, a_row]
    b <- pred$draws[, b_row]

    contrast_draws[, i] <- if (measure == "difference") {
      b - a
    } else if (measure == "ratio") {
      if (any(a <= 0)) .gp3ps_stop("Ratio contrasts require positive denominators.")
      b / a
    } else {
      if (!identical(fit$family, "binary")) {
        .gp3ps_stop("Odds-ratio profiles require a binary fit.")
      }
      eps <- sqrt(.Machine$double.eps)
      a <- pmin(pmax(a, eps), 1 - eps)
      b <- pmin(pmax(b, eps), 1 - eps)
      (b / (1 - b)) / (a / (1 - a))
    }
  }

  q <- apply(
    contrast_draws,
    2L,
    stats::quantile,
    probs = probs,
    names = FALSE
  )
  if (is.null(dim(q))) q <- matrix(q, ncol = 1L)

  table <- data.frame(
    profile_x = values,
    contrast_level_1 = as.character(contrast_levels[[1L]]),
    contrast_level_2 = as.character(contrast_levels[[2L]]),
    measure = measure,
    contrast_mean = colMeans(contrast_draws),
    contrast_lower = q[1L, ],
    contrast_median = q[2L, ],
    contrast_upper = q[3L, ],
    probability_gt_reference = if (measure == "difference") {
      colMeans(contrast_draws > 0)
    } else {
      colMeans(contrast_draws > 1)
    },
    automatic_interaction_decision = FALSE,
    stringsAsFactors = FALSE
  )

  structure(
    list(
      variable = variable,
      contrast_variable = contrast_variable,
      contrast_levels = contrast_levels,
      measure = measure,
      table = table,
      draws = contrast_draws,
      prediction = pred,
      automatic_interaction_decision = FALSE,
      interpretation = paste(
        "The contrast profile is a fitted posterior comparison across requested",
        "values and does not establish a causal interaction."
      )
    ),
    class = "gp3bayes_prediction_contrast_profile"
  )
}

#' Prediction Contrast Profile Table
#'
#' @param x A prediction contrast profile.
#' @return Contrast summaries.
#' @export
prediction_contrast_profile_table <- function(x) {
  if (!inherits(x, "gp3bayes_prediction_contrast_profile")) {
    .gp3ps_stop("`x` must be a gp3bayes prediction contrast profile.")
  }
  x$table
}

#' Plot a Prediction Contrast Profile
#'
#' @param x A prediction contrast profile.
#' @return A `ggplot`.
#' @export
plot_prediction_contrast_profile <- function(x) {
  .gp3g_require("ggplot2", "plot prediction contrast profiles")
  d <- prediction_contrast_profile_table(x)
  reference <- if (identical(x$measure, "difference")) 0 else 1

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = profile_x, y = contrast_median)
  ) +
    ggplot2::geom_hline(yintercept = reference, linetype = 2) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = contrast_lower, ymax = contrast_upper),
      alpha = 0.2
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(
      x = x$variable,
      y = x$measure,
      title = paste0(
        "Prediction contrast profile: ",
        x$contrast_levels[[2L]],
        " versus ",
        x$contrast_levels[[1L]]
      )
    ) +
    theme_gp3bayes()
}
