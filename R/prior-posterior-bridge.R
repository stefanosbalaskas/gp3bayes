# Declared-prior versus fitted-posterior bridge

.gp3pp_stop <- function(...) stop(paste0(...), call. = FALSE)

.gp3pp_prior_object <- function(x) {
  if (inherits(x, "gp3bayes_prior_specification")) return(x)
  if (inherits(x, "gp3bayes_fit")) {
    z <- x$specification$priors
    if (inherits(z, "gp3bayes_prior_specification")) return(z)
  }
  if (inherits(x, "gp3bayes_model_specification")) {
    z <- x$priors
    if (inherits(z, "gp3bayes_prior_specification")) return(z)
  }
  .gp3pp_stop(
    "`x` must be a gp3bayes fit, model specification, or prior specification."
  )
}

.gp3pp_class_for_variable <- function(variable) {
  if (variable %in% c("b_Intercept", "Intercept")) return("Intercept")
  if (grepl("^b_", variable)) return("b")
  if (grepl("^sd_", variable)) return("sd")
  if (grepl("^cor_", variable)) return("cor")
  if (identical(variable, "sigma")) return("sigma")
  NA_character_
}

.gp3pp_prior_row <- function(priors, parameter_class) {
  rows <- priors$table$parameter_class == parameter_class
  if (sum(rows) != 1L) {
    .gp3pp_stop("No unique declared prior for class `", parameter_class, "`.")
  }
  priors$table[rows, , drop = FALSE]
}

.gp3pp_integer <- function(x, name, minimum = 0L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x != floor(x) || x < minimum) {
    .gp3pp_stop("`", name, "` must be one integer >= ", minimum, ".")
  }
  as.integer(x)
}

.gp3pp_sample_one <- function(row, n) {
  distribution <- as.character(row$distribution[[1L]])
  if (distribution == "normal") {
    return(stats::rnorm(n, row$location[[1L]], row$scale[[1L]]))
  }
  if (distribution == "student_t") {
    z <- row$location[[1L]] +
      row$scale[[1L]] * stats::rt(n, df = row$df[[1L]])
    lower <- row$lower[[1L]]
    if (is.finite(lower) && lower >= 0) z <- abs(z)
    return(z)
  }
  if (distribution == "lkj") {
    eta <- row$shape[[1L]]
    return(2 * stats::rbeta(n, eta, eta) - 1)
  }
  .gp3pp_stop("Unsupported declared prior distribution `", distribution, "`.")
}

#' Prior-Specification Table
#'
#' @param x A gp3bayes fit, specification, or prior specification.
#' @return The backend-independent declared prior table.
#' @export
prior_specification_table <- function(x) {
  .gp3pp_prior_object(x)$table
}

#' Simulate Marginal Draws from Declared gp3bayes Priors
#'
#' @param x A gp3bayes fit, specification, or prior specification.
#' @param variables Posterior-style variable names. When `x` is a fitted model,
#'   supported variables can be inferred.
#' @param regex Optional regular expression applied after inference.
#' @param ndraws Number of marginal prior draws.
#' @param seed Non-negative integer seed.
#' @return A numeric draw matrix.
#' @export
simulate_declared_prior_draws <- function(
  x,
  variables = NULL,
  regex = NULL,
  ndraws = 4000L,
  seed = 1L
) {
  priors <- .gp3pp_prior_object(x)
  ndraws <- .gp3pp_integer(ndraws, "ndraws", 50L)
  seed <- .gp3pp_integer(seed, "seed", 0L)

  if (is.null(variables)) {
    if (!inherits(x, "gp3bayes_fit")) {
      .gp3pp_stop("`variables` is required when `x` is not a fitted model.")
    }
    m <- as.matrix(extract_posterior_draws(x, format = "matrix"))
    variables <- colnames(m)
    ok <- vapply(
      variables,
      function(v) !is.na(.gp3pp_class_for_variable(v)),
      logical(1L)
    )
    variables <- variables[ok]
  }

  if (!is.character(variables) || !length(variables) || anyNA(variables)) {
    .gp3pp_stop("`variables` must contain posterior-style variable names.")
  }
  variables <- unique(variables)
  if (!is.null(regex)) variables <- variables[grepl(regex, variables, perl = TRUE)]
  if (!length(variables)) .gp3pp_stop("No supported prior variables remain.")

  classes <- vapply(variables, .gp3pp_class_for_variable, character(1L))
  if (anyNA(classes)) {
    .gp3pp_stop(
      "Unsupported variables: ",
      paste(variables[is.na(classes)], collapse = ", "),
      "."
    )
  }

  missing_classes <- setdiff(unique(classes), priors$table$parameter_class)
  if (length(missing_classes)) {
    .gp3pp_stop(
      "Declared prior does not contain: ",
      paste(missing_classes, collapse = ", "),
      "."
    )
  }

  out <- withr::with_seed(seed, {
    do.call(
      cbind,
      lapply(seq_along(variables), function(i) {
        .gp3pp_sample_one(.gp3pp_prior_row(priors, classes[[i]]), ndraws)
      })
    )
  })
  colnames(out) <- variables
  out
}

.gp3pp_summary <- function(z, probs) {
  q <- stats::quantile(z, probs = probs, names = FALSE)
  c(
    mean = mean(z),
    sd = stats::sd(z),
    lower = q[[1L]],
    median = q[[2L]],
    upper = q[[3L]]
  )
}

.gp3pp_overlap <- function(a1, a2, b1, b2) {
  inter <- max(0, min(a2, b2) - max(a1, b1))
  union <- max(a2, b2) - min(a1, b1)
  if (!is.finite(union) || union <= 0) return(NA_real_)
  inter / union
}

.gp3pp_ks <- function(a, b) {
  grid <- sort(unique(c(a, b)))
  if (length(grid) > 5000L) {
    p <- seq(0.001, 0.999, length.out = 5000L)
    grid <- unique(c(
      stats::quantile(a, p, names = FALSE),
      stats::quantile(b, p, names = FALSE)
    ))
  }
  max(abs(stats::ecdf(a)(grid) - stats::ecdf(b)(grid)))
}

.gp3pp_wasserstein <- function(a, b) {
  p <- seq(0.001, 0.999, length.out = 999L)
  mean(abs(
    stats::quantile(a, p, names = FALSE) -
      stats::quantile(b, p, names = FALSE)
  ))
}

#' Create a Declared-Prior versus Posterior Bridge
#'
#' Compares marginal draws from the recorded gp3bayes prior specification with
#' fitted posterior draws on the same parameter scale.
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param variables Optional exact posterior variables.
#' @param regex Optional posterior-variable regular expression.
#' @param ndraws Number of prior draws and maximum posterior draws used.
#' @param probs Three interval probabilities.
#' @param seed Non-negative integer seed.
#' @return A `gp3bayes_prior_posterior_bridge`.
#' @export
prior_posterior_bridge <- function(
  fit,
  variables = NULL,
  regex = "^(b_|sd_|cor_|sigma$)",
  ndraws = 4000L,
  probs = c(0.025, 0.5, 0.975),
  seed = 1L
) {
  .gp3p_validate_fit(fit)
  ndraws <- .gp3pp_integer(ndraws, "ndraws", 50L)
  seed <- .gp3pp_integer(seed, "seed", 0L)
  probs <- .gp3p_probs(probs)

  posterior <- as.matrix(extract_posterior_draws(
    fit,
    variables = variables,
    regex = regex,
    format = "matrix"
  ))
  ok <- vapply(
    colnames(posterior),
    function(v) !is.na(.gp3pp_class_for_variable(v)),
    logical(1L)
  )
  posterior <- posterior[, ok, drop = FALSE]
  if (!ncol(posterior)) .gp3pp_stop("No supported posterior variables remain.")

  if (nrow(posterior) > ndraws) {
    ids <- withr::with_seed(
      seed + 1L,
      sort(sample.int(nrow(posterior), ndraws, replace = FALSE))
    )
    posterior <- posterior[ids, , drop = FALSE]
  }

  prior <- simulate_declared_prior_draws(
    fit,
    variables = colnames(posterior),
    ndraws = ndraws,
    seed = seed
  )

  summary_rows <- lapply(colnames(posterior), function(variable) {
    a <- prior[, variable]
    b <- posterior[, variable]
    pa <- .gp3pp_summary(a, probs)
    pb <- .gp3pp_summary(b, probs)
    prior_sd <- unname(pa[["sd"]])
    post_sd <- unname(pb[["sd"]])
    ratio <- if (is.finite(prior_sd) && prior_sd > 0) post_sd / prior_sd else NA_real_

    data.frame(
      variable = variable,
      parameter_class = .gp3pp_class_for_variable(variable),
      prior_mean = unname(pa[["mean"]]),
      prior_sd = prior_sd,
      prior_lower = unname(pa[["lower"]]),
      prior_median = unname(pa[["median"]]),
      prior_upper = unname(pa[["upper"]]),
      posterior_mean = unname(pb[["mean"]]),
      posterior_sd = post_sd,
      posterior_lower = unname(pb[["lower"]]),
      posterior_median = unname(pb[["median"]]),
      posterior_upper = unname(pb[["upper"]]),
      median_shift = unname(pb[["median"]] - pa[["median"]]),
      standardized_location_shift = if (
        is.finite(prior_sd) && prior_sd > 0
      ) unname((pb[["median"]] - pa[["median"]]) / prior_sd) else NA_real_,
      sd_ratio = ratio,
      contraction = if (is.finite(ratio)) 1 - ratio else NA_real_,
      interval_overlap_fraction = .gp3pp_overlap(
        pa[["lower"]], pa[["upper"]], pb[["lower"]], pb[["upper"]]
      ),
      stringsAsFactors = FALSE
    )
  })

  distance_rows <- lapply(colnames(posterior), function(variable) {
    a <- prior[, variable]
    b <- posterior[, variable]
    scale <- stats::sd(a)
    w <- .gp3pp_wasserstein(a, b)
    data.frame(
      variable = variable,
      ks_distance = .gp3pp_ks(a, b),
      quantile_wasserstein = w,
      standardized_quantile_wasserstein = if (
        is.finite(scale) && scale > 0
      ) w / scale else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  structure(
    list(
      bridge_version = "0.3",
      family = fit$family,
      variables = colnames(posterior),
      prior_draws = prior,
      posterior_draws = posterior,
      summary = do.call(rbind, summary_rows),
      distances = do.call(rbind, distance_rows),
      probs = probs,
      seed = seed,
      marginal_prior_simulation = TRUE,
      backend_saved_prior_draws_required = FALSE,
      automatic_prior_decision = FALSE,
      interpretation = paste(
        "Shift, contraction, interval overlap, and distribution distances are",
        "descriptive marginal comparisons and do not establish prior adequacy."
      )
    ),
    class = "gp3bayes_prior_posterior_bridge"
  )
}

#' @export
print.gp3bayes_prior_posterior_bridge <- function(x, ...) {
  cat("\ngp3bayes declared-prior versus posterior bridge\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Variables: ", length(x$variables), "\n", sep = "")
  cat(" Automatic prior decision: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_prior_posterior_bridge <- function(x, ...) x$summary

#' Prior-versus-Posterior Summary Table
#'
#' @param x A `gp3bayes_prior_posterior_bridge`.
#' @return Location, spread, interval, shift, and contraction summaries.
#' @export
prior_posterior_summary_table <- function(x) {
  if (!inherits(x, "gp3bayes_prior_posterior_bridge")) {
    .gp3pp_stop("`x` must be a gp3bayes prior-posterior bridge.")
  }
  x$summary
}

#' Prior-versus-Posterior Distance Table
#'
#' @param x A `gp3bayes_prior_posterior_bridge`.
#' @return Marginal empirical distribution-distance summaries.
#' @export
prior_posterior_distance_table <- function(x) {
  if (!inherits(x, "gp3bayes_prior_posterior_bridge")) {
    .gp3pp_stop("`x` must be a gp3bayes prior-posterior bridge.")
  }
  x$distances
}

#' Long Prior and Posterior Draw Table
#'
#' @param x A `gp3bayes_prior_posterior_bridge`.
#' @param max_draws Maximum draws per distribution and variable.
#' @param seed Non-negative integer seed.
#' @return A long draw table.
#' @export
prior_posterior_draws_long <- function(x, max_draws = 1000L, seed = 1L) {
  if (!inherits(x, "gp3bayes_prior_posterior_bridge")) {
    .gp3pp_stop("`x` must be a gp3bayes prior-posterior bridge.")
  }
  max_draws <- .gp3pp_integer(max_draws, "max_draws", 50L)
  seed <- .gp3pp_integer(seed, "seed", 0L)

  one <- function(m, distribution, offset) {
    ids <- seq_len(nrow(m))
    if (length(ids) > max_draws) {
      ids <- withr::with_seed(
        seed + offset,
        sort(sample.int(length(ids), max_draws, replace = FALSE))
      )
    }
    m <- m[ids, , drop = FALSE]
    data.frame(
      draw = rep(seq_len(nrow(m)), times = ncol(m)),
      variable = rep(colnames(m), each = nrow(m)),
      distribution = distribution,
      value = as.vector(m),
      stringsAsFactors = FALSE
    )
  }

  rbind(
    one(x$prior_draws, "prior", 0L),
    one(x$posterior_draws, "posterior", 1L)
  )
}

#' Plot Declared Prior and Posterior Densities
#'
#' @param x A prior-posterior bridge.
#' @param max_draws Maximum draws displayed per distribution.
#' @return A faceted `ggplot`.
#' @export
plot_prior_posterior_density <- function(x, max_draws = 1000L) {
  .gp3g_require("ggplot2", "plot prior and posterior densities")
  d <- prior_posterior_draws_long(x, max_draws = max_draws)
  ggplot2::ggplot(d, ggplot2::aes(x = value, linetype = distribution)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(
      x = "Parameter value",
      y = "Density",
      linetype = NULL,
      title = "Declared marginal prior and fitted posterior"
    ) +
    theme_gp3bayes()
}

#' Plot Prior and Posterior Intervals
#'
#' @param x A prior-posterior bridge.
#' @return A faceted `ggplot`.
#' @export
plot_prior_posterior_intervals <- function(x) {
  .gp3g_require("ggplot2", "plot prior and posterior intervals")
  d <- prior_posterior_summary_table(x)
  long <- rbind(
    data.frame(
      variable = d$variable,
      distribution = "prior",
      median = d$prior_median,
      lower = d$prior_lower,
      upper = d$prior_upper
    ),
    data.frame(
      variable = d$variable,
      distribution = "posterior",
      median = d$posterior_median,
      lower = d$posterior_lower,
      upper = d$posterior_upper
    )
  )
  ggplot2::ggplot(
    long,
    ggplot2::aes(x = distribution, y = median, ymin = lower, ymax = upper)
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = "Parameter value",
      title = "Declared prior and posterior intervals"
    ) +
    theme_gp3bayes()
}

#' Plot Prior-to-Posterior Location Shift
#'
#' @param x A prior-posterior bridge.
#' @return A `ggplot`.
#' @export
plot_prior_posterior_shift <- function(x) {
  .gp3g_require("ggplot2", "plot prior-to-posterior shift")
  d <- prior_posterior_summary_table(x)
  d$variable <- factor(d$variable, levels = rev(d$variable))
  ggplot2::ggplot(
    d,
    ggplot2::aes(x = variable, y = standardized_location_shift)
  ) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "Median shift / prior SD",
      title = "Prior-to-posterior location shift"
    ) +
    theme_gp3bayes()
}

#' Plot Prior-to-Posterior Contraction
#'
#' @param x A prior-posterior bridge.
#' @return A `ggplot`.
#' @export
plot_prior_posterior_contraction <- function(x) {
  .gp3g_require("ggplot2", "plot prior-to-posterior contraction")
  d <- prior_posterior_summary_table(x)
  d$variable <- factor(d$variable, levels = rev(d$variable))
  ggplot2::ggplot(d, ggplot2::aes(x = variable, y = contraction)) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = NULL,
      y = "1 - posterior SD / prior SD",
      title = "Prior-to-posterior marginal contraction"
    ) +
    theme_gp3bayes()
}

#' @export
plot.gp3bayes_prior_posterior_bridge <- function(
  x,
  type = c("density", "interval", "shift", "contraction"),
  ...
) {
  type <- match.arg(type)
  switch(
    type,
    density = plot_prior_posterior_density(x, ...),
    interval = plot_prior_posterior_intervals(x),
    shift = plot_prior_posterior_shift(x),
    contraction = plot_prior_posterior_contraction(x)
  )
}
