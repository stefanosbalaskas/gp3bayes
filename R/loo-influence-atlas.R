# Pointwise PSIS-LOO influence atlas

.gp3la_stop <- function(...) stop(paste0(...), call. = FALSE)

.gp3la_raw <- function(x) {
  raw <- if (inherits(x, "gp3bayes_psis_loo")) x$raw else x
  if (!inherits(raw, "loo")) {
    .gp3la_stop("`x` must contain a valid `loo` object.")
  }
  raw
}

#' Pointwise LOO Table
#'
#' @param x A gp3bayes PSIS-LOO or raw `loo` object.
#' @param data Optional observation-level data with matching rows.
#' @return Pointwise LOO estimates and Pareto-k diagnostics.
#' @export
loo_pointwise_table <- function(x, data = NULL) {
  .gp3p_require("loo", "extract pointwise LOO diagnostics")
  raw <- .gp3la_raw(x)
  pointwise <- as.data.frame(raw$pointwise, stringsAsFactors = FALSE)
  n <- nrow(pointwise)
  if (n < 1L) .gp3la_stop("No pointwise LOO observations are available.")

  k <- as.numeric(loo::pareto_k_values(raw))
  influence <- as.numeric(loo::pareto_k_influence_values(raw))
  if (length(k) != n || length(influence) != n) {
    .gp3la_stop("LOO pointwise and Pareto-k diagnostics differ in length.")
  }

  out <- data.frame(
    observation = seq_len(n),
    pointwise,
    pareto_k = k,
    influence_pareto_k = influence,
    flagged = !is.finite(k) | k >= 0.7,
    severe = is.finite(k) & k >= 1,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!is.null(data)) {
    if (!is.data.frame(data) || nrow(data) != n) {
      .gp3la_stop("`data` must have one row per LOO observation.")
    }
    out <- cbind(out, data)
  }
  out
}

#' LOO Influence Summary
#'
#' @param x A LOO object or pointwise LOO table.
#' @return A one-row influence summary.
#' @export
loo_influence_summary <- function(x) {
  d <- if (is.data.frame(x) && "pareto_k" %in% names(x)) {
    x
  } else {
    loo_pointwise_table(x)
  }
  finite <- d$pareto_k[is.finite(d$pareto_k)]
  q <- if (length(finite)) {
    stats::quantile(finite, c(0.5, 0.9, 0.95, 0.99), names = FALSE)
  } else {
    rep(NA_real_, 4L)
  }
  data.frame(
    observations = nrow(d),
    finite_pareto_k = length(finite),
    median_pareto_k = q[[1L]],
    p90_pareto_k = q[[2L]],
    p95_pareto_k = q[[3L]],
    p99_pareto_k = q[[4L]],
    flagged_k_ge_0_7 = sum(is.finite(d$pareto_k) & d$pareto_k >= 0.7),
    severe_k_ge_1 = sum(is.finite(d$pareto_k) & d$pareto_k >= 1),
    automatic_exclusion = FALSE,
    stringsAsFactors = FALSE
  )
}

#' Flagged LOO Observation Data
#'
#' @param x A LOO object or pointwise table.
#' @param threshold Explicit Pareto-k threshold.
#' @return Rows meeting the requested threshold.
#' @export
loo_flagged_data <- function(x, threshold = 0.7) {
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || !is.finite(threshold)) {
    .gp3la_stop("`threshold` must be one finite number.")
  }
  d <- if (is.data.frame(x) && "pareto_k" %in% names(x)) {
    x
  } else {
    loo_pointwise_table(x)
  }
  d[is.finite(d$pareto_k) & d$pareto_k >= threshold, , drop = FALSE]
}

#' Plot Pointwise LOO ELPD
#'
#' @param x A LOO object or pointwise table.
#' @return A `ggplot`.
#' @export
plot_loo_pointwise_elpd <- function(x) {
  .gp3g_require("ggplot2", "plot pointwise LOO ELPD")
  d <- if (is.data.frame(x)) x else loo_pointwise_table(x)
  if (!"elpd_loo" %in% names(d)) .gp3la_stop("`elpd_loo` is unavailable.")
  ggplot2::ggplot(d, ggplot2::aes(x = observation, y = elpd_loo)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Observation",
      y = "Pointwise ELPD-LOO",
      title = "Pointwise leave-one-out predictive contribution"
    ) +
    theme_gp3bayes()
}

#' Plot Pareto-k versus Pointwise ELPD
#'
#' @param x A LOO object or pointwise table.
#' @return A `ggplot`.
#' @export
plot_loo_pareto_vs_elpd <- function(x) {
  .gp3g_require("ggplot2", "plot Pareto-k against ELPD")
  d <- if (is.data.frame(x)) x else loo_pointwise_table(x)
  if (!all(c("pareto_k", "elpd_loo") %in% names(d))) {
    .gp3la_stop("`pareto_k` and `elpd_loo` are required.")
  }
  ggplot2::ggplot(d, ggplot2::aes(x = pareto_k, y = elpd_loo)) +
    ggplot2::geom_vline(xintercept = c(0.5, 0.7, 1), linetype = c(3, 2, 1)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Pareto k",
      y = "Pointwise ELPD-LOO",
      title = "LOO influence and predictive contribution"
    ) +
    theme_gp3bayes()
}

#' Plot Ranked LOO Influence
#'
#' @param x A LOO object or pointwise table.
#' @return A `ggplot`.
#' @export
plot_loo_influence_rank <- function(x) {
  .gp3g_require("ggplot2", "plot ranked LOO influence")
  d <- if (is.data.frame(x)) x else loo_pointwise_table(x)
  d <- d[order(d$pareto_k, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  d$influence_rank <- seq_len(nrow(d))
  ggplot2::ggplot(d, ggplot2::aes(x = influence_rank, y = pareto_k)) +
    ggplot2::geom_hline(yintercept = c(0.5, 0.7, 1), linetype = c(3, 2, 1)) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "Influence rank",
      y = "Pareto k",
      title = "Ranked PSIS-LOO influence"
    ) +
    theme_gp3bayes()
}

#' Create a LOO Influence Atlas
#'
#' @param x A gp3bayes PSIS-LOO or raw `loo` object.
#' @param data Optional observation-level data.
#' @param threshold Pareto-k threshold used for the flagged table.
#' @return A `gp3bayes_loo_influence_atlas`.
#' @export
create_loo_influence_atlas <- function(x, data = NULL, threshold = 0.7) {
  table <- loo_pointwise_table(x, data = data)
  structure(
    list(
      atlas_version = "0.3",
      threshold = threshold,
      table = table,
      flagged = loo_flagged_data(table, threshold),
      summary = loo_influence_summary(table),
      automatic_exclusion = FALSE,
      interpretation = paste(
        "Pointwise predictive contributions and PSIS influence are reported",
        "together. Flagged rows request review and are not excluded automatically."
      )
    ),
    class = "gp3bayes_loo_influence_atlas"
  )
}

#' @export
print.gp3bayes_loo_influence_atlas <- function(x, ...) {
  cat("\ngp3bayes LOO influence atlas\n")
  cat(" Observations: ", nrow(x$table), "\n", sep = "")
  cat(" Flagged: ", nrow(x$flagged), "\n", sep = "")
  cat(" Automatic exclusion: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_loo_influence_atlas <- function(x, ...) x$table

#' LOO Influence Atlas Table
#'
#' @param x A `gp3bayes_loo_influence_atlas`.
#' @return The complete pointwise atlas table.
#' @export
loo_influence_atlas_table <- function(x) {
  if (!inherits(x, "gp3bayes_loo_influence_atlas")) {
    .gp3la_stop("`x` must be a gp3bayes LOO influence atlas.")
  }
  x$table
}

#' @export
plot.gp3bayes_loo_influence_atlas <- function(
  x,
  type = c("rank", "elpd", "pareto_elpd"),
  ...
) {
  type <- match.arg(type)
  switch(
    type,
    rank = plot_loo_influence_rank(x$table),
    elpd = plot_loo_pointwise_elpd(x$table),
    pareto_elpd = plot_loo_pareto_vs_elpd(x$table)
  )
}
