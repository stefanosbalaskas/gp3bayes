# Grouped aggregation of pointwise PSIS-LOO influence

.gp3li_stop <- function(...) stop(paste0(...), call. = FALSE)

.gp3li_table <- function(x, data = NULL) {
  if (inherits(x, "gp3bayes_loo_influence_atlas")) return(x$table)
  if (is.data.frame(x) && "pareto_k" %in% names(x)) return(x)
  loo_pointwise_table(x, data = data)
}

#' Aggregate LOO Influence by a Declared Group
#'
#' @param x A LOO influence atlas, pointwise LOO table, gp3bayes PSIS-LOO, or
#'   raw `loo` object.
#' @param group Grouping-column name.
#' @param data Optional observation-level data when needed.
#'
#' @return A descriptive group-level influence table.
#' @export
loo_group_influence_table <- function(x, group, data = NULL) {
  d <- .gp3li_table(x, data)
  if (!is.character(group) || length(group) != 1L || is.na(group) ||
      !group %in% names(d)) {
    .gp3li_stop("`group` must name one pointwise-table or supplied-data column.")
  }

  split_rows <- split(seq_len(nrow(d)), as.character(d[[group]]))
  rows <- lapply(names(split_rows), function(group_value) {
    idx <- split_rows[[group_value]]
    k <- d$pareto_k[idx]
    elpd <- if ("elpd_loo" %in% names(d)) d$elpd_loo[idx] else rep(NA_real_, length(idx))

    data.frame(
      group = group,
      group_value = group_value,
      observations = length(idx),
      mean_pareto_k = mean(k, na.rm = TRUE),
      max_pareto_k = max(k, na.rm = TRUE),
      flagged_k_ge_0_7 = sum(is.finite(k) & k >= 0.7),
      severe_k_ge_1 = sum(is.finite(k) & k >= 1),
      total_elpd_loo = if (all(is.na(elpd))) NA_real_ else sum(elpd, na.rm = TRUE),
      mean_elpd_loo = if (all(is.na(elpd))) NA_real_ else mean(elpd, na.rm = TRUE),
      automatic_group_exclusion = FALSE,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Plot Grouped LOO Influence
#'
#' @param x A grouped LOO influence table.
#' @return A `ggplot`.
#' @export
plot_loo_group_influence <- function(x) {
  .gp3g_require("ggplot2", "plot grouped LOO influence")
  if (!is.data.frame(x) ||
      !all(c("group_value", "max_pareto_k") %in% names(x))) {
    .gp3li_stop("`x` must be a grouped LOO influence table.")
  }

  d <- x
  d$group_value <- factor(
    d$group_value,
    levels = rev(d$group_value[order(d$max_pareto_k)])
  )

  ggplot2::ggplot(d, ggplot2::aes(x = group_value, y = max_pareto_k)) +
    ggplot2::geom_hline(yintercept = c(0.5, 0.7, 1), linetype = c(3, 2, 1)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Group",
      y = "Maximum Pareto k",
      title = "Grouped PSIS-LOO influence"
    ) +
    theme_gp3bayes()
}

#' Plot Grouped Pointwise ELPD Contribution
#'
#' @param x A grouped LOO influence table.
#' @return A `ggplot`.
#' @export
plot_loo_group_elpd <- function(x) {
  .gp3g_require("ggplot2", "plot grouped pointwise ELPD")
  if (!is.data.frame(x) ||
      !all(c("group_value", "total_elpd_loo") %in% names(x))) {
    .gp3li_stop("`x` must be a grouped LOO influence table.")
  }
  if (all(is.na(x$total_elpd_loo))) .gp3li_stop("Pointwise `elpd_loo` is unavailable.")

  d <- x
  d$group_value <- factor(
    d$group_value,
    levels = rev(d$group_value[order(d$total_elpd_loo)])
  )

  ggplot2::ggplot(d, ggplot2::aes(x = group_value, y = total_elpd_loo)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Group",
      y = "Sum of pointwise ELPD-LOO",
      title = "Grouped predictive contribution"
    ) +
    theme_gp3bayes()
}
