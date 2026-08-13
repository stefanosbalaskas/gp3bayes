# Advanced hierarchical-effect diagnostics

.gp3ha_stop <- function(...) stop(paste0(...), call. = FALSE)

.gp3ha_int <- function(x, name, minimum = 1L) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) ||
      !is.finite(x) || x != floor(x) || x < minimum) {
    .gp3ha_stop("`", name, "` must be one integer >= ", minimum, ".")
  }
  as.integer(x)
}

.gp3ha_seed <- function(seed) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed < 0 || seed != floor(seed)) {
    .gp3ha_stop("`seed` must be one non-negative integer.")
  }
  as.integer(seed)
}

.gp3ha_ranef <- function(fit, groups = NULL) {
  .gp3p_validate_fit(fit)
  .gp3p_require("brms", "extract group-level posterior draws")
  re <- brms::ranef(fit$backend_fit, summary = FALSE)
  if (!is.null(groups)) {
    if (!is.character(groups) || anyNA(groups)) {
      .gp3ha_stop("`groups` must be NULL or character names.")
    }
    missing <- setdiff(groups, names(re))
    if (length(missing)) {
      .gp3ha_stop("Unknown grouping factors: ", paste(missing, collapse = ", "), ".")
    }
    re <- re[unique(groups)]
  }
  if (!length(re)) .gp3ha_stop("No group-level effects were returned.")
  re
}

#' Group-Level Posterior Draw Table
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param groups Optional grouping factors.
#' @param coefficients Optional group-level coefficients.
#' @param ndraws Optional number of draws retained.
#' @param seed Seed used only for draw subsampling.
#' @param max_rows Maximum permitted long-format rows.
#'
#' @return A long posterior draw table on the model linear-predictor scale.
#' @export
group_effect_draws_table <- function(
  fit,
  groups = NULL,
  coefficients = NULL,
  ndraws = NULL,
  seed = 1L,
  max_rows = 1000000L
) {
  re <- .gp3ha_ranef(fit, groups)
  seed <- .gp3ha_seed(seed)
  max_rows <- .gp3ha_int(max_rows, "max_rows")
  if (!is.null(ndraws)) ndraws <- .gp3ha_int(ndraws, "ndraws")
  if (!is.null(coefficients) &&
      (!is.character(coefficients) || anyNA(coefficients))) {
    .gp3ha_stop("`coefficients` must be NULL or character names.")
  }

  pieces <- list()
  k <- 0L
  total <- 0L

  for (g in names(re)) {
    a <- re[[g]]
    if (length(dim(a)) != 3L) next
    dn <- dimnames(a)
    lev <- dn[[2L]] %||% as.character(seq_len(dim(a)[[2L]]))
    coef <- dn[[3L]] %||% as.character(seq_len(dim(a)[[3L]]))
    keep_coef <- if (is.null(coefficients)) coef else intersect(coef, coefficients)
    if (!length(keep_coef)) next

    ids <- seq_len(dim(a)[[1L]])
    if (!is.null(ndraws) && length(ids) > ndraws) {
      ids <- withr::with_seed(
        seed,
        sort(sample.int(length(ids), ndraws, replace = FALSE))
      )
    }

    total <- total + length(ids) * length(lev) * length(keep_coef)
    if (total > max_rows) {
      .gp3ha_stop(
        "Requested draw table exceeds `max_rows`; reduce selectors/draws or ",
        "increase `max_rows` explicitly."
      )
    }

    for (cf in keep_coef) {
      k <- k + 1L
      m <- matrix(
        a[ids, , cf, drop = FALSE],
        nrow = length(ids),
        ncol = length(lev)
      )
      pieces[[k]] <- data.frame(
        group = g,
        level = rep(lev, each = length(ids)),
        coefficient = cf,
        draw = rep(ids, times = length(lev)),
        value = as.vector(m),
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(pieces)) .gp3ha_stop("No requested group-effect draws were available.")
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}

#' Group-Level Rank-Probability Table
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param group One grouping-factor name.
#' @param coefficient One group-level coefficient.
#' @param ndraws Number of posterior draws.
#' @param seed Draw-subsampling seed.
#'
#' @return Descriptive posterior rank probabilities. Rank 1 is the largest
#'   group-level deviation.
#' @export
group_effect_rank_probability_table <- function(
  fit,
  group,
  coefficient = "Intercept",
  ndraws = 1000L,
  seed = 1L
) {
  if (!is.character(group) || length(group) != 1L || is.na(group)) {
    .gp3ha_stop("`group` must identify one grouping factor.")
  }
  if (!is.character(coefficient) || length(coefficient) != 1L ||
      is.na(coefficient)) {
    .gp3ha_stop("`coefficient` must identify one group-level coefficient.")
  }

  d <- group_effect_draws_table(
    fit,
    groups = group,
    coefficients = coefficient,
    ndraws = ndraws,
    seed = seed
  )

  lev <- unique(d$level)
  ids <- sort(unique(d$draw))
  m <- vapply(
    lev,
    function(z) {
      dz <- d[d$level == z, , drop = FALSE]
      dz <- dz[match(ids, dz$draw), , drop = FALSE]
      dz$value
    },
    numeric(length(ids))
  )
  colnames(m) <- lev
  ranks <- t(apply(-m, 1L, rank, ties.method = "average"))

  data.frame(
    group = group,
    coefficient = coefficient,
    level = lev,
    posterior_mean = colMeans(m),
    posterior_sd = apply(m, 2L, stats::sd),
    mean_rank = colMeans(ranks),
    median_rank = apply(ranks, 2L, stats::median),
    probability_highest = colMeans(ranks == 1),
    probability_lowest = colMeans(ranks == ncol(m)),
    probability_positive = colMeans(m > 0),
    automatic_ranking_decision = FALSE,
    stringsAsFactors = FALSE
  )
}

.gp3ha_sum <- function(z, probs) {
  q <- stats::quantile(z, probs = probs, names = FALSE)
  c(mean = mean(z), lower = q[[1L]], median = q[[2L]], upper = q[[3L]])
}

#' Random-Intercept Latent Variance Partition
#'
#' For binary logit models the residual latent variance is `pi^2 / 3`. For
#' lognormal duration models residual log-scale variance is `sigma^2`.
#' Random-slope variance is deliberately excluded.
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param probs Three interval probabilities.
#'
#' @return A `gp3bayes_random_intercept_variance_partition`.
#' @export
random_intercept_variance_partition <- function(
  fit,
  probs = c(0.025, 0.5, 0.975)
) {
  .gp3p_validate_fit(fit)
  probs <- .gp3p_probs(probs)

  draws <- as.matrix(extract_posterior_draws(
    fit,
    regex = "(^sd_.*__Intercept$)|(^sigma$)",
    format = "matrix"
  ))
  sd_cols <- grep("^sd_.*__Intercept$", colnames(draws), value = TRUE)
  if (!length(sd_cols)) .gp3ha_stop("No random-intercept SD draws were found.")

  group_var <- draws[, sd_cols, drop = FALSE]^2
  group_names <- sub("__Intercept$", "", sub("^sd_", "", sd_cols))

  residual_var <- if (identical(fit$family, "binary")) {
    rep(pi^2 / 3, nrow(draws))
  } else {
    if (!"sigma" %in% colnames(draws)) {
      .gp3ha_stop("Duration variance partition requires posterior `sigma`.")
    }
    draws[, "sigma"]^2
  }

  total <- rowSums(group_var) + residual_var
  fractions <- sweep(group_var, 1L, total, "/")
  residual_fraction <- residual_var / total

  rows <- lapply(seq_along(sd_cols), function(i) {
    v <- .gp3ha_sum(group_var[, i], probs)
    f <- .gp3ha_sum(fractions[, i], probs)
    data.frame(
      component = group_names[[i]],
      component_type = "random_intercept",
      variance_mean = v[["mean"]],
      variance_lower = v[["lower"]],
      variance_median = v[["median"]],
      variance_upper = v[["upper"]],
      fraction_mean = f[["mean"]],
      fraction_lower = f[["lower"]],
      fraction_median = f[["median"]],
      fraction_upper = f[["upper"]],
      stringsAsFactors = FALSE
    )
  })

  v <- .gp3ha_sum(residual_var, probs)
  f <- .gp3ha_sum(residual_fraction, probs)
  rows[[length(rows) + 1L]] <- data.frame(
    component = if (fit$family == "binary") "logit_residual" else "lognormal_residual",
    component_type = "residual",
    variance_mean = v[["mean"]],
    variance_lower = v[["lower"]],
    variance_median = v[["median"]],
    variance_upper = v[["upper"]],
    fraction_mean = f[["mean"]],
    fraction_lower = f[["lower"]],
    fraction_median = f[["median"]],
    fraction_upper = f[["upper"]],
    stringsAsFactors = FALSE
  )

  structure(
    list(
      family = fit$family,
      table = do.call(rbind, rows),
      probs = probs,
      random_slope_variance_included = FALSE,
      automatic_importance_decision = FALSE,
      interpretation = paste(
        "Fractions describe baseline latent variance partitioning only.",
        "They are not causal variance attributions or automatic rankings."
      )
    ),
    class = "gp3bayes_random_intercept_variance_partition"
  )
}

#' Random-Intercept Variance-Partition Table
#'
#' @param x A variance-partition object.
#' @return Component-level variance and fraction summaries.
#' @export
random_intercept_variance_partition_table <- function(x) {
  if (!inherits(x, "gp3bayes_random_intercept_variance_partition")) {
    .gp3ha_stop("`x` must be a gp3bayes random-intercept variance partition.")
  }
  x$table
}

#' Plot Group-Effect Posterior Distributions
#'
#' @param x A table returned by `group_effect_draws_table()`.
#' @param max_levels Maximum displayed grouping levels.
#' @return A faceted `ggplot`.
#' @export
plot_group_effect_distribution <- function(x, max_levels = 20L) {
  .gp3g_require("ggplot2", "plot group-effect distributions")
  max_levels <- .gp3ha_int(max_levels, "max_levels")
  if (!is.data.frame(x) ||
      !all(c("group", "level", "coefficient", "value") %in% names(x))) {
    .gp3ha_stop("`x` must be a group-effect draw table.")
  }
  order_levels <- names(sort(tapply(abs(x$value), x$level, mean), decreasing = TRUE))
  keep <- utils::head(order_levels, max_levels)
  d <- x[x$level %in% keep, , drop = FALSE]

  ggplot2::ggplot(
    d,
    ggplot2::aes(x = value, group = level, linetype = level)
  ) +
    ggplot2::geom_density() +
    ggplot2::facet_grid(group ~ coefficient, scales = "free") +
    ggplot2::labs(
      x = "Group-level deviation",
      y = "Density",
      linetype = "Level",
      title = "Group-level posterior distributions"
    ) +
    theme_gp3bayes()
}

#' Plot Group-Effect Rank Probabilities
#'
#' @param x A rank-probability table.
#' @return A `ggplot`.
#' @export
plot_group_effect_rank_probability <- function(x) {
  .gp3g_require("ggplot2", "plot group-effect rank probabilities")
  if (!is.data.frame(x) ||
      !all(c("level", "mean_rank", "probability_highest") %in% names(x))) {
    .gp3ha_stop("`x` must be a group-effect rank-probability table.")
  }
  d <- x
  d$level <- factor(d$level, levels = rev(d$level[order(d$mean_rank)]))
  ggplot2::ggplot(d, ggplot2::aes(x = level, y = probability_highest)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = "Grouping level",
      y = "Posterior probability of largest deviation",
      title = "Group-level rank uncertainty"
    ) +
    theme_gp3bayes()
}

#' Plot Random-Intercept Variance Partition
#'
#' @param x A variance-partition object or table.
#' @return A `ggplot`.
#' @export
plot_random_intercept_variance_partition <- function(x) {
  .gp3g_require("ggplot2", "plot random-intercept variance partition")
  d <- if (inherits(x, "gp3bayes_random_intercept_variance_partition")) {
    random_intercept_variance_partition_table(x)
  } else {
    x
  }
  if (!is.data.frame(d) ||
      !all(c("component", "fraction_median", "fraction_lower", "fraction_upper") %in%
           names(d))) {
    .gp3ha_stop("`x` does not contain variance-partition summaries.")
  }
  d$component <- factor(d$component, levels = rev(d$component))
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = component,
      y = fraction_median,
      ymin = fraction_lower,
      ymax = fraction_upper
    )
  ) +
    ggplot2::geom_pointrange() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      x = NULL,
      y = "Latent variance fraction",
      title = "Baseline random-intercept variance partition"
    ) +
    theme_gp3bayes()
}

#' @export
print.gp3bayes_random_intercept_variance_partition <- function(x, ...) {
  cat("\ngp3bayes random-intercept latent variance partition\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Components: ", nrow(x$table), "\n", sep = "")
  cat(" Random-slope variance included: FALSE\n")
  invisible(x)
}
