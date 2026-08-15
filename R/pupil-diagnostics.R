
.gp3p_fit_data <- function(fit) {
  if (!inherits(fit, "gp3bayes_pupil_fit") || !isTRUE(fit$fit_performed)) {
    .gp3p_stop("`fit` must be a fitted pupil model.")
  }
  if (is.null(fit$translation$data) || !is.data.frame(fit$translation$data)) {
    .gp3p_stop("Malformed pupil fit: translated fitting data are unavailable.")
  }
  fit$translation$data
}

.gp3p_extract_expected <- function(fit, ndraws, max_cells) {
  d <- .gp3p_fit_data(fit)
  if (nrow(d) * ndraws > max_cells) {
    .gp3p_stop("Requested posterior expansion exceeds `max_cells`.")
  }
  brms::posterior_epred(fit$backend_fit, ndraws = ndraws)
}

.gp3p_acf_table <- function(residual, series, max_lag) {
  pieces <- split(seq_along(residual), series)
  vals <- vector("list", length(pieces))
  k <- 0L
  for (idx in pieces) {
    r <- residual[idx]
    r <- r[is.finite(r)]
    if (length(r) <= max_lag + 1L) next
    ac <- stats::acf(
      r, lag.max = max_lag, plot = FALSE, na.action = stats::na.pass
    )$acf
    k <- k + 1L
    vals[[k]] <- data.frame(
      lag = seq.int(0L, length(ac) - 1L),
      acf = as.numeric(ac),
      stringsAsFactors = FALSE
    )
  }
  vals <- vals[seq_len(k)]
  if (!length(vals)) {
    return(data.frame(lag = integer(), acf = numeric(), n_series = integer()))
  }
  all <- do.call(rbind, vals)
  mean_tab <- stats::aggregate(acf ~ lag, data = all, FUN = mean)
  n_tab <- stats::aggregate(acf ~ lag, data = all, FUN = length)
  data.frame(
    lag = mean_tab$lag,
    acf = mean_tab$acf,
    n_series = as.integer(n_tab$acf),
    stringsAsFactors = FALSE
  )
}

.gp3p_sampler_evidence <- function(fit) {
  empty <- data.frame(
    metric = character(), value = numeric(), status = character(),
    interpretation = character(), stringsAsFactors = FALSE
  )
  if (!requireNamespace("brms", quietly = TRUE)) return(empty)
  np <- try(brms::nuts_params(fit$backend_fit), silent = TRUE)
  if (inherits(np, "try-error") || !is.data.frame(np) ||
      !all(c("Parameter", "Value") %in% names(np))) {
    return(empty)
  }

  divergence <- sum(
    np$Value[np$Parameter == "divergent__"] > 0, na.rm = TRUE
  )
  tree_values <- np$Value[np$Parameter == "treedepth__"]
  tree_limit <- .gp3p_null_or(fit$sampling$max_treedepth, NA_integer_)
  tree_hits <- if (length(tree_values) && is.finite(tree_limit)) {
    sum(tree_values >= tree_limit, na.rm = TRUE)
  } else NA_real_

  energy_rows <- np[np$Parameter == "energy__", , drop = FALSE]
  bfmi <- numeric()
  if (nrow(energy_rows)) {
    if ("Chain" %in% names(energy_rows)) {
      energy_split <- split(energy_rows$Value, energy_rows$Chain)
    } else {
      energy_split <- list(energy_rows$Value)
    }
    bfmi <- vapply(
      energy_split,
      function(e) {
        e <- e[is.finite(e)]
        if (length(e) < 3L || stats::var(e) <= 0) return(NA_real_)
        mean(diff(e)^2) / stats::var(e)
      },
      numeric(1)
    )
  }
  min_bfmi <- if (!length(bfmi) || all(is.na(bfmi))) NA_real_
  else min(bfmi, na.rm = TRUE)

  data.frame(
    metric = c("divergent_transitions", "max_treedepth_hits", "minimum_ebfmi"),
    value = c(divergence, tree_hits, min_bfmi),
    status = c(
      if (is.finite(divergence) && divergence == 0) "pass" else "review",
      if (is.finite(tree_hits) && tree_hits == 0) "pass" else "review",
      if (is.finite(min_bfmi) && min_bfmi >= 0.3) "pass" else "review"
    ),
    interpretation = c(
      "Sampler divergence evidence only; zero divergences do not certify model adequacy.",
      "Sampler treedepth evidence only; no threshold hit does not certify convergence.",
      "Energy diagnostic evidence only; the 0.3 threshold is a numerical review heuristic."
    ),
    stringsAsFactors = FALSE
  )
}

.gp3p_trapezoid <- function(time, value) {
  ok <- is.finite(time) & is.finite(value)
  time <- time[ok]
  value <- value[ok]
  if (length(time) < 2L) return(NA_real_)
  ord <- order(time)
  time <- time[ord]
  value <- value[ord]
  sum(diff(time) * (utils::head(value, -1L) + utils::tail(value, -1L)) / 2)
}

.gp3p_curve_matrix <- function(y, time) {
  split_time <- split(seq_along(time), time)
  times <- as.numeric(names(split_time))
  ord <- order(times)
  split_time <- split_time[ord]
  times <- times[ord]
  if (is.vector(y)) {
    curve <- vapply(split_time, function(idx) mean(y[idx]), numeric(1))
    return(list(time = times, curve = matrix(curve, nrow = 1L)))
  }
  curve <- vapply(
    split_time,
    function(idx) rowMeans(y[, idx, drop = FALSE]),
    numeric(nrow(y))
  )
  if (is.vector(curve)) curve <- matrix(curve, nrow = nrow(y))
  list(time = times, curve = curve)
}

.gp3p_ppc_features <- function(yrep, d, window = NULL) {
  observed_curve <- .gp3p_curve_matrix(d$.pupil_model, d$.event_time)
  replicated_curve <- .gp3p_curve_matrix(yrep, d$.event_time)
  t <- observed_curve$time
  obs <- observed_curve$curve[1L, ]
  rep <- replicated_curve$curve

  peak_idx <- max.col(rep, ties.method = "first")
  peak <- rep[cbind(seq_len(nrow(rep)), peak_idx)]
  latency <- t[peak_idx]
  auc <- apply(rep, 1L, function(z) .gp3p_trapezoid(t, z))
  obs_peak_idx <- which.max(obs)

  feature <- data.frame(
    statistic = c("peak_response", "peak_latency", "auc"),
    observed = c(obs[obs_peak_idx], t[obs_peak_idx], .gp3p_trapezoid(t, obs)),
    replicated_median = c(
      stats::median(peak),
      stats::median(latency),
      stats::median(auc, na.rm = TRUE)
    ),
    lower = c(
      stats::quantile(peak, 0.05, names = FALSE, type = 8),
      stats::quantile(latency, 0.05, names = FALSE, type = 8),
      stats::quantile(auc, 0.05, names = FALSE, type = 8, na.rm = TRUE)
    ),
    upper = c(
      stats::quantile(peak, 0.95, names = FALSE, type = 8),
      stats::quantile(latency, 0.95, names = FALSE, type = 8),
      stats::quantile(auc, 0.95, names = FALSE, type = 8, na.rm = TRUE)
    ),
    window_start = NA_real_,
    window_end = NA_real_,
    interpretation = c(
      "Descriptive whole-support PPC peak; not a confirmatory peak declaration.",
      "Descriptive whole-support PPC peak latency; not a confirmatory time-point selection.",
      "Descriptive whole-support PPC area under the mean trajectory."
    ),
    stringsAsFactors = FALSE
  )

  if (!is.null(window)) {
    if (!is.numeric(window) || length(window) != 2L ||
        any(!is.finite(window)) || window[1L] >= window[2L]) {
      .gp3p_stop("`window` must be two increasing finite event-time values.")
    }
    idx <- d$.event_time >= window[1L] & d$.event_time <= window[2L]
    if (!any(idx)) .gp3p_stop("`window` has no fitted pupil observations.")
    rep_window <- rowMeans(yrep[, idx, drop = FALSE])
    feature <- rbind(
      feature,
      data.frame(
        statistic = "declared_window_mean",
        observed = mean(d$.pupil_model[idx]),
        replicated_median = stats::median(rep_window),
        lower = stats::quantile(rep_window, 0.05, names = FALSE, type = 8),
        upper = stats::quantile(rep_window, 0.95, names = FALSE, type = 8),
        window_start = window[1L],
        window_end = window[2L],
        interpretation = "PPC for the user-declared analysis window.",
        stringsAsFactors = FALSE
      )
    )
  }
  feature
}

.gp3p_group_heterogeneity <- function(yrep, observed, group, label) {
  groups <- split(seq_along(group), group)
  if (length(groups) < 2L) {
    return(data.frame(
      grouping = label, observed_sd = NA_real_,
      replicated_median_sd = NA_real_, lower = NA_real_, upper = NA_real_,
      n_groups = length(groups), stringsAsFactors = FALSE
    ))
  }
  obs_means <- vapply(groups, function(idx) mean(observed[idx]), numeric(1))
  rep_means <- vapply(
    groups,
    function(idx) rowMeans(yrep[, idx, drop = FALSE]),
    numeric(nrow(yrep))
  )
  if (is.vector(rep_means)) rep_means <- matrix(rep_means, nrow = nrow(yrep))
  rep_sd <- apply(rep_means, 1L, stats::sd)
  data.frame(
    grouping = label,
    observed_sd = stats::sd(obs_means),
    replicated_median_sd = stats::median(rep_sd),
    lower = stats::quantile(rep_sd, 0.05, names = FALSE, type = 8),
    upper = stats::quantile(rep_sd, 0.95, names = FALSE, type = 8),
    n_groups = length(groups),
    stringsAsFactors = FALSE
  )
}

.gp3p_mean_lag1 <- function(y, series) {
  groups <- split(seq_along(series), series)
  vals <- vapply(
    groups,
    function(idx) {
      z <- y[idx]
      if (length(z) < 3L || stats::sd(z) == 0) return(NA_real_)
      suppressWarnings(stats::cor(utils::head(z, -1L), utils::tail(z, -1L)))
    },
    numeric(1)
  )
  if (all(is.na(vals))) NA_real_ else mean(vals, na.rm = TRUE)
}

.gp3p_safe_quantile <- function(x, probability) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  stats::quantile(x, probability, names = FALSE, type = 8)
}

.gp3p_measurement_context_by_time <- function(fit) {
  p <- fit$specification$prepared$data
  condition <- if (".condition" %in% names(p)) as.character(p$.condition) else "all"
  key <- interaction(p$.event_time, condition, drop = TRUE, lex.order = TRUE)
  groups <- split(seq_len(nrow(p)), key)
  rows <- lapply(
    groups,
    function(idx) {
      blink <- if (".blink" %in% names(p)) as.logical(p$.blink[idx]) else rep(NA, length(idx))
      interp <- if (".interpolated" %in% names(p)) as.logical(p$.interpolated[idx]) else rep(NA, length(idx))
      data.frame(
        .event_time = p$.event_time[idx[1L]],
        .condition = condition[idx[1L]],
        n_samples = length(idx),
        missing_pupil_proportion = mean(is.na(p$.pupil_model[idx])),
        blink_proportion = if (all(is.na(blink))) NA_real_ else mean(blink, na.rm = TRUE),
        interpolated_proportion = if (all(is.na(interp))) NA_real_ else mean(interp, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Diagnose temporal and sampling behaviour of a pupil fit
#'
#' Reports posterior R-hat/ESS summaries, NUTS sampler evidence when available,
#' residual temporal drift, and residual autocorrelation. Thresholds are
#' numerical review gates, not adequacy certification.
#'
#' @param fit Fitted pupil model.
#' @param ndraws Draws used for expected-value residual summaries.
#' @param max_lag Maximum residual ACF lag.
#' @param max_cells Maximum draw-by-observation cells.
#' @return A `gp3bayes_pupil_diagnostics`.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
diagnose_pupil_fit <- function(
    fit, ndraws = 200L, max_lag = 10L, max_cells = 3000000L) {
  ndraws <- .gp3p_positive(ndraws, "ndraws", TRUE)
  max_lag <- .gp3p_positive(max_lag, "max_lag", TRUE)
  max_cells <- .gp3p_positive(max_cells, "max_cells", TRUE)
  d <- .gp3p_fit_data(fit)

  if (!requireNamespace("posterior", quietly = TRUE)) {
    .gp3p_stop("Package `posterior` is required for pupil fit diagnostics.")
  }
  posterior_draws <- posterior::as_draws_df(fit$backend_fit)
  summ <- posterior::summarise_draws(posterior_draws)
  numeric_summary <- data.frame(
    variable = summ$variable,
    rhat = if ("rhat" %in% names(summ)) summ$rhat else NA_real_,
    ess_bulk = if ("ess_bulk" %in% names(summ)) summ$ess_bulk else NA_real_,
    ess_tail = if ("ess_tail" %in% names(summ)) summ$ess_tail else NA_real_,
    stringsAsFactors = FALSE
  )

  expected <- .gp3p_extract_expected(fit, ndraws, max_cells)
  mu <- colMeans(expected)
  residual <- d$.pupil_model - mu
  acf_table <- .gp3p_acf_table(residual, d$.series_id, max_lag)

  drift <- stats::lm(residual ~ d$.event_time)
  slope <- unname(stats::coef(drift)[2L])
  max_rhat <- if (all(is.na(numeric_summary$rhat))) NA_real_
  else max(numeric_summary$rhat, na.rm = TRUE)
  min_bulk <- if (all(is.na(numeric_summary$ess_bulk))) NA_real_
  else min(numeric_summary$ess_bulk, na.rm = TRUE)
  min_tail <- if (all(is.na(numeric_summary$ess_tail))) NA_real_
  else min(numeric_summary$ess_tail, na.rm = TRUE)

  evidence <- data.frame(
    metric = c(
      "max_rhat", "min_bulk_ess", "min_tail_ess",
      "residual_time_slope", "max_abs_residual_acf_nonzero_lag"
    ),
    value = c(
      max_rhat, min_bulk, min_tail, slope,
      if (nrow(acf_table) > 1L && any(acf_table$lag > 0L)) {
        max(abs(acf_table$acf[acf_table$lag > 0L]), na.rm = TRUE)
      } else NA_real_
    ),
    status = c(
      if (is.finite(max_rhat) && max_rhat <= 1.01) "pass" else "review",
      if (is.finite(min_bulk) && min_bulk >= 400) "pass" else "review",
      if (is.finite(min_tail) && min_tail >= 400) "pass" else "review",
      "review", "review"
    ),
    interpretation = c(
      "Numerical chain diagnostic only.",
      "Numerical Monte Carlo information only.",
      "Numerical tail information only.",
      "Descriptive residual temporal drift; no adequacy decision.",
      "Descriptive remaining serial structure; no adequacy decision."
    ),
    stringsAsFactors = FALSE
  )
  sampler <- .gp3p_sampler_evidence(fit)
  if (nrow(sampler)) evidence <- rbind(evidence, sampler)
  status <- if (any(evidence$status == "fail")) "fail"
  else if (any(evidence$status == "review")) "review" else "pass"

  structure(
    list(
      status = status,
      evidence = evidence,
      parameter_diagnostics = numeric_summary,
      sampler_diagnostics = sampler,
      residuals = data.frame(
        .event_time = d$.event_time,
        .series_id = d$.series_id,
        residual = residual,
        stringsAsFactors = FALSE
      ),
      residual_acf = acf_table,
      adequacy_certified = FALSE
    ),
    class = c("gp3bayes_pupil_diagnostics", "gp3bayes_sampling_diagnostics")
  )
}

#' Summarise residual autocorrelation for a pupil fit
#' @param x A pupil fit or pupil diagnostics object.
#' @param max_lag Maximum lag when `x` is a fit.
#' @param ndraws Draws for fit-based expected residuals.
#' @return A data frame of mean within-series residual autocorrelations.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
pupil_residual_acf <- function(x, max_lag = 10L, ndraws = 200L) {
  if (inherits(x, "gp3bayes_pupil_fit")) {
    x <- diagnose_pupil_fit(x, ndraws = ndraws, max_lag = max_lag)
  }
  if (!inherits(x, "gp3bayes_pupil_diagnostics")) {
    .gp3p_stop("`x` must be a pupil fit or pupil diagnostics object.")
  }
  x$residual_acf
}

#' Run pupil-specific posterior predictive checks
#'
#' Compares observed and replicated trajectories, distributional features,
#' whole-support peak/latency and AUC, optional declared-window response,
#' lag-one serial structure, participant/trial heterogeneity, residual
#' trajectories, and blink/interpolation context. The object reports evidence
#' and never declares a model adequate.
#'
#' @param fit A fitted pupil model.
#' @param ndraws Posterior predictive draws.
#' @param probability Predictive envelope probability.
#' @param window Optional user-declared event-time window in canonical seconds.
#' @param max_cells Maximum draw-by-observation cells.
#' @return A `gp3bayes_pupil_ppc`.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
check_pupil_posterior_predictive <- function(
    fit, ndraws = 200L, probability = 0.90, window = NULL,
    max_cells = 3000000L) {
  ndraws <- .gp3p_positive(ndraws, "ndraws", TRUE)
  probability <- .gp3p_probability(probability, "probability", TRUE)
  max_cells <- .gp3p_positive(max_cells, "max_cells", TRUE)
  d <- .gp3p_fit_data(fit)
  if (nrow(d) * ndraws > max_cells) {
    .gp3p_stop("PPC expansion exceeds `max_cells`.")
  }
  yrep <- brms::posterior_predict(fit$backend_fit, ndraws = ndraws)
  if (!is.matrix(yrep)) yrep <- matrix(yrep, nrow = ndraws)

  condition <- if (".condition" %in% names(d)) {
    as.character(d$.condition)
  } else {
    rep("all", nrow(d))
  }
  group <- interaction(d$.event_time, condition, drop = TRUE, lex.order = TRUE)
  split_idx <- split(seq_len(nrow(d)), group)
  alpha <- (1 - probability) / 2
  rows <- lapply(
    split_idx,
    function(idx) {
      observed <- mean(d$.pupil_model[idx])
      rep_mean <- rowMeans(yrep[, idx, drop = FALSE])
      q <- stats::quantile(
        rep_mean, c(alpha, 0.5, 1 - alpha), names = FALSE, type = 8
      )
      data.frame(
        .event_time = d$.event_time[idx[1L]],
        .condition = condition[idx[1L]],
        observed_mean = observed,
        replicated_mean = mean(rep_mean),
        replicated_median = q[2L],
        lower = q[1L],
        upper = q[3L],
        stringsAsFactors = FALSE
      )
    }
  )
  trajectory <- do.call(rbind, rows)
  trajectory <- trajectory[order(trajectory$.condition, trajectory$.event_time), ]
  rownames(trajectory) <- NULL

  expected <- colMeans(
    .gp3p_extract_expected(fit, min(ndraws, 200L), max_cells)
  )
  residual <- d$.pupil_model - expected
  residual_group <- split(seq_len(nrow(d)), group)
  residual_trajectory <- do.call(
    rbind,
    lapply(
      residual_group,
      function(idx) data.frame(
        .event_time = d$.event_time[idx[1L]],
        .condition = condition[idx[1L]],
        mean_residual = mean(residual[idx]),
        sd_residual = if (length(idx) > 1L) stats::sd(residual[idx]) else NA_real_,
        n = length(idx),
        stringsAsFactors = FALSE
      )
    )
  )
  rownames(residual_trajectory) <- NULL

  distribution <- data.frame(
    statistic = c("mean", "sd", "min", "max"),
    observed = c(
      mean(d$.pupil_model), stats::sd(d$.pupil_model),
      min(d$.pupil_model), max(d$.pupil_model)
    ),
    replicated_median = c(
      stats::median(rowMeans(yrep)),
      stats::median(apply(yrep, 1L, stats::sd)),
      stats::median(apply(yrep, 1L, min)),
      stats::median(apply(yrep, 1L, max))
    ),
    stringsAsFactors = FALSE
  )

  features <- .gp3p_ppc_features(yrep, d, window = window)

  lag1_rep <- apply(
    yrep, 1L, function(z) .gp3p_mean_lag1(z, d$.series_id)
  )
  autocorrelation <- data.frame(
    statistic = "mean_within_series_lag1",
    observed = .gp3p_mean_lag1(d$.pupil_model, d$.series_id),
    replicated_median = stats::median(lag1_rep, na.rm = TRUE),
    lower = stats::quantile(lag1_rep, 0.05, names = FALSE, type = 8, na.rm = TRUE),
    upper = stats::quantile(lag1_rep, 0.95, names = FALSE, type = 8, na.rm = TRUE),
    stringsAsFactors = FALSE
  )

  heterogeneity <- rbind(
    .gp3p_group_heterogeneity(
      yrep, d$.pupil_model, d$.participant, "participant"
    ),
    .gp3p_group_heterogeneity(
      yrep, d$.pupil_model, d$.series_id, "trial_series"
    )
  )

  structure(
    list(
      status = "evidence",
      trajectory = trajectory,
      distribution = distribution,
      features = features,
      residuals = data.frame(
        .event_time = d$.event_time,
        .condition = condition,
        .series_id = d$.series_id,
        residual = residual,
        stringsAsFactors = FALSE
      ),
      residual_trajectory = residual_trajectory,
      autocorrelation = autocorrelation,
      heterogeneity = heterogeneity,
      measurement_context = .gp3p_measurement_context_by_time(fit),
      probability = probability,
      declared_window = window,
      unit = fit$outcome_unit,
      model_adequacy_certified = FALSE,
      confirmatory_peak_selected = FALSE
    ),
    class = c("gp3bayes_pupil_ppc", "gp3bayes_posterior_predictive_check")
  )
}

#' Extract pupil PPC evidence tables
#' @param x A pupil PPC object.
#' @param component One of `"trajectory"`, `"distribution"`, `"features"`,
#'   `"residuals"`, `"residual_trajectory"`, `"autocorrelation"`,
#'   `"heterogeneity"`, or `"measurement_context"`.
#' @return A data frame.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
pupil_ppc_table <- function(
    x,
    component = c(
      "trajectory", "distribution", "features", "residuals",
      "residual_trajectory", "autocorrelation", "heterogeneity",
      "measurement_context"
    )) {
  if (!inherits(x, "gp3bayes_pupil_ppc")) {
    .gp3p_stop("`x` must be a pupil PPC object.")
  }
  component <- match.arg(component)
  x[[component]]
}

#' @export
print.gp3bayes_pupil_diagnostics <- function(x, ...) {
  cat("<gp3bayes_pupil_diagnostics>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Evidence metrics: ", nrow(x$evidence), "\n", sep = "")
  cat("  Adequacy certified: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_pupil_ppc <- function(x, ...) {
  cat("<gp3bayes_pupil_ppc>\n")
  cat("  Trajectory cells: ", nrow(x$trajectory), "\n", sep = "")
  cat("  Feature checks: ", nrow(x$features), "\n", sep = "")
  cat("  Predictive envelope: ", 100 * x$probability, "%\n", sep = "")
  cat("  Confirmatory peak selected: FALSE\n")
  cat("  Model adequacy certified: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_diagnostics <- function(x, ...) x$evidence

#' @export
as.data.frame.gp3bayes_pupil_ppc <- function(x, ...) x$trajectory

#' Summarise a fitted pupil posterior
#'
#' Returns posterior location, uncertainty, R-hat, and ESS evidence for model
#' parameters without converting them into psychological constructs.
#' @param fit A fitted pupil model.
#' @param probability Credible interval probability.
#' @return A `gp3bayes_pupil_posterior_summary`.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
summarise_pupil_posterior <- function(fit, probability = 0.95) {
  if (!inherits(fit, "gp3bayes_pupil_fit") || !isTRUE(fit$fit_performed)) {
    .gp3p_stop("`fit` must be a fitted pupil model.")
  }
  probability <- .gp3p_probability(probability, "probability", TRUE)
  if (!requireNamespace("posterior", quietly = TRUE)) {
    .gp3p_stop("Package `posterior` is required for posterior summaries.")
  }
  draws <- posterior::as_draws_df(fit$backend_fit)
  summ <- posterior::summarise_draws(draws)
  mat <- posterior::as_draws_matrix(draws)
  alpha <- (1 - probability) / 2
  qs <- t(apply(
    mat, 2L, stats::quantile,
    probs = c(alpha, 1 - alpha), names = FALSE, type = 8
  ))
  qmap <- qs[match(summ$variable, rownames(qs)), , drop = FALSE]
  summ$lower <- qmap[, 1L]
  summ$upper <- qmap[, 2L]
  structure(
    list(
      table = as.data.frame(summ),
      probability = probability,
      outcome_unit = fit$outcome_unit,
      interpretation = "Parameter summaries on the approved Gaussian pupil model scale."
    ),
    class = "gp3bayes_pupil_posterior_summary"
  )
}

#' @export
print.gp3bayes_pupil_posterior_summary <- function(x, ...) {
  cat("<gp3bayes_pupil_posterior_summary>\n")
  cat("  Parameters: ", nrow(x$table), "\n", sep = "")
  cat("  Outcome unit: ", x$outcome_unit, "\n", sep = "")
  cat("  Psychological interpretation assigned: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_posterior_summary <- function(x, ...) x$table
