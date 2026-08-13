# Post-fit exploration and diagnostic tables

.gp3p_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.gp3p_require <- function(package, purpose) {
  if (!requireNamespace(package, quietly = TRUE)) {
    .gp3p_stop(
      "Optional package `", package, "` is required to ", purpose, "."
    )
  }
  invisible(TRUE)
}

.gp3p_validate_fit <- function(fit, family = NULL) {
  if (!inherits(fit, "gp3bayes_fit")) {
    .gp3p_stop("`fit` must inherit from `gp3bayes_fit`.")
  }
  if (is.null(fit$backend_fit) || !inherits(fit$backend_fit, "brmsfit")) {
    .gp3p_stop(
      "`fit$backend_fit` must be a fitted `brmsfit` object."
    )
  }
  if (!fit$family %in% c("binary", "duration")) {
    .gp3p_stop("The fit does not use an approved gp3bayes family.")
  }
  if (!is.null(family) && !identical(fit$family, family)) {
    .gp3p_stop(
      "`fit` must use the approved `", family, "` family."
    )
  }
  invisible(fit)
}

.gp3p_training_data <- function(fit) {
  .gp3p_validate_fit(fit)
  data <- fit$specification$prepared$data
  if (!is.data.frame(data) || nrow(data) < 1L) {
    .gp3p_stop("The fitted object does not contain valid prepared data.")
  }
  data
}

.gp3p_contract <- function(fit) {
  contract <- fit$specification$contract
  if (is.null(contract)) {
    contract <- fit$specification$prepared$contract
  }
  if (is.null(contract)) {
    .gp3p_stop("The fitted object does not retain its model contract.")
  }
  contract
}

.gp3p_mapping <- function(fit, name) {
  contract <- .gp3p_contract(fit)
  if (!is.null(contract$mappings) && name %in% names(contract$mappings)) {
    return(contract$mappings[[name]])
  }
  legacy <- paste0(name, "_col")
  if (legacy %in% names(contract)) return(contract[[legacy]])
  NULL
}

.gp3p_outcome_col <- function(fit) {
  out <- .gp3p_mapping(fit, "outcome")
  if (is.null(out) || !nzchar(out)) {
    .gp3p_stop("The model contract does not identify the outcome column.")
  }
  out
}

.gp3p_probs <- function(probs) {
  if (
    !is.numeric(probs) ||
      length(probs) != 3L ||
      anyNA(probs) ||
      any(!is.finite(probs)) ||
      any(probs < 0 | probs > 1) ||
      is.unsorted(probs, strictly = TRUE)
  ) {
    .gp3p_stop(
      "`probs` must contain three finite, strictly increasing probabilities."
    )
  }
  as.numeric(probs)
}

.gp3p_call_supported <- function(fun, args) {
  formal_names <- names(formals(fun))
  if (!"..." %in% formal_names) {
    args <- args[names(args) %in% formal_names]
  }
  do.call(fun, args)
}

.gp3p_draw_matrix <- function(x, variables = NULL, regex = NULL) {
  if (inherits(x, "gp3bayes_fit")) {
    return(
      extract_posterior_draws(
        x,
        variables = variables,
        regex = regex,
        format = "matrix"
      )
    )
  }

  if (inherits(x, "draws")) {
    .gp3p_require("posterior", "convert posterior draws")
    out <- posterior::as_draws_matrix(x)
    out <- as.matrix(out)
  } else if (is.data.frame(x)) {
    numeric_cols <- vapply(x, is.numeric, logical(1L))
    out <- as.matrix(x[, numeric_cols, drop = FALSE])
  } else if (is.matrix(x) && is.numeric(x)) {
    out <- x
  } else {
    .gp3p_stop(
      "`x` must be a gp3bayes fit, posterior draws object, numeric matrix, ",
      "or numeric data frame."
    )
  }

  if (nrow(out) < 2L || ncol(out) < 1L || anyNA(out) || any(!is.finite(out))) {
    .gp3p_stop("Posterior draws must be a finite matrix with at least two rows.")
  }
  if (is.null(colnames(out))) {
    colnames(out) <- paste0("V", seq_len(ncol(out)))
  }

  if (!is.null(variables)) {
    missing <- setdiff(variables, colnames(out))
    if (length(missing)) {
      .gp3p_stop("Unknown posterior variables: ", paste(missing, collapse = ", "), ".")
    }
    out <- out[, variables, drop = FALSE]
  }
  if (!is.null(regex)) {
    keep <- grepl(regex, colnames(out), perl = TRUE)
    if (!any(keep)) {
      .gp3p_stop("`regex` did not match any posterior variables.")
    }
    out <- out[, keep, drop = FALSE]
  }
  out
}

.gp3p_summary_matrix <- function(draws, probs) {
  probs <- .gp3p_probs(probs)
  q <- apply(
    draws,
    2L,
    stats::quantile,
    probs = probs,
    names = FALSE,
    na.rm = FALSE
  )
  if (is.null(dim(q))) q <- matrix(q, ncol = 1L)
  data.frame(
    variable = colnames(draws),
    mean = colMeans(draws),
    sd = apply(draws, 2L, stats::sd),
    lower = q[1L, ],
    median = q[2L, ],
    upper = q[3L, ],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Extract Posterior Draws from a gp3bayes Fit
#'
#' Converts the fitted `brms` posterior into a standard `posterior` draws
#' representation without changing the fitted model.
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param variables Optional exact posterior variable names.
#' @param regex Optional regular expression used to retain posterior variables.
#' @param format One of `"array"`, `"matrix"`, `"df"`, or `"rvars"`.
#'
#' @return A posterior draws object in the requested format.
#' @export
extract_posterior_draws <- function(
  fit,
  variables = NULL,
  regex = NULL,
  format = c("array", "matrix", "df", "rvars")
) {
  .gp3p_validate_fit(fit)
  .gp3p_require("posterior", "extract posterior draws")
  format <- match.arg(format)

  draws <- posterior::as_draws_array(fit$backend_fit)
  available <- posterior::variables(draws)

  selected <- available
  if (!is.null(variables)) {
    if (!is.character(variables) || !length(variables)) {
      .gp3p_stop("`variables` must be a non-empty character vector.")
    }
    missing <- setdiff(variables, available)
    if (length(missing)) {
      .gp3p_stop("Unknown posterior variables: ", paste(missing, collapse = ", "), ".")
    }
    selected <- intersect(selected, variables)
  }
  if (!is.null(regex)) {
    if (!is.character(regex) || length(regex) != 1L || is.na(regex)) {
      .gp3p_stop("`regex` must be one non-missing character string.")
    }
    selected <- selected[grepl(regex, selected, perl = TRUE)]
  }
  if (!length(selected)) {
    .gp3p_stop("No posterior variables remain after selection.")
  }

  draws <- posterior::subset_draws(draws, variable = selected)

  switch(
    format,
    array = posterior::as_draws_array(draws),
    matrix = posterior::as_draws_matrix(draws),
    df = posterior::as_draws_df(draws),
    rvars = posterior::as_draws_rvars(draws)
  )
}

#' Extract NUTS Sampler Diagnostics
#'
#' @param fit A fitted `gp3bayes_fit`.
#'
#' @return A data frame returned from `brms::nuts_params()`.
#' @export
extract_sampler_diagnostics <- function(fit) {
  .gp3p_validate_fit(fit)
  .gp3p_require("brms", "extract NUTS sampler diagnostics")
  out <- tryCatch(
    brms::nuts_params(fit$backend_fit),
    error = function(e) {
      .gp3p_stop(
        "Sampler diagnostics could not be extracted: ",
        conditionMessage(e)
      )
    }
  )
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Extract Pointwise Log-Likelihood Draws
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param newdata Optional prediction data. `NULL` uses the fitted data.
#' @param include_group_effects Whether fitted group-level effects are included.
#' @param ndraws Optional number of posterior draws.
#'
#' @return A numeric matrix with posterior draws in rows and observations in
#'   columns.
#' @export
extract_log_likelihood <- function(
  fit,
  newdata = NULL,
  include_group_effects = TRUE,
  ndraws = NULL
) {
  .gp3p_validate_fit(fit)
  .gp3p_require("brms", "extract pointwise log likelihood")
  if (!is.logical(include_group_effects) ||
      length(include_group_effects) != 1L ||
      is.na(include_group_effects)) {
    .gp3p_stop("`include_group_effects` must be TRUE or FALSE.")
  }
  if (!is.null(newdata) && !is.data.frame(newdata)) {
    .gp3p_stop("`newdata` must be NULL or a data frame.")
  }
  if (!is.null(ndraws)) {
    if (!is.numeric(ndraws) || length(ndraws) != 1L || is.na(ndraws) ||
        ndraws < 1 || ndraws != floor(ndraws)) {
      .gp3p_stop("`ndraws` must be NULL or one positive integer.")
    }
    ndraws <- as.integer(ndraws)
  }
  args <- list(
    object = fit$backend_fit,
    newdata = newdata,
    re_formula = if (include_group_effects) NULL else NA,
    ndraws = ndraws
  )
  args <- args[!vapply(args, is.null, logical(1L))]
  out <- .gp3p_call_supported(brms::log_lik, args)
  out <- as.matrix(out)
  if (!is.numeric(out) || anyNA(out) || any(!is.finite(out))) {
    .gp3p_stop("The extracted log-likelihood matrix is not finite.")
  }
  out
}

#' Posterior Interval Table
#'
#' @param x A gp3bayes fit, posterior draws object, numeric matrix, or numeric
#'   data frame.
#' @param variables Optional exact posterior variable names.
#' @param regex Optional regular expression for posterior variable names.
#' @param probs Three probabilities defining lower, median, and upper summaries.
#'
#' @return A data frame containing posterior location, spread, and intervals.
#' @examples
#' draws <- cbind(alpha = rnorm(200), beta = rnorm(200, 0.5))
#' posterior_interval_table(draws)
#' @export
posterior_interval_table <- function(
  x,
  variables = NULL,
  regex = NULL,
  probs = c(0.025, 0.5, 0.975)
) {
  draws <- .gp3p_draw_matrix(x, variables = variables, regex = regex)
  .gp3p_summary_matrix(draws, probs)
}

#' Posterior Direction and ROPE Probability Table
#'
#' @param x A gp3bayes fit or posterior draws accepted by
#'   `posterior_interval_table()`.
#' @param variables,regex Posterior variable selectors.
#' @param rope Optional two-element interval defining a region of practical
#'   equivalence. It is descriptive only.
#'
#' @return A data frame with posterior direction probabilities and, when
#'   requested, the posterior probability inside the supplied interval.
#' @examples
#' draws <- cbind(alpha = rnorm(500), beta = rnorm(500, 0.4))
#' posterior_probability_table(draws, rope = c(-0.1, 0.1))
#' @export
posterior_probability_table <- function(
  x,
  variables = NULL,
  regex = NULL,
  rope = NULL
) {
  draws <- .gp3p_draw_matrix(x, variables = variables, regex = regex)
  if (!is.null(rope)) {
    if (!is.numeric(rope) || length(rope) != 2L || anyNA(rope) ||
        any(!is.finite(rope)) || rope[[1L]] >= rope[[2L]]) {
      .gp3p_stop("`rope` must be NULL or two increasing finite numbers.")
    }
  }
  out <- data.frame(
    variable = colnames(draws),
    probability_gt_zero = colMeans(draws > 0),
    probability_lt_zero = colMeans(draws < 0),
    stringsAsFactors = FALSE
  )
  if (!is.null(rope)) {
    out$probability_in_rope <- colMeans(
      draws >= rope[[1L]] & draws <= rope[[2L]]
    )
    out$rope_lower <- rope[[1L]]
    out$rope_upper <- rope[[2L]]
  }
  out
}

#' Posterior Correlation Table
#'
#' @param x A gp3bayes fit or posterior draws accepted by
#'   `posterior_interval_table()`.
#' @param variables,regex Posterior variable selectors.
#' @param method Correlation method.
#'
#' @return A long data frame of unique posterior-draw correlations.
#' @examples
#' x <- cbind(a = rnorm(100), b = rnorm(100), c = rnorm(100))
#' posterior_correlation_table(x)
#' @export
posterior_correlation_table <- function(
  x,
  variables = NULL,
  regex = NULL,
  method = c("pearson", "spearman")
) {
  method <- match.arg(method)
  draws <- .gp3p_draw_matrix(x, variables = variables, regex = regex)
  if (ncol(draws) < 2L) {
    .gp3p_stop("At least two posterior variables are required.")
  }
  cm <- stats::cor(draws, method = method)
  idx <- which(lower.tri(cm), arr.ind = TRUE)
  data.frame(
    variable_1 = rownames(cm)[idx[, "row"]],
    variable_2 = colnames(cm)[idx[, "col"]],
    correlation = cm[idx],
    method = method,
    stringsAsFactors = FALSE
  )
}

#' Modern MCMC Diagnostic Table
#'
#' Computes rank-normalised R-hat, bulk ESS, tail ESS, and Monte Carlo standard
#' error summaries through the `posterior` package.
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param variables Optional exact posterior variable names.
#' @param regex Optional posterior-variable regular expression.
#'
#' @return A data frame with posterior diagnostics by variable.
#' @export
mcmc_diagnostic_table <- function(fit, variables = NULL, regex = NULL) {
  .gp3p_validate_fit(fit)
  .gp3p_require("posterior", "summarise MCMC diagnostics")
  draws <- extract_posterior_draws(
    fit,
    variables = variables,
    regex = regex,
    format = "array"
  )
  out <- posterior::summarise_draws(
    draws,
    "mean",
    "median",
    "sd",
    "rhat",
    "ess_bulk",
    "ess_tail",
    "mcse_mean"
  )
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Identify MCMC Diagnostic Flags
#'
#' @param x A fitted gp3bayes object or an MCMC diagnostic table.
#' @param rhat_threshold R-hat value above which a parameter is flagged.
#' @param min_bulk_ess Minimum bulk effective sample size.
#' @param min_tail_ess Minimum tail effective sample size.
#' @param max_mcse_fraction Maximum MCSE-to-posterior-SD fraction.
#'
#' @return A parameter-level flag table. Flags request review; they do not
#'   establish or negate model adequacy.
#' @examples
#' d <- data.frame(
#'   variable = c("a", "b"),
#'   sd = c(1, 1),
#'   rhat = c(1.00, 1.03),
#'   ess_bulk = c(1000, 150),
#'   ess_tail = c(900, 120),
#'   mcse_mean = c(0.02, 0.15)
#' )
#' identify_mcmc_issues(d)
#' @export
identify_mcmc_issues <- function(
  x,
  rhat_threshold = 1.01,
  min_bulk_ess = 400,
  min_tail_ess = 400,
  max_mcse_fraction = 0.10
) {
  d <- if (inherits(x, "gp3bayes_fit")) mcmc_diagnostic_table(x) else x
  if (!is.data.frame(d)) .gp3p_stop("`x` must be a fit or diagnostic table.")
  required <- c("variable", "sd", "rhat", "ess_bulk", "ess_tail", "mcse_mean")
  if (!all(required %in% names(d))) {
    .gp3p_stop("The diagnostic table is missing required columns.")
  }
  if (!is.numeric(rhat_threshold) || length(rhat_threshold) != 1L ||
      rhat_threshold <= 1) {
    .gp3p_stop("`rhat_threshold` must be one number greater than 1.")
  }

  sd_safe <- ifelse(is.finite(d$sd) & d$sd > 0, d$sd, NA_real_)
  mcse_fraction <- abs(d$mcse_mean) / sd_safe

  out <- data.frame(
    variable = d$variable,
    rhat = d$rhat,
    ess_bulk = d$ess_bulk,
    ess_tail = d$ess_tail,
    mcse_fraction = mcse_fraction,
    rhat_flag = is.na(d$rhat) | d$rhat > rhat_threshold,
    bulk_ess_flag = is.na(d$ess_bulk) | d$ess_bulk < min_bulk_ess,
    tail_ess_flag = is.na(d$ess_tail) | d$ess_tail < min_tail_ess,
    mcse_flag = is.na(mcse_fraction) | mcse_fraction > max_mcse_fraction,
    stringsAsFactors = FALSE
  )
  out$flagged <- with(
    out,
    rhat_flag | bulk_ess_flag | tail_ess_flag | mcse_flag
  )
  out
}

#' Summarise NUTS Sampler Diagnostics
#'
#' @param fit A fitted `gp3bayes_fit`.
#'
#' @return A data frame containing sampler-level summaries and review flags.
#' @export
sampler_diagnostic_table <- function(fit) {
  .gp3p_validate_fit(fit)
  np <- extract_sampler_diagnostics(fit)
  if (!nrow(np)) {
    return(data.frame(
      metric = character(),
      value = numeric(),
      threshold = numeric(),
      flagged = logical(),
      stringsAsFactors = FALSE
    ))
  }

  lower_names <- tolower(names(np))
  parameter_col <- names(np)[match("parameter", lower_names)]
  value_col <- names(np)[match("value", lower_names)]
  chain_col <- names(np)[match("chain", lower_names)]

  if (anyNA(c(parameter_col, value_col))) {
    .gp3p_stop("The NUTS diagnostic table has an unsupported structure.")
  }

  parameter <- as.character(np[[parameter_col]])
  value <- as.numeric(np[[value_col]])
  max_td <- fit$sampling$max_treedepth
  if (is.null(max_td) || !is.finite(max_td)) max_td <- 12

  divergence <- sum(value[parameter == "divergent__"] > 0, na.rm = TRUE)
  treedepth <- sum(
    value[parameter == "treedepth__"] >= max_td,
    na.rm = TRUE
  )

  rows <- list(
    data.frame(
      metric = "divergent_transitions",
      value = divergence,
      threshold = 0,
      flagged = divergence > 0,
      stringsAsFactors = FALSE
    ),
    data.frame(
      metric = "max_treedepth_hits",
      value = treedepth,
      threshold = 0,
      flagged = treedepth > 0,
      stringsAsFactors = FALSE
    )
  )

  energy_rows <- parameter == "energy__"
  if (any(energy_rows) && !is.na(chain_col)) {
    chain <- np[[chain_col]][energy_rows]
    energy <- value[energy_rows]
    split_energy <- split(energy, chain)
    bfmi <- vapply(
      split_energy,
      function(z) {
        if (length(z) < 3L || stats::var(z) == 0) return(NA_real_)
        mean(diff(z)^2) / stats::var(z)
      },
      numeric(1L)
    )
    rows[[length(rows) + 1L]] <- data.frame(
      metric = paste0("ebfmi_chain_", names(bfmi)),
      value = unname(bfmi),
      threshold = 0.30,
      flagged = is.na(bfmi) | bfmi < 0.30,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

#' Summarise MCMC Quality Evidence
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param ... Threshold arguments passed to `identify_mcmc_issues()`.
#'
#' @return A `gp3bayes_mcmc_quality` object containing parameter and sampler
#'   diagnostic evidence. It is a review object, not an adequacy certificate.
#' @export
summarise_mcmc_quality <- function(fit, ...) {
  parameters <- mcmc_diagnostic_table(fit)
  issues <- identify_mcmc_issues(parameters, ...)
  sampler <- sampler_diagnostic_table(fit)

  structure(
    list(
      family = fit$family,
      parameters = parameters,
      issues = issues,
      sampler = sampler,
      flagged_parameters = sum(issues$flagged),
      flagged_sampler_metrics = sum(sampler$flagged),
      interpretation = paste(
        "Flags identify diagnostics requiring inspection.",
        "Absence of flags does not establish model adequacy."
      )
    ),
    class = "gp3bayes_mcmc_quality"
  )
}

#' @export
print.gp3bayes_mcmc_quality <- function(x, ...) {
  cat("\ngp3bayes MCMC quality evidence\n")
  cat(" Family: ", x$family, "\n", sep = "")
  cat(" Flagged parameters: ", x$flagged_parameters, "\n", sep = "")
  cat(" Flagged sampler metrics: ", x$flagged_sampler_metrics, "\n", sep = "")
  cat(" ", x$interpretation, "\n", sep = "")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_mcmc_quality <- function(x, ...) {
  x$issues
}

#' Group-Level Effect Table
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param groups Optional grouping factors to retain.
#' @param probs Lower and upper credible interval probabilities.
#'
#' @return A tidy data frame of estimated group-level deviations.
#' @export
group_effect_table <- function(
  fit,
  groups = NULL,
  probs = c(0.025, 0.975)
) {
  .gp3p_validate_fit(fit)
  .gp3p_require("brms", "extract group-level effects")
  if (!is.numeric(probs) || length(probs) != 2L || anyNA(probs) ||
      probs[[1L]] < 0 || probs[[2L]] > 1 || probs[[1L]] >= probs[[2L]]) {
    .gp3p_stop("`probs` must contain two increasing probabilities.")
  }

  re <- brms::ranef(
    fit$backend_fit,
    summary = TRUE,
    probs = probs
  )
  if (!is.null(groups)) {
    missing <- setdiff(groups, names(re))
    if (length(missing)) {
      .gp3p_stop("Unknown grouping factors: ", paste(missing, collapse = ", "), ".")
    }
    re <- re[groups]
  }

  pieces <- lapply(names(re), function(group_name) {
    a <- re[[group_name]]
    if (length(dim(a)) != 3L) return(NULL)
    dn <- dimnames(a)
    level <- dn[[1L]]
    coefficient <- dn[[2L]]
    statistic <- dn[[3L]]
    estimate_name <- if ("Estimate" %in% statistic) "Estimate" else statistic[[1L]]
    se_name <- if ("Est.Error" %in% statistic) "Est.Error" else statistic[[2L]]
    q_names <- setdiff(statistic, c(estimate_name, se_name))
    if (length(q_names) < 2L) return(NULL)
    grid <- expand.grid(
      level = level,
      coefficient = coefficient,
      stringsAsFactors = FALSE
    )
    grid$group <- group_name
    grid$estimate <- as.vector(a[, , estimate_name])
    grid$se <- as.vector(a[, , se_name])
    grid$lower <- as.vector(a[, , q_names[[1L]]])
    grid$upper <- as.vector(a[, , q_names[[length(q_names)]]])
    grid[, c("group", "level", "coefficient", "estimate", "se", "lower", "upper")]
  })

  pieces <- Filter(Negate(is.null), pieces)
  if (!length(pieces)) {
    .gp3p_stop("No group-level effects could be extracted.")
  }
  do.call(rbind, pieces)
}

#' Variance-Component Posterior Table
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param probs Three posterior interval probabilities.
#'
#' @return A posterior summary table for group SDs, correlations, and residual
#'   scale where applicable.
#' @export
variance_component_table <- function(
  fit,
  probs = c(0.025, 0.5, 0.975)
) {
  posterior_interval_table(
    fit,
    regex = "^(sd_|cor_|sigma$)",
    probs = probs
  )
}

#' LOO Diagnostic Table
#'
#' @param x A `gp3bayes_psis_loo` or `loo` object.
#'
#' @return Observation-level Pareto-k diagnostics.
#' @export
loo_diagnostic_table <- function(x) {
  .gp3p_require("loo", "summarise LOO diagnostics")
  if (inherits(x, "gp3bayes_psis_loo")) {
    k <- x$pareto_k
  } else if (inherits(x, "loo")) {
    k <- loo::pareto_k_values(x)
  } else {
    .gp3p_stop("`x` must be a gp3bayes PSIS-LOO or `loo` object.")
  }
  k <- as.numeric(k)
  category <- cut(
    k,
    breaks = c(-Inf, 0.5, 0.7, 1, Inf),
    labels = c("good", "okay", "review", "severe"),
    right = FALSE
  )
  data.frame(
    observation = seq_along(k),
    pareto_k = k,
    category = as.character(category),
    flagged = !is.finite(k) | k >= 0.7,
    stringsAsFactors = FALSE
  )
}

#' LOO Summary Table
#'
#' @param x A `gp3bayes_psis_loo` or `loo` object.
#'
#' @return A tidy table of LOO estimates and standard errors.
#' @export
loo_summary_table <- function(x) {
  raw <- if (inherits(x, "gp3bayes_psis_loo")) x$raw else x
  if (!inherits(raw, "loo")) {
    .gp3p_stop("`x` must contain a valid `loo` object.")
  }
  e <- as.matrix(raw$estimates)
  data.frame(
    quantity = rownames(e),
    estimate = e[, "Estimate"],
    se = e[, "SE"],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' LOO Model-Comparison Table
#'
#' @param x A `gp3bayes_loo_comparison` or matrix returned by
#'   `loo::loo_compare()`.
#'
#' @return A data frame retaining ELPD differences and their standard errors.
#' @export
model_comparison_table <- function(x) {
  m <- if (inherits(x, "gp3bayes_loo_comparison")) x$comparison else x
  m <- as.matrix(m)
  if (!all(c("elpd_diff", "se_diff") %in% colnames(m))) {
    .gp3p_stop("The comparison object does not contain ELPD-difference columns.")
  }
  data.frame(
    model = rownames(m),
    elpd_diff = m[, "elpd_diff"],
    se_diff = m[, "se_diff"],
    automatic_selection = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' LOO Model-Weight Table
#'
#' @param x A `gp3bayes_loo_weights` object or named numeric weight vector.
#'
#' @return A data frame of model weights. The table does not select a model.
#' @export
model_weights_table <- function(x) {
  w <- if (inherits(x, "gp3bayes_loo_weights")) x$weights else x
  if (!is.numeric(w) || !length(w)) {
    .gp3p_stop("`x` must contain numeric model weights.")
  }
  nms <- names(w)
  if (is.null(nms)) nms <- paste0("model_", seq_along(w))
  data.frame(
    model = nms,
    weight = as.numeric(w),
    automatic_selection = FALSE,
    stringsAsFactors = FALSE
  )
}
