.gp3b_validate_gp3bayes_fit <- function(
  fit,
  required_class = NULL
) {
  if (!inherits(fit, "gp3bayes_fit")) {
    .gp3b_stop(
      "`fit` must inherit from `gp3bayes_fit`."
    )
  }

  if (
    !is.null(required_class) &&
      !inherits(fit, required_class)
  ) {
    .gp3b_stop(
      "`fit` must inherit from `",
      required_class,
      "`."
    )
  }

  if (
    is.null(fit$backend_fit) ||
      !inherits(fit$backend_fit, "brmsfit")
  ) {
    .gp3b_stop(
      "`fit$backend_fit` must be a fitted `brmsfit` object."
    )
  }

  if (
    is.null(fit$backend_fit$fit) ||
      !inherits(fit$backend_fit$fit, "stanfit")
  ) {
    .gp3b_stop(
      "`fit$backend_fit$fit` must be a fitted `stanfit` object."
    )
  }

  invisible(fit)
}

.gp3b_validate_probability <- function(
  value,
  name,
  open = FALSE
) {
  .gp3b_assert_numeric_scalar(
    value,
    name,
    lower = 0,
    upper = 1,
    lower_open = open,
    upper_open = open
  )
}

.gp3b_with_seed <- function(seed, code) {
  seed <- .gp3b_assert_integer(
    seed,
    "seed",
    minimum = 0L
  )

  had_seed <- exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  )

  if (had_seed) {
    old_seed <- get(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  }

  on.exit(
    {
      if (had_seed) {
        assign(
          ".Random.seed",
          old_seed,
          envir = .GlobalEnv
        )
      } else if (
        exists(
          ".Random.seed",
          envir = .GlobalEnv,
          inherits = FALSE
        )
      ) {
        rm(
          ".Random.seed",
          envir = .GlobalEnv
        )
      }
    },
    add = TRUE
  )

  set.seed(seed)
  force(code)
}

.gp3b_classify_upper <- function(
  value,
  pass,
  review
) {
  if (
    length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)
  ) {
    return("not_assessed")
  }

  if (value <= pass) {
    "pass"
  } else if (value <= review) {
    "review"
  } else {
    "fail"
  }
}

.gp3b_classify_lower <- function(
  value,
  pass,
  review
) {
  if (
    length(value) != 1L ||
      is.na(value) ||
      !is.finite(value)
  ) {
    return("not_assessed")
  }

  if (value >= pass) {
    "pass"
  } else if (value >= review) {
    "review"
  } else {
    "fail"
  }
}


.gp3b_safe_max <- function(x) {
  x <- x[
    is.finite(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  max(x)
}

.gp3b_safe_min <- function(x) {
  x <- x[
    is.finite(x)
  ]

  if (length(x) == 0L) {
    return(NA_real_)
  }

  min(x)
}

.gp3b_worst_status <- function(status) {
  status <- as.character(status)
  status <- status[
    !is.na(status) &
      nzchar(status)
  ]

  if (length(status) == 0L) {
    return("review")
  }

  if ("fail" %in% status) {
    return("fail")
  }

  if (
    any(
      status %in% c(
        "review",
        "not_assessed",
        "not_applicable"
      )
    )
  ) {
    return("review")
  }

  "pass"
}

.gp3b_parameter_variables <- function(
  backend_fit,
  include = NULL
) {
  .gp3b_require_namespace(
    "posterior",
    "extract posterior draws"
  )

  available <- posterior::variables(
    posterior::as_draws_array(
      backend_fit
    )
  )

  default <- grep(
    "^(b_|sd_|cor_|sigma$)",
    available,
    value = TRUE
  )

  if (is.null(include)) {
    selected <- default
  } else {
    if (
      !is.character(include) ||
        anyNA(include) ||
        any(!nzchar(include)) ||
        anyDuplicated(include)
    ) {
      .gp3b_stop(
        "`variables` must be a character vector of unique ",
        "non-empty posterior variable names."
      )
    }

    missing <- setdiff(
      include,
      available
    )

    if (length(missing) > 0L) {
      .gp3b_stop(
        "Requested posterior variables were not found: ",
        paste(missing, collapse = ", "),
        "."
      )
    }

    selected <- include
  }

  if (length(selected) == 0L) {
    .gp3b_stop(
      "No supported posterior parameters were found."
    )
  }

  selected
}

.gp3b_extract_draws <- function(
  fit,
  variables = NULL
) {
  .gp3b_validate_gp3bayes_fit(fit)

  .gp3b_require_namespace(
    "posterior",
    "extract posterior draws"
  )

  selected <- .gp3b_parameter_variables(
    fit$backend_fit,
    include = variables
  )

  posterior::as_draws_array(
    fit$backend_fit,
    variable = selected
  )
}


.gp3b_current_package_version <- function() {
  installed <- tryCatch(
    as.character(
      utils::packageVersion(
        "gp3bayes"
      )
    ),
    error = function(error) {
      NA_character_
    }
  )

  if (!is.na(installed)) {
    return(installed)
  }

  if (file.exists("DESCRIPTION")) {
    description <- tryCatch(
      read.dcf(
        "DESCRIPTION",
        fields = "Version"
      ),
      error = function(error) {
        NULL
      }
    )

    if (
      !is.null(description) &&
        length(description) == 1L
    ) {
      return(
        as.character(
          description[[1L]]
        )
      )
    }
  }

  "unknown"
}

.gp3b_markdown_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- "NA"
  gsub(
    "|",
    "\\\\|",
    x,
    fixed = TRUE
  )
}

.gp3b_markdown_table <- function(
  x,
  digits = 3L,
  max_rows = Inf
) {
  if (
    !is.data.frame(x) ||
      ncol(x) == 0L
  ) {
    return("_Not available._")
  }

  if (
    is.finite(max_rows) &&
      nrow(x) > max_rows
  ) {
    x <- x[
      seq_len(max_rows),
      ,
      drop = FALSE
    ]
  }

  formatted <- lapply(
    x,
    function(column) {
      if (is.numeric(column)) {
        out <- formatC(
          column,
          digits = digits,
          format = "fg",
          flag = "#"
        )
        out[is.na(column)] <- "NA"
        out
      } else {
        .gp3b_markdown_escape(column)
      }
    }
  )

  formatted <- as.data.frame(
    formatted,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  header <- paste0(
    "| ",
    paste(
      .gp3b_markdown_escape(names(formatted)),
      collapse = " | "
    ),
    " |"
  )

  divider <- paste0(
    "| ",
    paste(
      rep("---", ncol(formatted)),
      collapse = " | "
    ),
    " |"
  )

  body <- if (nrow(formatted) > 0L) {
    apply(
      formatted,
      1L,
      function(row) {
        paste0(
          "| ",
          paste(row, collapse = " | "),
          " |"
        )
      }
    )
  } else {
    character()
  }

  c(
    header,
    divider,
    body
  )
}

.gp3b_summary_interval <- function(
  draws,
  probability
) {
  probability <- .gp3b_validate_probability(
    probability,
    "probability",
    open = TRUE
  )

  alpha <- 1 - probability

  lower_function <- function(x) {
    stats::quantile(
      x,
      probs = alpha / 2,
      names = FALSE,
      type = 8
    )
  }

  upper_function <- function(x) {
    stats::quantile(
      x,
      probs = 1 - alpha / 2,
      names = FALSE,
      type = 8
    )
  }

  posterior::summarise_draws(
    draws,
    mean = mean,
    median = stats::median,
    sd = stats::sd,
    lower = lower_function,
    upper = upper_function,
    probability_positive = function(x) {
      mean(x > 0)
    },
    rhat = posterior::rhat,
    ess_bulk = posterior::ess_bulk,
    ess_tail = posterior::ess_tail
  )
}

.gp3b_sampler_chain_table <- function(
  fit,
  max_treedepth
) {
  sampler <- brms::nuts_params(
    fit$backend_fit
  )

  required <- c(
    "Chain",
    "Iteration",
    "Parameter",
    "Value"
  )

  if (!all(required %in% names(sampler))) {
    .gp3b_stop(
      "`brms::nuts_params()` returned an unsupported structure."
    )
  }

  chains <- sort(
    unique(
      sampler$Chain
    )
  )

  rows <- lapply(
    chains,
    function(chain) {
      chain_data <- sampler[
        sampler$Chain == chain,
        ,
        drop = FALSE
      ]

      extract_parameter <- function(parameter) {
        values <- chain_data$Value[
          chain_data$Parameter == parameter
        ]

        as.numeric(values)
      }

      divergent <- extract_parameter(
        "divergent__"
      )
      treedepth <- extract_parameter(
        "treedepth__"
      )
      energy <- extract_parameter(
        "energy__"
      )

      ebfmi <- NA_real_

      if (
        length(energy) >= 3L &&
          stats::var(energy) > 0
      ) {
        ebfmi <- mean(
          diff(energy)^2
        ) / stats::var(energy)
      }

      data.frame(
        chain = as.integer(chain),
        iterations = max(
          length(divergent),
          length(treedepth),
          length(energy)
        ),
        divergences = if (length(divergent)) {
          sum(divergent > 0)
        } else {
          NA_integer_
        },
        treedepth_hits = if (length(treedepth)) {
          sum(treedepth >= max_treedepth)
        } else {
          NA_integer_
        },
        treedepth_hit_fraction = if (length(treedepth)) {
          mean(treedepth >= max_treedepth)
        } else {
          NA_real_
        },
        ebfmi = ebfmi,
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(
    rbind,
    rows
  )
}

.gp3b_diagnose_fit <- function(
  fit,
  required_class,
  rhat_pass = 1.01,
  rhat_fail = 1.05,
  ess_per_chain_pass = 100,
  ess_per_chain_fail = 50,
  maximum_treedepth_fraction = 0.01,
  ebfmi_pass = 0.30,
  ebfmi_fail = 0.20
) {
  .gp3b_validate_gp3bayes_fit(
    fit,
    required_class = required_class
  )

  .gp3b_require_namespace(
    "posterior",
    "compute posterior sampling diagnostics"
  )

  rhat_pass <- .gp3b_assert_numeric_scalar(
    rhat_pass,
    "rhat_pass",
    lower = 1
  )
  rhat_fail <- .gp3b_assert_numeric_scalar(
    rhat_fail,
    "rhat_fail",
    lower = rhat_pass
  )
  ess_per_chain_pass <- .gp3b_assert_numeric_scalar(
    ess_per_chain_pass,
    "ess_per_chain_pass",
    lower = 1
  )
  ess_per_chain_fail <- .gp3b_assert_numeric_scalar(
    ess_per_chain_fail,
    "ess_per_chain_fail",
    lower = 1,
    upper = ess_per_chain_pass
  )
  maximum_treedepth_fraction <- .gp3b_validate_probability(
    maximum_treedepth_fraction,
    "maximum_treedepth_fraction"
  )
  ebfmi_pass <- .gp3b_assert_numeric_scalar(
    ebfmi_pass,
    "ebfmi_pass",
    lower = 0
  )
  ebfmi_fail <- .gp3b_assert_numeric_scalar(
    ebfmi_fail,
    "ebfmi_fail",
    lower = 0,
    upper = ebfmi_pass
  )

  draws <- .gp3b_extract_draws(fit)

  parameter_table <- as.data.frame(
    posterior::summarise_draws(
      draws
    ),
    stringsAsFactors = FALSE
  )

  chains <- posterior::nchains(draws)

  parameter_table$ess_bulk_per_chain <-
    parameter_table$ess_bulk / chains
  parameter_table$ess_tail_per_chain <-
    parameter_table$ess_tail / chains

  parameter_table$rhat_status <- vapply(
    parameter_table$rhat,
    .gp3b_classify_upper,
    character(1),
    pass = rhat_pass,
    review = rhat_fail
  )

  parameter_table$ess_bulk_status <- vapply(
    parameter_table$ess_bulk_per_chain,
    .gp3b_classify_lower,
    character(1),
    pass = ess_per_chain_pass,
    review = ess_per_chain_fail
  )

  parameter_table$ess_tail_status <- vapply(
    parameter_table$ess_tail_per_chain,
    .gp3b_classify_lower,
    character(1),
    pass = ess_per_chain_pass,
    review = ess_per_chain_fail
  )

  maximum_tree_depth <- if (
    !is.null(fit$sampling$max_treedepth)
  ) {
    as.integer(
      fit$sampling$max_treedepth
    )
  } else {
    10L
  }

  chain_table <- .gp3b_sampler_chain_table(
    fit,
    max_treedepth = maximum_tree_depth
  )

  total_divergences <- if (
    all(is.na(chain_table$divergences))
  ) {
    NA_real_
  } else {
    sum(
      chain_table$divergences,
      na.rm = TRUE
    )
  }

  maximum_tree_fraction <- if (
    all(is.na(chain_table$treedepth_hit_fraction))
  ) {
    NA_real_
  } else {
    max(
      chain_table$treedepth_hit_fraction,
      na.rm = TRUE
    )
  }

  minimum_ebfmi <- if (
    all(is.na(chain_table$ebfmi))
  ) {
    NA_real_
  } else {
    min(
      chain_table$ebfmi,
      na.rm = TRUE
    )
  }

  component_table <- data.frame(
    component = c(
      "rhat",
      "bulk_ess_per_chain",
      "tail_ess_per_chain",
      "divergences",
      "treedepth_saturation",
      "energy_ebfmi"
    ),
    observed = c(
      .gp3b_safe_max(
        parameter_table$rhat
      ),
      .gp3b_safe_min(
        parameter_table$ess_bulk_per_chain
      ),
      .gp3b_safe_min(
        parameter_table$ess_tail_per_chain
      ),
      total_divergences,
      maximum_tree_fraction,
      minimum_ebfmi
    ),
    pass_rule = c(
      paste0("<=", rhat_pass),
      paste0(">=", ess_per_chain_pass),
      paste0(">=", ess_per_chain_pass),
      "0",
      "0",
      paste0(">=", ebfmi_pass)
    ),
    review_rule = c(
      paste0(">", rhat_pass, " and <=", rhat_fail),
      paste0(">=", ess_per_chain_fail, " and <", ess_per_chain_pass),
      paste0(">=", ess_per_chain_fail, " and <", ess_per_chain_pass),
      "not used",
      paste0(">0 and <=", maximum_treedepth_fraction),
      paste0(">=", ebfmi_fail, " and <", ebfmi_pass)
    ),
    stringsAsFactors = FALSE
  )

  component_table$status <- c(
    .gp3b_classify_upper(
      component_table$observed[[1L]],
      pass = rhat_pass,
      review = rhat_fail
    ),
    .gp3b_classify_lower(
      component_table$observed[[2L]],
      pass = ess_per_chain_pass,
      review = ess_per_chain_fail
    ),
    .gp3b_classify_lower(
      component_table$observed[[3L]],
      pass = ess_per_chain_pass,
      review = ess_per_chain_fail
    ),
    if (
      is.na(component_table$observed[[4L]])
    ) {
      "not_assessed"
    } else if (
      component_table$observed[[4L]] == 0
    ) {
      "pass"
    } else {
      "fail"
    },
    if (
      is.na(component_table$observed[[5L]])
    ) {
      "not_assessed"
    } else if (
      component_table$observed[[5L]] == 0
    ) {
      "pass"
    } else if (
      component_table$observed[[5L]] <=
        maximum_treedepth_fraction
    ) {
      "review"
    } else {
      "fail"
    },
    .gp3b_classify_lower(
      component_table$observed[[6L]],
      pass = ebfmi_pass,
      review = ebfmi_fail
    )
  )

  overall_status <- .gp3b_worst_status(
    component_table$status
  )

  structure(
    list(
      diagnostic_version = "0.1",
      family = fit$family,
      status = overall_status,
      component_table = component_table,
      parameter_table = parameter_table,
      chain_table = chain_table,
      thresholds = list(
        rhat_pass = rhat_pass,
        rhat_fail = rhat_fail,
        ess_per_chain_pass = ess_per_chain_pass,
        ess_per_chain_fail = ess_per_chain_fail,
        maximum_treedepth_fraction =
          maximum_treedepth_fraction,
        ebfmi_pass = ebfmi_pass,
        ebfmi_fail = ebfmi_fail
      ),
      diagnostics_assessed = TRUE,
      convergence_claim = FALSE,
      posterior_adequacy_established = FALSE,
      interpretation = paste(
        "The status reports whether prespecified numerical sampling",
        "thresholds were met. It is not an automatic declaration of",
        "convergence or posterior adequacy."
      )
    ),
    class = c(
      paste0(
        "gp3bayes_",
        fit$family,
        "_diagnostics"
      ),
      "gp3bayes_sampling_diagnostics"
    )
  )
}

#' Plot Sampling Diagnostics
#'
#' Produces trace, energy, treedepth, or divergence plots for an approved
#' fitted `gp3bayes` model.
#'
#' @param fit A fitted `gp3bayes_fit`.
#' @param type One of `"trace"`, `"energy"`, `"treedepth"`, or
#'   `"divergence"`.
#' @param variables Optional posterior parameter names used for trace plots.
#'
#' @return A plot object created by `bayesplot`.
#'
#' @details
#' Diagnostic plots support interpretation of sampling behaviour. They do not
#' establish convergence or substantive model adequacy by themselves.
#'
#' @export
plot_sampling_diagnostics <- function(
  fit,
  type = c(
    "trace",
    "energy",
    "treedepth",
    "divergence"
  ),
  variables = NULL
) {
  .gp3b_validate_gp3bayes_fit(fit)

  .gp3b_require_namespace(
    "posterior",
    "extract posterior draws for plotting"
  )
  .gp3b_require_namespace(
    "bayesplot",
    "plot posterior sampling diagnostics"
  )

  type <- match.arg(type)

  sampler <- brms::nuts_params(
    fit$backend_fit
  )

  if (identical(type, "energy")) {
    return(
      bayesplot::mcmc_nuts_energy(
        sampler
      )
    )
  }

  if (
    type %in% c(
      "treedepth",
      "divergence"
    )
  ) {
    log_posterior <- brms::log_posterior(
      fit$backend_fit
    )

    if (identical(type, "treedepth")) {
      return(
        bayesplot::mcmc_nuts_treedepth(
          sampler,
          log_posterior
        )
      )
    }

    return(
      bayesplot::mcmc_nuts_divergence(
        sampler,
        log_posterior
      )
    )
  }

  selected <- .gp3b_parameter_variables(
    fit$backend_fit,
    include = variables
  )

  if (
    is.null(variables) &&
      length(selected) > 8L
  ) {
    selected <- selected[
      seq_len(8L)
    ]
  }

  draws <- posterior::as_draws_array(
    fit$backend_fit,
    variable = selected
  )

  bayesplot::mcmc_trace(
    as.array(draws),
    pars = selected,
    np = sampler
  )
}

#' @export
print.gp3bayes_sampling_diagnostics <- function(
  x,
  ...
) {
  cat("<gp3bayes_sampling_diagnostics>\n")
  cat(
    "  Family: ",
    x$family,
    "\n",
    sep = ""
  )
  cat(
    "  Threshold status: ",
    x$status,
    "\n",
    sep = ""
  )
  cat("  Diagnostics assessed: TRUE\n")
  cat("  Automatic convergence claim: FALSE\n")
  cat("  Posterior adequacy established: FALSE\n")

  invisible(x)
}
