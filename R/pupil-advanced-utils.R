# Internal utilities for gp3bayes 0.5 advanced pupillometry.

.p05_require <- function(pkg, why = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Package `", pkg, "` is required")
    if (!is.null(why)) msg <- paste0(msg, " ", why)
    stop(paste0(msg, "."), call. = FALSE)
  }
  invisible(TRUE)
}

.p05_data <- function(x) {
  if (is.data.frame(x)) return(x)
  candidates <- c("data", "prepared_data", "analysis_data", "long_data", "pupil_data")
  if (is.list(x)) {
    for (nm in candidates) {
      if (is.data.frame(x[[nm]])) return(x[[nm]])
    }
  }
  for (nm in candidates) {
    z <- attr(x, nm, exact = TRUE)
    if (is.data.frame(z)) return(z)
  }
  stop(
    "Could not locate a data.frame in `prepared`. Expected the object itself to be a data.frame or to contain a data/prepared_data/analysis_data/long_data component.",
    call. = FALSE
  )
}

.p05_contract <- function(x) {
  if (is.list(x)) {
    for (nm in c("contract", "pupil_contract", "mapping", "schema")) {
      if (is.list(x[[nm]])) return(x[[nm]])
    }
  }
  for (nm in c("contract", "pupil_contract", "mapping", "schema")) {
    z <- attr(x, nm, exact = TRUE)
    if (is.list(z)) return(z)
  }
  list()
}

.p05_first_character <- function(x, names) {
  for (nm in names) {
    value <- x[[nm]]
    if (is.character(value) && length(value) == 1L && nzchar(value)) return(value)
  }
  NULL
}

.p05_detect_col <- function(data, contract, contract_names, candidates, required = TRUE, label = "column") {
  nm <- .p05_first_character(contract, contract_names)
  if (!is.null(nm) && nm %in% names(data)) return(nm)
  hit <- candidates[candidates %in% names(data)]
  if (length(hit)) return(hit[[1L]])
  if (!required) return(NULL)
  stop(
    paste0(
      "Could not resolve the ", label, ". Supply a prepared 0.4 pupil object with its contract/mapping, or use conventional columns such as: ",
      paste(candidates, collapse = ", "), "."
    ),
    call. = FALSE
  )
}

.p05_mapping <- function(prepared, require_condition = TRUE) {
  data <- .p05_data(prepared)
  contract <- .p05_contract(prepared)

  out <- list(
    response = .p05_detect_col(
      data, contract,
      c("pupil_col", "response_col", "outcome_col", "diameter_col", "pupil_response_col"),
      c("pupil", "pupil_diameter", "pupil_size", "diameter", "response", "outcome", "pupil_value"),
      TRUE, "pupil response column"
    ),
    time = .p05_detect_col(
      data, contract,
      c("time_col", "time_ms_col", "timestamp_col"),
      c("time_ms", "time", "timestamp", "time_s", "sample_time"),
      TRUE, "time column"
    ),
    participant = .p05_detect_col(
      data, contract,
      c("participant_col", "participant_id_col", "subject_col", "unit_col"),
      c("participant_id", "participant", "subject_id", "subject", "unit_id", "id"),
      TRUE, "participant column"
    ),
    condition = .p05_detect_col(
      data, contract,
      c("condition_col", "condition_id_col"),
      c("condition", "experimental_condition", "trial_condition", "group"),
      require_condition, "condition column"
    ),
    trial = .p05_detect_col(
      data, contract,
      c("trial_col", "trial_id_col"),
      c("trial_id", "trial", "epoch_id", "segment_id"),
      FALSE, "trial column"
    ),
    item = .p05_detect_col(
      data, contract,
      c("item_col", "item_id_col", "stimulus_col"),
      c("item_id", "item", "stimulus_id", "stimulus"),
      FALSE, "item column"
    )
  )

  out
}

.p05_q <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  vapply(
    x,
    function(z) {
      if (make.names(z) == z && !grepl("^[0-9]", z)) z else paste0("`", gsub("`", "\\`", z, fixed = TRUE), "`")
    },
    character(1L),
    USE.NAMES = FALSE
  )
}

.p05_rhs_join <- function(terms) {
  terms <- terms[!is.na(terms) & nzchar(terms)]
  if (!length(terms)) "1" else paste(terms, collapse = " + ")
}

.p05_safe_sd <- function(x) {
  z <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(z) || z <= 0) z <- stats::mad(x, constant = 1.4826, na.rm = TRUE)
  if (!is.finite(z) || z <= 0) z <- max(abs(stats::median(x, na.rm = TRUE)), 1)
  as.numeric(z)
}

.p05_series_data <- function(data, mapping) {
  ans <- data
  p <- mapping$participant
  tr <- mapping$trial
  t <- mapping$time

  if (is.null(tr)) {
    ans$.gp3bayes_series <- factor(ans[[p]])
  } else {
    ans$.gp3bayes_series <- interaction(ans[[p]], ans[[tr]], drop = TRUE, lex.order = TRUE)
  }

  ord <- order(ans$.gp3bayes_series, ans[[t]], seq_len(nrow(ans)))
  ans <- ans[ord, , drop = FALSE]
  ans$.gp3bayes_time_index <- stats::ave(
    seq_len(nrow(ans)),
    ans$.gp3bayes_series,
    FUN = seq_along
  )
  rownames(ans) <- NULL
  ans
}

.p05_underlying_fit <- function(x) {
  if (inherits(x, "brmsfit")) return(x)
  if (is.list(x)) {
    for (nm in c("backend_fit", "fit", "brms_fit")) {
      if (inherits(x[[nm]], "brmsfit")) return(x[[nm]])
    }
  }
  stop("Expected a brmsfit or a gp3bayes 0.5 fit containing a brmsfit.", call. = FALSE)
}

.p05_fit_spec <- function(x) {
  if (is.list(x) && inherits(x$specification, "gp3bayes_pupil_advanced_specification")) return(x$specification)
  NULL
}

.p05_quantile_summary <- function(draws, probs = c(0.025, 0.5, 0.975)) {
  stopifnot(is.matrix(draws) || is.data.frame(draws))
  m <- as.matrix(draws)
  data.frame(
    mean = colMeans(m, na.rm = TRUE),
    sd = apply(m, 2L, stats::sd, na.rm = TRUE),
    q_low = apply(m, 2L, stats::quantile, probs = probs[[1L]], na.rm = TRUE, names = FALSE),
    median = apply(m, 2L, stats::quantile, probs = probs[[2L]], na.rm = TRUE, names = FALSE),
    q_high = apply(m, 2L, stats::quantile, probs = probs[[3L]], na.rm = TRUE, names = FALSE),
    row.names = NULL,
    check.names = FALSE
  )
}

.p05_log_mean_exp <- function(x) {
  m <- max(x)
  if (!is.finite(m)) return(m)
  m + log(mean(exp(x - m)))
}

.p05_assert_probability <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 || x >= 1) {
    stop("`", name, "` must be one finite number strictly between 0 and 1.", call. = FALSE)
  }
  invisible(TRUE)
}

.p05_assert_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(TRUE)
}

.p05_check_covariates <- function(data, covariates, protected = character()) {
  if (is.null(covariates)) covariates <- character()
  if (!is.character(covariates)) stop("`covariates` must be a character vector.", call. = FALSE)
  covariates <- unique(covariates[nzchar(covariates)])
  missing <- setdiff(covariates, names(data))
  if (length(missing)) {
    stop("Unknown covariate(s): ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  bad <- intersect(covariates, protected)
  if (length(bad)) {
    stop("Structural columns cannot also be declared as covariates: ", paste(bad, collapse = ", "), ".", call. = FALSE)
  }
  covariates
}

#' Inspect the resolved 0.5 pupil column mapping
#'
#' Resolves the response, time, participant, condition, trial, and item columns
#' that the advanced 0.5 layer will use. Resolution is read-only and does not
#' modify the prepared object.
#'
#' @param prepared A 0.4 prepared pupil object or compatible data frame.
#' @return A data frame describing the resolved mapping.
#' @export
pupil_advanced_mapping_table <- function(prepared) {
  m <- .p05_mapping(prepared, require_condition = FALSE)
  data.frame(
    role = names(m),
    column = vapply(m, function(x) if (is.null(x)) NA_character_ else x, character(1L)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Report the gp3bayes 0.5 advanced-pupillometry capability boundary
#'
#' @return A data frame listing supported, experimental, and deliberately
#'   excluded capabilities.
#' @export
pupil_advanced_capabilities <- function() {
  data.frame(
    capability = c(
      "Gaussian observation model",
      "Student-t robust observation model",
      "Distributional residual scale",
      "AR(1), AR(2), bounded ARMA residual structure",
      "Spline and Gaussian-process trajectories",
      "Known measurement uncertainty",
      "MAR-oriented missing-response/predictor models",
      "Joint binocular modelling",
      "PSIS-LOO and exact K-fold comparison",
      "Explicit leave-future-out refit plans",
      "Experimental nonlinear response-shape model",
      "Automatic blink interpolation",
      "Automatic MNAR identification",
      "Automatic cognitive-state inference",
      "Automatic model winner selection",
      "Automatic causal interpretation"
    ),
    status = c(
      rep("supported", 10),
      "experimental",
      rep("excluded", 5)
    ),
    stringsAsFactors = FALSE
  )
}

.p05_assert_integerish <- function(x, name, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x != as.integer(x) || x < lower || x > upper) {
    stop(
      "`", name, "` must be one integer in [", format(lower), ", ", format(upper), "].",
      call. = FALSE
    )
  }
  as.integer(x)
}

.p05_assert_fraction <- function(x, name, upper_inclusive = FALSE) {
  upper_ok <- if (upper_inclusive) x <= 1 else x < 1
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 || !upper_ok) {
    stop(
      "`", name, "` must be one finite fraction in [0, 1", if (upper_inclusive) "]" else ")", ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}
