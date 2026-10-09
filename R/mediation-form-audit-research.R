#' Compare linear and quadratic mediator-outcome specifications
#'
#' Descriptive model-form sensitivity for repeated-measure experimental
#' mediation data. The participant-clustered percentile bootstrap refits
#' both models for each resample and records failures. Does NOT establish
#' sequential ignorability, a Bayesian posterior or a causal natural effect.
#'
#' @param data Trial-level data frame.
#' @param treatment,mediator,outcome,participant Column-name strings.
#' @param resamples Number of cluster-bootstrap replicates (>=20).
#' @param seed Integer seed (restored on function exit).
#' @return List with estimates, bootstrap ledger and claim boundary.
#' @export
audit_mediation_functional_form <- function(
    data, treatment, mediator, outcome, participant = "participant_id",
    resamples = 199L, seed = 2026L) {
  cols <- c(treatment, mediator, outcome, participant)
  if (!is.data.frame(data) || !nrow(data) ||
      anyDuplicated(cols) || !all(cols %in% names(data))) {
    stop("Data and four distinct existing column names required.", call. = FALSE)
  }
  if (!is.numeric(resamples) || length(resamples) != 1L ||
      !is.finite(resamples) || resamples < 20L ||
      resamples != floor(resamples) ||
      !is.numeric(seed) || length(seed) != 1L ||
      !is.finite(seed) || seed != floor(seed)) {
    stop("resamples >=20 and integer seed required.", call. = FALSE)
  }
  x <- data[[treatment]]
  m <- data[[mediator]]
  y <- data[[outcome]]
  id <- as.character(data[[participant]])
  if (!is.numeric(x) || !is.numeric(m) || !is.numeric(y) ||
      anyNA(x) || anyNA(m) || anyNA(y) || anyNA(id) ||
      any(!is.finite(c(x, m, y))) || !all(x %in% c(0, 1))) {
    stop("Treatment must be 0/1 and all inputs finite/complete.", call. = FALSE)
  }
  if (length(unique(id)) < 4L) {
    stop("At least four independent participants required.", call. = FALSE)
  }
  .estimate <- function(rows, form) {
    d <- data.frame(x = x[rows], m = m[rows], y = y[rows])
    med <- stats::lm(m ~ x, data = d)
    if (med$rank < 2L) stop("Mediator design rank deficient.")
    # Both models include treatment x mediator interaction. The nonlinear
    # comparator includes a quadratic response shape and its interaction.
    formula <- if (form == "linear") {
      y ~ x * m
    } else {
      y ~ x * m + I(m^2) + x:I(m^2)
    }
    fitted <- stats::lm(formula, data = d)
    if (fitted$rank < length(stats::coef(fitted))) {
      stop("Outcome design rank deficient.")
    }
    resid <- stats::residuals(med)
    baseline <- unname(stats::coef(med)[1L]) + resid
    shift <- unname(stats::coef(med)[2L])
    at0 <- data.frame(x = 1, m = baseline)
    at1 <- data.frame(x = 1, m = baseline + shift)
    mean(stats::predict(fitted, newdata = at1) -
         stats::predict(fitted, newdata = at0))
  }
  rows <- seq_len(nrow(data))
  base <- c(linear = .estimate(rows, "linear"),
            quadratic = .estimate(rows, "quadratic"))
  slices <- split(rows, id)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) prior_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) assign(".Random.seed", prior_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  vals <- matrix(NA_real_, nrow = resamples, ncol = 2L,
                 dimnames = list(NULL, c("linear", "quadratic")))
  errors <- matrix("", nrow = resamples, ncol = 2L,
                   dimnames = list(NULL, c("linear", "quadratic")))
  for (b in seq_len(resamples)) {
    chosen <- sample.int(length(slices), size = length(slices), replace = TRUE)
    sample_rows <- unlist(slices[chosen], use.names = FALSE)
    for (form in colnames(vals)) {
      result <- tryCatch(.estimate(sample_rows, form),
                         error = function(e) e)
      if (inherits(result, "error")) {
        errors[b, form] <- conditionMessage(result)
      } else {
        vals[b, form] <- result
      }
    }
  }
  result <- do.call(rbind, lapply(names(base), function(form) {
    valid <- vals[is.finite(vals[, form]), form]
    data.frame(
      model = form, mediator_shift_estimate = unname(base[form]),
      bootstrap_successes = length(valid),
      bootstrap_failures = resamples - length(valid),
      percentile_lower = if (length(valid)) {
        unname(stats::quantile(valid, .025))
      } else NA_real_,
      percentile_upper = if (length(valid)) {
        unname(stats::quantile(valid, .975))
      } else NA_real_
    )
  }))
  rownames(result) <- NULL
  list(
    estimates = result,
    bootstrap_ledger = data.frame(
      replicate = seq_len(resamples),
      linear = vals[, "linear"],
      quadratic = vals[, "quadratic"],
      linear_error = errors[, "linear"],
      quadratic_error = errors[, "quadratic"]
    ),
    n_participants = length(slices),
    estimand = "model_based_mediator_shift_at_treatment_one",
    claim_boundary = paste(
      "Only Gaussian continuous outcome/mediator, 0/1 treatment, and",
      "prespecified quadratic model comparison. Participant bootstrap",
      "is descriptive; no causal identification, GAM discovery or",
      "Bayesian posterior claim."
    )
  )
}
