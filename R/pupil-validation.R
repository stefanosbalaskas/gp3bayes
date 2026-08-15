
.gp3p_balanced_group_folds <- function(group, K, seed) {
  levels_group <- unique(as.character(group))
  if (length(levels_group) < K) {
    .gp3p_stop("Validation requires at least `K` distinct validation groups.")
  }
  shuffled <- withr::with_seed(seed, sample(levels_group))
  assignments <- rep(seq_len(K), length.out = length(shuffled))
  map <- stats::setNames(assignments, shuffled)
  unname(map[as.character(group)])
}

.gp3p_within_participant_trial_folds <- function(participant, trial, K, seed) {
  participant <- as.character(participant)
  trial <- as.character(trial)
  out <- integer(length(participant))
  participant_levels <- unique(participant)
  for (p in participant_levels) {
    idx <- which(participant == p)
    trials <- unique(trial[idx])
    if (length(trials) < 2L) {
      .gp3p_stop(
        "Target `new_trial_known_participant` requires at least two trials for every participant."
      )
    }
    k_local <- min(K, length(trials))
    shuffled <- withr::with_seed(
      seed + match(p, participant_levels), sample(trials)
    )
    map <- stats::setNames(
      rep(seq_len(k_local), length.out = length(shuffled)), shuffled
    )
    out[idx] <- map[trial[idx]]
  }
  out
}

#' Create an explicit pupil predictive-validation plan
#'
#' Defines the prediction target before choosing a partition. The plan
#' distinguishes observation-level known-trial prediction, new trials for
#' known participants, new participants, and finite future time segments.
#' Only non-missing model-outcome rows enter validation partitions.
#'
#' @param x A prepared pupil object, pupil specification, or pupil fit.
#' @param target One of `"new_sample_known_trial"`,
#'   `"new_trial_known_participant"`, `"new_participant"`, or
#'   `"future_segment"`.
#' @param K Number of folds for K-fold targets.
#' @param future_fraction Fraction at the end of each trial series held out for
#'   the future-segment target.
#' @param seed Reproducibility seed.
#' @return A `gp3bayes_pupil_validation_plan` with explicit fold/split
#'   membership and leakage checks.
#' @section Interpretation:
#' Observation-wise validation is not presented as a universal default for
#' temporally dependent pupil samples. The declared prediction target
#' determines the partition.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
create_pupil_validation_plan <- function(
    x,
    target = c(
      "new_trial_known_participant", "new_participant",
      "future_segment", "new_sample_known_trial"
    ),
    K = 5L,
    future_fraction = 0.20,
    seed = 2026) {

  target <- match.arg(target)
  K <- .gp3p_positive(K, "K", TRUE)
  future_fraction <- .gp3p_probability(
    future_fraction, "future_fraction", TRUE
  )
  seed <- as.integer(seed)
  if (length(seed) != 1L || is.na(seed)) {
    .gp3p_stop("`seed` must be one integer.")
  }

  prepared <- if (inherits(x, "gp3bayes_pupil_prepared")) {
    x
  } else if (inherits(x, "gp3bayes_pupil_model_specification")) {
    x$prepared
  } else if (inherits(x, "gp3bayes_pupil_fit")) {
    x$specification$prepared
  } else {
    .gp3p_stop("`x` must be a pupil prepared object, specification, or fit.")
  }

  model_rows <- which(!is.na(prepared$data$.pupil_model))
  d <- prepared$data[model_rows, , drop = FALSE]
  rownames(d) <- NULL
  n <- nrow(d)
  if (n < 4L) .gp3p_stop("Validation requires at least four non-missing pupil observations.")

  fold_id <- rep(NA_integer_, n)
  split_table <- data.frame()
  strategy <- ""
  qualification <- ""

  if (identical(target, "new_sample_known_trial")) {
    K <- min(K, n)
    if (K < 2L) .gp3p_stop("Observation K-fold requires at least two folds.")
    fold_id <- withr::with_seed(
      seed, sample(rep(seq_len(K), length.out = n))
    )
    strategy <- "observation_kfold"
    qualification <- paste(
      "Observation-level K-fold conditions on the observed trial/participant",
      "structure and assesses a new sample within known hierarchy; it is not",
      "a future-time or new-group validation target."
    )
  } else if (identical(target, "new_trial_known_participant")) {
    fold_id <- .gp3p_within_participant_trial_folds(
      d$.participant, d$.trial, K, seed
    )
    K <- max(fold_id)
    strategy <- "grouped_trial_kfold_within_participant"
    qualification <- paste(
      "All samples from a participant-trial series remain in one fold;",
      "participants remain represented through their other trials."
    )
  } else if (identical(target, "new_participant")) {
    K_eff <- min(K, nlevels(d$.participant))
    if (K_eff < 2L) {
      .gp3p_stop("New-participant validation requires at least two participants.")
    }
    fold_id <- .gp3p_balanced_group_folds(d$.participant, K_eff, seed)
    K <- K_eff
    strategy <- "grouped_participant_kfold"
    qualification <- "All samples from a participant remain in one fold."
  } else {
    series <- split(seq_len(n), d$.series_id)
    split_table <- do.call(
      rbind,
      lapply(
        names(series),
        function(s) {
          idx <- series[[s]]
          idx <- idx[order(d$.event_time[idx])]
          n_test <- max(1L, floor(length(idx) * future_fraction))
          cut <- length(idx) - n_test
          if (cut < 2L) {
            .gp3p_stop(
              "Future-segment split leaves too little training data in series ", s, "."
            )
          }
          data.frame(
            row = idx,
            source_row = d$.source_row[idx],
            series_id = s,
            role = c(rep("train", cut), rep("test", n_test)),
            event_time = d$.event_time[idx],
            stringsAsFactors = FALSE
          )
        }
      )
    )
    split_table <- split_table[order(split_table$row), , drop = FALSE]
    strategy <- "leave_future_segment_out"
    qualification <- paste(
      "Training observations precede held-out observations within each series;",
      "this is a finite declared future segment, not an infinite-horizon forecast."
    )
  }

  leakage <- if (identical(target, "new_participant")) {
    any(vapply(
      split(seq_len(n), d$.participant),
      function(idx) length(unique(fold_id[idx])) > 1L,
      logical(1)
    ))
  } else if (identical(target, "new_trial_known_participant")) {
    any(vapply(
      split(seq_len(n), d$.series_id),
      function(idx) length(unique(fold_id[idx])) > 1L,
      logical(1)
    ))
  } else if (identical(target, "future_segment")) {
    any(vapply(
      split(split_table, split_table$series_id),
      function(z) {
        tr <- z$event_time[z$role == "train"]
        te <- z$event_time[z$role == "test"]
        max(tr) >= min(te)
      },
      logical(1)
    ))
  } else {
    FALSE
  }

  structure(
    list(
      plan_version = "0.4-pupil-1",
      family = "pupil",
      target = target,
      strategy = strategy,
      K = if (identical(target, "future_segment")) NA_integer_ else K,
      fold_id = if (identical(target, "future_segment")) NULL else fold_id,
      split_table = split_table,
      model_row_index = model_rows,
      source_rows = d$.source_row,
      future_fraction = future_fraction,
      seed = seed,
      n_rows = n,
      leakage_detected = leakage,
      qualification = qualification,
      automatic_strategy_selection = FALSE
    ),
    class = "gp3bayes_pupil_validation_plan"
  )
}

#' Validate a pupil model for an explicit prediction target
#'
#' Executes exact target-specific K-fold through `brms::kfold()` for K-fold
#' targets, or a finite leave-future-segment refit for the future target.
#' Execution is opt-in because it can be computationally expensive.
#'
#' @param fit A fitted pupil model.
#' @param plan A pupil validation plan.
#' @param execute Whether to execute refitting. `FALSE` returns validated
#'   partition evidence without refitting.
#' @param ndraws Draws retained for finite future-segment prediction scoring.
#' @param max_cells Memory guard for future-segment predictions.
#' @return A `gp3bayes_pupil_validation`.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
validate_pupil_model <- function(
    fit, plan, execute = FALSE, ndraws = 200L, max_cells = 3000000L) {
  if (!inherits(fit, "gp3bayes_pupil_fit") || !isTRUE(fit$fit_performed)) {
    .gp3p_stop("`fit` must be a fitted pupil model.")
  }
  if (!inherits(plan, "gp3bayes_pupil_validation_plan")) {
    .gp3p_stop("`plan` must be created by `create_pupil_validation_plan()`.")
  }
  execute <- .gp3p_flag(execute, "execute")
  ndraws <- .gp3p_positive(ndraws, "ndraws", TRUE)
  max_cells <- .gp3p_positive(max_cells, "max_cells", TRUE)
  if (isTRUE(plan$leakage_detected)) {
    .gp3p_stop("Validation plan failed its leakage check.")
  }
  if (nrow(fit$translation$data) != plan$n_rows ||
      !identical(as.numeric(fit$translation$data$.source_row),
                 as.numeric(plan$source_rows))) {
    .gp3p_stop(
      "Validation plan does not match the fitted model rows. Recreate the plan from this fit."
    )
  }

  if (!execute) {
    return(structure(
      list(
        target = plan$target,
        strategy = plan$strategy,
        executed = FALSE,
        plan = plan,
        result = NULL,
        table = data.frame(
          target = plan$target,
          strategy = plan$strategy,
          executed = FALSE,
          leakage_detected = plan$leakage_detected,
          qualification = plan$qualification,
          stringsAsFactors = FALSE
        ),
        validity_certified = FALSE
      ),
      class = "gp3bayes_pupil_validation"
    ))
  }

  if (!requireNamespace("brms", quietly = TRUE)) {
    .gp3p_stop("Package `brms` is required to execute pupil validation.")
  }

  if (!identical(plan$target, "future_segment")) {
    kfold <- brms::kfold(
      fit$backend_fit,
      K = plan$K,
      folds = plan$fold_id,
      save_fits = FALSE,
      cores = min(2L, fit$sampling$cores)
    )
    estimates <- as.data.frame(kfold$estimates)
    estimates$quantity <- rownames(kfold$estimates)
    rownames(estimates) <- NULL
    table <- data.frame(
      target = plan$target,
      strategy = plan$strategy,
      quantity = estimates$quantity,
      estimate = estimates$Estimate,
      se = estimates$SE,
      stringsAsFactors = FALSE
    )
    result <- kfold
  } else {
    d <- fit$translation$data
    st <- plan$split_table
    role_map <- stats::setNames(st$role, st$source_row)
    roles <- unname(role_map[as.character(d$.source_row)])
    if (anyNA(roles)) {
      .gp3p_stop("Future validation split could not be mapped to all fitted rows.")
    }
    train <- d[roles == "train", , drop = FALSE]
    test <- d[roles == "test", , drop = FALSE]
    if (!nrow(train) || !nrow(test)) {
      .gp3p_stop("Future split produced empty train or test data.")
    }
    if (nrow(test) * ndraws > max_cells) {
      .gp3p_stop("Future validation exceeds `max_cells`.")
    }

    refit <- stats::update(
      fit$backend_fit,
      newdata = train,
      recompile = FALSE,
      cores = min(2L, fit$sampling$cores),
      refresh = 0L
    )
    yrep <- brms::posterior_predict(
      refit, newdata = test, ndraws = ndraws, allow_new_levels = FALSE
    )
    epred <- brms::posterior_epred(
      refit, newdata = test, ndraws = ndraws, allow_new_levels = FALSE
    )
    mu <- colMeans(epred)
    error <- test$.pupil_model - mu
    lower <- apply(
      yrep, 2L, stats::quantile, probs = 0.05, names = FALSE, type = 8
    )
    upper <- apply(
      yrep, 2L, stats::quantile, probs = 0.95, names = FALSE, type = 8
    )
    table <- data.frame(
      target = plan$target,
      strategy = plan$strategy,
      n_train = nrow(train),
      n_test = nrow(test),
      rmse = sqrt(mean(error^2)),
      mae = mean(abs(error)),
      predictive_coverage_90 = mean(
        test$.pupil_model >= lower & test$.pupil_model <= upper
      ),
      stringsAsFactors = FALSE
    )
    result <- list(refit = refit, test = test, yrep = yrep)
  }

  structure(
    list(
      target = plan$target,
      strategy = plan$strategy,
      executed = TRUE,
      plan = plan,
      result = result,
      table = table,
      validity_certified = FALSE
    ),
    class = "gp3bayes_pupil_validation"
  )
}

#' Extract a pupil validation table
#' @param x A pupil validation object.
#' @return A data frame.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
pupil_validation_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_validation")) {
    .gp3p_stop("`x` must be a pupil validation object.")
  }
  x$table
}

#' @export
print.gp3bayes_pupil_validation_plan <- function(x, ...) {
  cat("<gp3bayes_pupil_validation_plan>\n")
  cat("  Target: ", x$target, "\n", sep = "")
  cat("  Strategy: ", x$strategy, "\n", sep = "")
  cat("  Model-support rows: ", x$n_rows, "\n", sep = "")
  cat("  Leakage detected: ", x$leakage_detected, "\n", sep = "")
  cat("  Automatic strategy selection: FALSE\n")
  invisible(x)
}

#' @export
print.gp3bayes_pupil_validation <- function(x, ...) {
  cat("<gp3bayes_pupil_validation>\n")
  cat("  Target: ", x$target, "\n", sep = "")
  cat("  Strategy: ", x$strategy, "\n", sep = "")
  cat("  Executed: ", x$executed, "\n", sep = "")
  cat("  Predictive validity certified: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_validation <- function(x, ...) {
  pupil_validation_table(x)
}
