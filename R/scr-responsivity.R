#' Bayesian retention-first SCR responsivity summary
#'
#' Estimates participant-level response probabilities with a conjugate
#' Beta-Binomial model while preserving the conventional amplitude-threshold
#' non-responder flag. Low-reactive participants are retained so hard exclusion
#' can be evaluated as a sensitivity specification rather than preprocessing.
#'
#' @param participant Participant identifiers.
#' @param amplitude Trial-level SCR amplitudes.
#' @param threshold Conventional response threshold in microsiemens.
#' @param prior_alpha,prior_beta Positive Beta prior parameters.
#' @param probability_cutoff Optional posterior-probability cutoff used only for
#'   descriptive classification.
#' @return A data frame with posterior responsivity parameters and flags.
#' @export
estimate_scr_responsivity_bayes <- function(participant, amplitude, threshold = 0.02,
                                            prior_alpha = 1, prior_beta = 1,
                                            probability_cutoff = 0.5) {
  if (length(participant) != length(amplitude)) stop("Inputs must have equal length.", call. = FALSE)
  if (threshold < 0 || prior_alpha <= 0 || prior_beta <= 0) stop("Invalid threshold or prior.", call. = FALSE)
  dat <- data.frame(participant = participant,
                    amplitude = suppressWarnings(as.numeric(amplitude)),
                    stringsAsFactors = FALSE)
  dat <- dat[is.finite(dat$amplitude), , drop = FALSE]
  groups <- split(dat$amplitude, dat$participant, drop = TRUE)
  rows <- lapply(names(groups), function(id) {
    x <- pmax(0, groups[[id]]); n <- length(x); k <- sum(x >= threshold)
    a <- prior_alpha + k; b <- prior_beta + n - k
    p <- a / (a + b)
    data.frame(participant = id, n_trials = n, responses = k,
      response_rate = k / n, posterior_alpha = a, posterior_beta = b,
      posterior_response_probability = p,
      posterior_responder = p >= probability_cutoff,
      conventional_nonresponder = stats::median(x) < threshold,
      retain_for_modeling = TRUE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  attr(out, "interpretation") <- paste(
    "Posterior responsivity is a graded modelling quantity.",
    "The conventional non-responder flag is retained for provenance and sensitivity analysis, not automatic deletion."
  )
  out
}
