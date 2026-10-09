utils::globalVariables(c("term", "draw", "estimate", "lower", "upper", "from", "to", "status"))

# Experimental, BriDGE-inspired evidence figures.
# Standalone plotting from explicitly supplied estimates: does not infer causal
# effects, fit a GAM, discover a DAG, or create Bayesian posterior draws.

#' Plot mediation evidence atlas
#'
#' Visualise effect estimates, externally supplied bootstrap draws or an
#' adjacency comparison of a prespecified versus discovered DAG. This is a
#' plotting layer, not causal discovery or posterior inference. Inspired by
#' the visualization *types* in the BriDGE research package; implemented
#' independently using ggplot2.
#'
#' @param data Data frame containing `term, estimate, lower, upper` for
#'   `effects`; `term, draw` for `bootstrap`. Unused for `dag`.
#' @param type One of `effects`, `bootstrap`, `dag`.
#' @param theory_edges Data frame with `from` and `to` for `dag`.
#' @param discovered_edges Same schema, for descriptive adjacency audit.
#' @return A ggplot object. DAG discrepancies are not proof of causal edges.
#' @export
plot_mediation_evidence_atlas <- function(
    data = NULL, type = c("effects", "bootstrap", "dag"),
    theory_edges = NULL, discovered_edges = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Install the optional ggplot2 package for this evidence atlas.", call. = FALSE)
  }
  type <- match.arg(type)
  if (identical(type, "dag")) {
    valid <- function(x) is.data.frame(x) &&
      all(c("from", "to") %in% names(x)) &&
      !anyNA(x[c("from", "to")]) &&
      all(nzchar(as.character(unlist(x[c("from", "to")]))))
    if (!valid(theory_edges) || !valid(discovered_edges)) {
      stop("Both edge tables require complete 'from' and 'to' columns.", call. = FALSE)
    }
    fmt <- function(x) unique(data.frame(
      from = as.character(x$from), to = as.character(x$to),
      stringsAsFactors = FALSE
    ))
    theoretical <- fmt(theory_edges)
    discovered <- fmt(discovered_edges)
    if (!nrow(theoretical) && !nrow(discovered)) {
      stop("At least one asserted edge is required.", call. = FALSE)
    }
    nodes <- sort(unique(c(
      theoretical$from, theoretical$to, discovered$from, discovered$to
    )))
    grid <- expand.grid(from = nodes, to = nodes, stringsAsFactors = FALSE)
    key <- function(x) paste(x$from, x$to, sep = " -> ")
    grid_key <- key(grid)
    has_t <- grid_key %in% key(theoretical)
    has_d <- grid_key %in% key(discovered)
    grid$status <- ifelse(has_t & has_d, "Both",
                          ifelse(has_t, "Theory only",
                                 ifelse(has_d, "Discovered only", "Neither")))
    grid <- grid[grid$status != "Neither", , drop = FALSE]
    grid$from <- factor(grid$from, levels = nodes)
    grid$to <- factor(grid$to, levels = rev(nodes))
    p <- ggplot2::ggplot(grid, ggplot2::aes(x = from, y = to, fill = status)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::scale_fill_manual(values = c(
        "Both" = "#387C88", "Theory only" = "#E2AA64",
        "Discovered only" = "#9B75A5"
      )) +
      ggplot2::labs(
        title = "Theory and learned graph: edge audit",
        subtitle = "Structural comparison only; discovered edges are not causal proof",
        x = "From node", y = "To node", fill = "Edge evidence"
      ) +
      theme_gp3bayes()
    attr(p, "edge_audit") <- grid
    return(p)
  }
  expected <- if (identical(type, "effects")) {
    c("term", "estimate", "lower", "upper")
  } else c("term", "draw")
  if (!is.data.frame(data) || !nrow(data) || !all(expected %in% names(data))) {
    stop("Nonempty data are required with columns: ",
         paste(expected, collapse = ", "), call. = FALSE)
  }
  d <- data[, expected, drop = FALSE]
  numerical <- setdiff(expected, "term")
  if (!all(vapply(d[numerical], is.numeric, logical(1))) ||
      anyNA(d) || any(!is.finite(as.matrix(d[numerical])))) {
    stop("Evidence values must be finite and complete.", call. = FALSE)
  }
  if (any(!nzchar(as.character(d$term)))) {
    stop("Evidence term labels cannot be blank.", call. = FALSE)
  }
  d$term <- factor(as.character(d$term), levels = unique(as.character(d$term)))
  if (identical(type, "effects")) {
    if (any(d$lower > d$estimate | d$estimate > d$upper)) {
      stop("Each estimate must lie within its stated interval.", call. = FALSE)
    }
    p <- ggplot2::ggplot(d, ggplot2::aes(x = estimate, y = term)) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::geom_segment(ggplot2::aes(
        x = lower, xend = upper, yend = term
      ), linewidth = 0.8) +
      ggplot2::geom_point(size = 2.5) +
      ggplot2::labs(
        title = "Mediation effect estimates",
        subtitle = "Intervals supplied by the analyst; confirm their uncertainty type",
        x = "Effect estimate and supplied interval", y = NULL
      )
  } else {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = draw)) +
      ggplot2::geom_density(fill = "#387C88", alpha = 0.28, linewidth = 0.7) +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
      ggplot2::facet_wrap(~term, scales = "free") +
      ggplot2::labs(
        title = "Resampled mediation effects",
        subtitle = "Input draws are user supplied, not generated by this plot",
        x = "Effect draw", y = "Density"
      )
  }
  p + theme_gp3bayes()
}
