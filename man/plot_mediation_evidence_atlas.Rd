\name{plot_mediation_evidence_atlas}
\alias{plot_mediation_evidence_atlas}
\title{Plot a mediation evidence atlas}
\usage{
plot_mediation_evidence_atlas(
  data = NULL, type = c("effects", "bootstrap", "dag"),
  theory_edges = NULL, discovered_edges = NULL
)
}
\arguments{
\item{data}{Data frame with \code{term, estimate, lower, upper} for effects or \code{term, draw} for bootstrap.}
\item{type}{One of \code{effects}, \code{bootstrap}, or \code{dag}.}
\item{theory_edges}{Data frame with \code{from} and \code{to} for hypothesized edges.}
\item{discovered_edges}{Data frame with \code{from} and \code{to} for empirically proposed edges.}
}
\description{
Produces ggplot figures for user-supplied effect estimates, resampled
draws or a theory-versus-discovered adjacency audit. Requires optional
\pkg{ggplot2}. Does not fit a mediator model, generate draws, infer a
Bayesian posterior, or establish causal graph identification.
}
\value{A ggplot object. DAG plots include an \code{edge_audit} attribute.}
\examples{
\donttest{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  x <- data.frame(term = "indirect", estimate = 0.2,
                  lower = 0.1, upper = 0.3)
  plot_mediation_evidence_atlas(x)
}
}
}
