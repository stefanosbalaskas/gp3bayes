\name{audit_mediation_functional_form}
\alias{audit_mediation_functional_form}
\title{Participant-aware mediation functional-form audit}
\usage{
audit_mediation_functional_form(
  data, treatment, mediator, outcome, participant = "participant_id",
  resamples = 199L, seed = 2026L
)
}
\arguments{
\item{data}{Trial-level data frame with participant identity.}
\item{treatment}{Binary 0/1 treatment column name.}
\item{mediator}{Continuous numeric mediator column name.}
\item{outcome}{Continuous numeric outcome column name.}
\item{participant}{Participant identity column name.}
\item{resamples}{Participant-cluster bootstrap replicates, at least 20.}
\item{seed}{Deterministic integer seed; prior RNG state restored.}
}
\description{
Compares linear versus quadratic treatment-by-mediator outcome models
with refitted participant-cluster bootstraps. Reports model-based
mediator shifts, descriptive percentile intervals and failed resamples.
This is not a Bayesian posterior, identified natural indirect effect,
causal graph discovery or a validated GAM method.
}
\value{List containing \code{estimates}, \code{bootstrap_ledger},
\code{n_participants}, \code{estimand}, and \code{claim_boundary}.}
