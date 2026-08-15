# Run pupil-specific posterior predictive checks

Compares observed and replicated trajectories, distributional features,
whole-support peak/latency and AUC, optional declared-window response,
lag-one serial structure, participant/trial heterogeneity, residual
trajectories, and blink/interpolation context. The object reports
evidence and never declares a model adequate.

## Usage

``` r
check_pupil_posterior_predictive(
  fit,
  ndraws = 200L,
  probability = 0.9,
  window = NULL,
  max_cells = 3000000L
)
```

## Arguments

- fit:

  A fitted pupil model.

- ndraws:

  Posterior predictive draws.

- probability:

  Predictive envelope probability.

- window:

  Optional user-declared event-time window in canonical seconds.

- max_cells:

  Maximum draw-by-observation cells.

## Value

A `gp3bayes_pupil_ppc`.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```
