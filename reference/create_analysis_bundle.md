# Create a Structured Post-Fit Analysis Bundle

Collects reusable posterior, diagnostic, prediction, calibration,
scoring, and optionally PSIS-LOO tables without making an automatic
adequacy or model selection decision.

## Usage

``` r
create_analysis_bundle(
  fit,
  newdata = NULL,
  ndraws = 1000L,
  include_group_effects = FALSE,
  include_loo = FALSE
)
```

## Arguments

- fit:

  A fitted `gp3bayes_fit`.

- newdata:

  Optional prediction data.

- ndraws:

  Posterior draws used for prediction-facing components.

- include_group_effects:

  Whether prediction summaries include recorded group-level effects.

- include_loo:

  Whether PSIS-LOO is computed.

## Value

A `gp3bayes_analysis_bundle`.
