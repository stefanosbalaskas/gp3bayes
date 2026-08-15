# Validate a pupil model for an explicit prediction target

Executes exact target-specific K-fold through
[`brms::kfold()`](https://mc-stan.org/loo/reference/kfold-generic.html)
for K-fold targets, or a finite leave-future-segment refit for the
future target. Execution is opt-in because it can be computationally
expensive.

## Usage

``` r
validate_pupil_model(
  fit,
  plan,
  execute = FALSE,
  ndraws = 200L,
  max_cells = 3000000L
)
```

## Arguments

- fit:

  A fitted pupil model.

- plan:

  A pupil validation plan.

- execute:

  Whether to execute refitting. `FALSE` returns validated partition
  evidence without refitting.

- ndraws:

  Draws retained for finite future-segment prediction scoring.

- max_cells:

  Memory guard for future-segment predictions.

## Value

A `gp3bayes_pupil_validation`.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```
