# Create an explicit pupil predictive-validation plan

Defines the prediction target before choosing a partition. The plan
distinguishes observation-level known-trial prediction, new trials for
known participants, new participants, and finite future time segments.
Only non-missing model-outcome rows enter validation partitions.

## Usage

``` r
create_pupil_validation_plan(
  x,
  target = c("new_trial_known_participant", "new_participant", "future_segment",
    "new_sample_known_trial"),
  K = 5L,
  future_fraction = 0.2,
  seed = 2026
)
```

## Arguments

- x:

  A prepared pupil object, pupil specification, or pupil fit.

- target:

  One of `"new_sample_known_trial"`, `"new_trial_known_participant"`,
  `"new_participant"`, or `"future_segment"`.

- K:

  Number of folds for K-fold targets.

- future_fraction:

  Fraction at the end of each trial series held out for the
  future-segment target.

- seed:

  Reproducibility seed.

## Value

A `gp3bayes_pupil_validation_plan` with explicit fold/split membership
and leakage checks.

## Interpretation

Observation-wise validation is not presented as a universal default for
temporally dependent pupil samples. The declared prediction target
determines the partition.

## Examples

``` r
# See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
```
