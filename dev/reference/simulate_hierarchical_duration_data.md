# Simulate Hierarchical Lognormal Duration Data

Generates deterministic strictly positive uncensored durations from the
approved hierarchical lognormal contract.

## Usage

``` r
simulate_hierarchical_duration_data(
  n_participants = 40L,
  trials_per_participant = 20L,
  n_items = 20L,
  baseline_median = 500,
  condition_effect = log(1.15),
  participant_covariate_effect = log(1.08),
  trial_covariate_effect = log(1.04),
  interaction_effect = log(1.05),
  participant_sd = 0.35,
  item_sd = 0.2,
  random_slope_sd = 0.15,
  random_slope_cor = 0,
  residual_sd = 0.4,
  condition_probability = 0.5,
  balanced_condition = TRUE,
  include_items = TRUE,
  outcome_unit = "milliseconds",
  seed = 1L
)
```

## Arguments

- n_participants:

  Number of participants.

- trials_per_participant:

  Observations per participant.

- n_items:

  Number of crossed items when `include_items = TRUE`.

- baseline_median:

  Baseline duration median in `outcome_unit`.

- condition_effect:

  Population condition contrast on the log-duration scale.

- participant_covariate_effect:

  Participant-covariate coefficient on the log-duration scale.

- trial_covariate_effect:

  Trial-covariate coefficient on the log-duration scale.

- interaction_effect:

  Condition-by-participant-covariate coefficient on the log-duration
  scale.

- participant_sd:

  Participant random-intercept standard deviation on the log scale.

- item_sd:

  Crossed item random-intercept standard deviation.

- random_slope_sd:

  Participant condition-slope standard deviation.

- random_slope_cor:

  Correlation between participant intercepts and condition slopes.

- residual_sd:

  Lognormal residual standard deviation.

- condition_probability:

  Focal-condition probability for an unbalanced design.

- balanced_condition:

  Whether each participant receives an approximately balanced condition
  sequence.

- include_items:

  Whether crossed items are generated.

- outcome_unit:

  Recorded duration unit.

- seed:

  Non-negative integer random-number seed.

## Value

A `gp3bayes_duration_simulation` containing synthetic data, stored
truth, random effects, and design metadata.

## Details

The generated outcome is strictly positive, finite, and uncensored. The
function does not generate zero, censored, truncated, shifted, or
survival outcomes.
