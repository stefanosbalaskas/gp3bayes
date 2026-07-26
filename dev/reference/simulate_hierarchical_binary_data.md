# Simulate Hierarchical Binary Data

Generates deterministic synthetic repeated-measures data from the
approved Bernoulli-logit contract. The simulator records all generating
parameters, participant effects, optional crossed item effects, and the
random-number seed. It performs no model fitting.

## Usage

``` r
simulate_hierarchical_binary_data(
  n_participants = 40,
  trials_per_participant = 20,
  n_items = 20,
  intercept = stats::qlogis(0.35),
  condition_effect = 0.8,
  participant_covariate_effect = 0.3,
  trial_covariate_effect = 0.15,
  interaction_effect = 0.25,
  participant_sd = 0.7,
  item_sd = 0.35,
  random_slope_sd = 0.3,
  random_slope_cor = 0,
  condition_probability = 0.5,
  balanced_condition = TRUE,
  include_items = TRUE,
  seed = 1
)
```

## Arguments

- n_participants:

  Number of participants.

- trials_per_participant:

  Number of observations per participant.

- n_items:

  Number of crossed items when `include_items = TRUE`.

- intercept:

  Population intercept on the log-odds scale.

- condition_effect:

  Population condition contrast on the log-odds scale.

- participant_covariate_effect:

  Participant-covariate coefficient.

- trial_covariate_effect:

  Trial-covariate coefficient.

- interaction_effect:

  Condition-by-participant-covariate coefficient.

- participant_sd:

  Participant random-intercept standard deviation.

- item_sd:

  Crossed item random-intercept standard deviation.

- random_slope_sd:

  Participant condition-slope standard deviation.

- random_slope_cor:

  Correlation between participant intercepts and condition slopes. It
  must lie strictly between -1 and 1.

- condition_probability:

  Treatment probability when `balanced_condition = FALSE`.

- balanced_condition:

  Whether each participant receives an approximately balanced condition
  sequence.

- include_items:

  Whether to generate a crossed item identifier.

- seed:

  Non-negative integer random-number seed.

## Value

A `gp3bayes_binary_simulation` containing synthetic data, stored truth,
generated random effects, and design metadata.

## Details

The condition is generated using `-0.5` and `0.5` internally and
returned as a factor with levels `control` and `treatment`. The
data-generating model includes one participant random intercept, one
optional correlated participant condition slope, and one optional
crossed item intercept.

## Examples

``` r
simulation <- simulate_hierarchical_binary_data(
  n_participants = 12,
  trials_per_participant = 8,
  n_items = 6,
  seed = 2026
)

simulation
#> <gp3bayes_binary_simulation>
#>   Rows: 96
#>   Participants: 12
#>   Items: 6
#>   True condition effect: 0.8
#>   Seed: 2026
head(simulation$data)
#>   participant_id item_id trial_id condition participant_covariate
#> 1           p001    i001        1   control            -0.8401472
#> 2           p001    i002        2   control            -0.8401472
#> 3           p001    i003        3   control            -0.8401472
#> 4           p001    i004        4 treatment            -0.8401472
#> 5           p001    i005        5   control            -0.8401472
#> 6           p001    i006        6 treatment            -0.8401472
#>   trial_covariate selected true_probability
#> 1     -1.05852268        0        0.2321841
#> 2      0.04983863        0        0.1982548
#> 3      0.40720658        1        0.2881628
#> 4     -0.08758285        0        0.2151161
#> 5      1.20370874        0        0.4440256
#> 6     -0.90809542        0        0.2400411
```
