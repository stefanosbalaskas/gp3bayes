# Bayesian dynamic pupillometry: governed foundation

## Scope

Development version 0.4.0.9000 adds a restricted Gaussian hierarchical
pupil-timecourse family. The family is designed for pupil series that
have already been produced by a documented preprocessing workflow. It
does not detect blinks, interpolate samples, correct pupil
foreshortening error (PFE), or infer psychological constructs from pupil
change.

The workflow is deliberately staged:

1.  declare a pupil contract;
2.  prepare data only through explicit deterministic transformations;
3.  inspect readiness and measurement-context audits;
4.  create a closed model specification;
5.  fit through an approved `rstan` or `cmdstanr` backend;
6.  estimate declared pupil trajectories and window estimands;
7.  inspect posterior predictive and temporal diagnostics;
8.  validate against an explicit prediction target; and
9.  compare prespecified sensitivity scenarios.

## Deterministic synthetic example

``` r

sim <- simulate_pupil_timecourse(
  n_participants = 6,
  trials_per_participant = 4,
  sampling_frequency = 20,
  time_window = c(-0.5, 1.5),
  seed = 20260814
)
str(sim$truth, max.level = 1)
#> List of 17
#>  $ seed                       : int 20260814
#>  $ waveform                   : chr "normalized gamma-shaped synthetic response"
#>  $ sampling_frequency         : num 20
#>  $ time_window                : num [1:2] -0.5 1.5
#>  $ baseline_window            : num [1:2] -0.5 0
#>  $ baseline_pupil_mm          : num 4
#>  $ response_amplitude_mm      : num 0.45
#>  $ condition_difference_mm    : num 0.18
#>  $ peak_latency_s             : num 0.9
#>  $ participant_sd_mm          : num 0.25
#>  $ item_sd_mm                 : num 0.08
#>  $ residual_innovation_sd_mm  : num 0.08
#>  $ ar1                        : num 0.55
#>  $ blink_trial_probability    : num 0.15
#>  $ luminance_nuisance_included: logi TRUE
#>  $ gaze_drift_included        : logi TRUE
#>  $ psychological_construct    : chr NA
```

The simulator uses one convenient smooth response waveform for software
testing. It is not a claim that all biological pupil responses have this
shape.

``` r

contract <- create_pupil_contract(
  outcome_col = "pupil_mm",
  participant_col = "participant_id",
  trial_col = "trial_id",
  item_col = "item_id",
  condition_col = "condition",
  time_col = "event_time",
  pupil_unit = "millimetres",
  sampling_frequency = 20,
  eye = "combined",
  source_vendor = "synthetic",
  blink_col = "blink",
  interpolation_col = "interpolated",
  gaze_x_col = "gaze_x",
  gaze_y_col = "gaze_y",
  luminance_col = "luminance",
  baseline_window = c(-0.5, 0),
  baseline_method = "none"
)
prepared <- prepare_pupil_timecourse(sim$data, contract)
audit <- audit_pupil_readiness(prepared)
pupil_readiness_table(audit)
#>                                   metric       value status
#> 1                                   rows         984   pass
#> 2                           participants           6   pass
#> 3                                 trials          24   pass
#> 4                                  items          12   pass
#> 5                             conditions           2   pass
#> 6                  estimated_sampling_hz          20   pass
#> 7               median_sampling_interval        0.05   pass
#> 8                   sampling_interval_cv  1.3885e-15   pass
#> 9               missing_pupil_proportion  0.00914634   pass
#> 10                      blink_proportion  0.00914634   pass
#> 11               interpolated_proportion           0   pass
#> 12                    invalid_proportion        <NA> review
#> 13                     baseline_coverage           1   pass
#> 14               trials_lacking_baseline           0   pass
#> 15                             pupil_min     3.29346   pass
#> 16                             pupil_max     5.11051   pass
#> 17               minimum_trial_time_span           2 review
#> 18               maximum_trial_time_span           2 review
#> 19                        gaze_available        TRUE   pass
#> 20              gaze_condition_imbalance  0.00274931 review
#> 21         left_right_pupil_disagreement        <NA> review
#> 22                   luminance_available        TRUE   pass
#> 23         luminance_condition_imbalance 5.16134e-05 review
#> 24                    contrast_available       FALSE review
#> 25                pfe_corrected_upstream       FALSE review
#> 26 preprocessing_provenance_completeness           0 review
#>                                                                       detail
#> 1                                                                           
#> 2  One-participant data cannot support population participant heterogeneity.
#> 3                                                                           
#> 4                                                Item hierarchy is optional.
#> 5                         A condition contrast requires at least two levels.
#> 6                                                            Declared 20 Hz.
#> 7                                                                           
#> 8            AR(1) sample-order dependence requires regular-enough sampling.
#> 9           Missingness is reported, not automatically repaired or excluded.
#> 10                                                                          
#> 11                                                                          
#> 12                                                                          
#> 13                                                                          
#> 14                                                                          
#> 15                                                                          
#> 16                                                                          
#> 17                       Smallest observed within-trial event-time coverage.
#> 18                        Largest observed within-trial event-time coverage.
#> 19                                                                          
#> 20                   Descriptive between-condition gaze-position difference.
#> 21                                Median absolute paired-channel difference.
#> 22                                                                          
#> 23                  Descriptive between-condition mean luminance difference.
#> 24                                                                          
#> 25           PFE status is contextual evidence, not an automatic correction.
#> 26
```

Readiness output is evidence about the observed series. A warning is not
an automatic exclusion rule, and a pass is not evidence that the
measurement or scientific interpretation is valid.

## Restricted specification

``` r

spec <- specify_pupil_timecourse_model(
  prepared,
  temporal_structure = "smooth",
  condition_trajectory = TRUE,
  smooth_basis_dimension = 6,
  autocorrelation = "ar1",
  participant_trajectory = "none",
  item_effects = TRUE
)
pupil_specification_table(spec)
#>                     field
#> 1                  family
#> 2              likelihood
#> 3                    link
#> 4                 formula
#> 5      temporal_structure
#> 6  smooth_basis_dimension
#> 7    condition_trajectory
#> 8         autocorrelation
#> 9     participant_effects
#> 10 participant_trajectory
#> 11           item_effects
#> 12             covariates
#> 13           outcome_unit
#> 14     baseline_operation
#> 15   unrestricted_formula
#>                                                                                                                                                                 value
#> 1                                                                                                                                                               pupil
#> 2                                                                                                                                                            Gaussian
#> 3                                                                                                                                                            identity
#> 4  .pupil_model ~ .condition + s(.event_time, by = .condition, k = 6) +      (1 | .participant) + (1 | .item) + ar(time = .sample_index,      gr = .series_id, p = 1)
#> 5                                                                                                                                                              smooth
#> 6                                                                                                                                                                   6
#> 7                                                                                                                                                                TRUE
#> 8                                                                                                                                                                 ar1
#> 9                                                                                                                                                    random_intercept
#> 10                                                                                                                                                               none
#> 11                                                                                                                                                               TRUE
#> 12                                                                                                                                                                   
#> 13                                                                                                                                                        millimetres
#> 14                                                                                                                                                               none
#> 15                                                                                                                                                              FALSE
```

The specification constructs the approved formula internally. There is
no user-facing arbitrary formula or arbitrary family argument.

## Interpretation boundary

Reported quantities are pupil diameter, pupil change, trajectories, and
posterior contrasts in those measurements. They are not automatically
cognitive load, attention, arousal, stress, emotion, surprise, or
effort. Those interpretations require a separate scientific argument and
appropriate experimental design.
