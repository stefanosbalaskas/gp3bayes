# Preparing and auditing pupil time courses

## Preparation is not preprocessing automation

[`prepare_pupil_timecourse()`](https://stefanosbalaskas.github.io/gp3bayes/reference/prepare_pupil_timecourse.md)
validates declared columns and records only transformations explicitly
requested by the analyst. It does not detect or repair blinks,
interpolate missing values, smooth traces, select an eye, or choose a
baseline window.

``` r

sim <- simulate_pupil_timecourse(
  n_participants = 5,
  trials_per_participant = 4,
  sampling_frequency = 20,
  time_window = c(-0.5, 1.5),
  seed = 14
)
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
  blink_col = "blink",
  interpolation_col = "interpolated",
  gaze_x_col = "gaze_x",
  gaze_y_col = "gaze_y",
  luminance_col = "luminance",
  baseline_window = c(-0.5, 0),
  baseline_method = "none",
  preprocessing_provenance = "deterministic synthetic example"
)
prepared <- prepare_pupil_timecourse(sim$data, contract)
prepared
#> <gp3bayes_pupil_prepared>
#>   Rows: 820
#>   Participants: 5
#>   Trials: 20
#>   Model unit: millimetres
#>   Baseline operation: none
#>   Sampling interval CV: 1.389e-15
#>   Readiness status: review
```

## Explicit baseline transformation

A baseline operation is performed only when requested, and the declared
baseline window must be available. Data already declared as
baseline-adjusted cannot be baseline-adjusted a second time.

``` r

baseline_prepared <- prepare_pupil_timecourse(
  sim$data,
  contract,
  baseline_operation = "subtract",
  baseline_window = c(-0.5, 0)
)
head(baseline_prepared$transformation_log)
#> NULL
```

The raw declared pupil value remains linked to the model value in the
prepared object.

## Readiness evidence

``` r

readiness <- audit_pupil_readiness(prepared)
pupil_readiness_table(readiness, "summary")
#>                                   metric       value status
#> 1                                   rows         820   pass
#> 2                           participants           5   pass
#> 3                                 trials          20   pass
#> 4                                  items          12   pass
#> 5                             conditions           2   pass
#> 6                  estimated_sampling_hz          20   pass
#> 7               median_sampling_interval        0.05   pass
#> 8                   sampling_interval_cv 1.38865e-15   pass
#> 9               missing_pupil_proportion  0.00731707   pass
#> 10                      blink_proportion  0.00731707   pass
#> 11               interpolated_proportion           0   pass
#> 12                    invalid_proportion        <NA> review
#> 13                     baseline_coverage           1   pass
#> 14               trials_lacking_baseline           0   pass
#> 15                             pupil_min     3.61634   pass
#> 16                             pupil_max     5.47178   pass
#> 17               minimum_trial_time_span           2 review
#> 18               maximum_trial_time_span           2 review
#> 19                        gaze_available        TRUE   pass
#> 20              gaze_condition_imbalance  0.00402644 review
#> 21         left_right_pupil_disagreement        <NA> review
#> 22                   luminance_available        TRUE   pass
#> 23         luminance_condition_imbalance 2.05469e-05 review
#> 24                    contrast_available       FALSE review
#> 25                pfe_corrected_upstream       FALSE review
#> 26 preprocessing_provenance_completeness       0.333 review
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
head(pupil_readiness_table(readiness, "participant"))
#>   participant rows trials missing_pupil_proportion blink_proportion
#> 1        P001  164      4               0.00000000       0.00000000
#> 2        P002  164      4               0.00000000       0.00000000
#> 3        P003  164      4               0.01829268       0.01829268
#> 4        P004  164      4               0.00000000       0.00000000
#> 5        P005  164      4               0.01829268       0.01829268
#>   interpolated_proportion
#> 1                       0
#> 2                       0
#> 3                       0
#> 4                       0
#> 5                       0
head(pupil_readiness_table(readiness, "trial"))
#>        series_id participant     trial time_start time_end time_span rows
#> 1 P001.P001_T001        P001 P001_T001       -0.5      1.5         2   41
#> 2 P001.P001_T002        P001 P001_T002       -0.5      1.5         2   41
#> 3 P001.P001_T003        P001 P001_T003       -0.5      1.5         2   41
#> 4 P001.P001_T004        P001 P001_T004       -0.5      1.5         2   41
#> 5 P002.P002_T001        P002 P002_T001       -0.5      1.5         2   41
#> 6 P002.P002_T002        P002 P002_T002       -0.5      1.5         2   41
#>   nonmissing_pupil
#> 1               41
#> 2               41
#> 3               41
#> 4               41
#> 5               41
#> 6               41
```

The audit reports sample support, sampling intervals, missingness,
baseline coverage, indicators, gaze/luminance availability, and related
measurement context. It does not remove observations.

## Measurement-context audit

``` r

measurement <- audit_pupil_measurement_context(prepared)
pupil_measurement_audit_table(measurement)
#>          domain available   observed
#> 1         blink      TRUE  0.0073171
#> 2 interpolation      TRUE          0
#> 3      baseline      TRUE  -0.5 to 0
#> 4      sampling      TRUE 1.3886e-15
#> 5 gaze_position      TRUE  0.0080523
#> 6           pfe      TRUE      FALSE
#> 7     luminance      TRUE 2.0547e-05
#> 8      contrast     FALSE       <NA>
#> 9  time_on_task     FALSE       <NA>
#>                                                       interpretation
#> 1                                   Reported data-loss context only.
#> 2                      Reported upstream interpolation context only.
#> 3                         Baseline declaration; no automatic choice.
#> 4              Sampling irregularity context for temporal modelling.
#> 5 Between-condition mean gaze-position difference; descriptive only.
#> 6  Upstream PFE-correction declaration; no automatic PFE correction.
#> 7     Between-condition mean luminance difference; descriptive only.
#> 8              Contrast availability/range; no automatic adjustment.
#> 9                             Recording-time support when available.
```

PFE status is carried from the contract. Gaze coordinates are evidence
about measurement context and can be declared as nuisance covariates or
used in sensitivity scenarios, but this foundation does not implement a
universal PFE correction.
