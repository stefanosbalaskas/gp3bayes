# Synthetic Gazepoint pupillometry case study

## Purpose

This case study is **entirely synthetic**. Its statistics are software
demonstrations and are not empirical evidence about Gazepoint hardware,
participants, or psychological processes.

``` r

sim <- simulate_pupil_timecourse(
  n_participants = 8,
  trials_per_participant = 6,
  sampling_frequency = 20,
  time_window = c(-0.5, 1.5),
  condition_difference = 0.18,
  blink_trial_probability = 0.04,
  include_gaze = TRUE,
  include_luminance = TRUE,
  seed = 20260814
)
```

## A Gazepoint-like mapping audit

The raw simulator is vendor-neutral. The next object creates a small
Gazepoint-like view solely to exercise the verified field bridge.

``` r

gp_like <- data.frame(
  TIME = sim$data$event_time[1:20],
  LPD = 15 + sim$data$pupil_mm[1:20],
  LPV = as.integer(!is.na(sim$data$pupil_mm[1:20])),
  BPOGX = sim$data$gaze_x[1:20],
  BPOGY = sim$data$gaze_y[1:20],
  BPOGV = 1
)
gazepoint_pupil_mapping_table(inspect_gazepoint_pupil_schema(gp_like))
#>        field                        role              unit      eye
#> 1       TIME                        time           seconds     none
#> 2  TIME_TICK                   time_tick             ticks     none
#> 3      LPOGX                 left_gaze_x normalized_screen     left
#> 4      LPOGY                 left_gaze_y normalized_screen     left
#> 5      LPOGV             left_gaze_valid normalized_screen     left
#> 6      RPOGX                right_gaze_x normalized_screen    right
#> 7      RPOGY                right_gaze_y normalized_screen    right
#> 8      RPOGV            right_gaze_valid normalized_screen    right
#> 9      BPOGX                 best_gaze_x normalized_screen combined
#> 10     BPOGY                 best_gaze_y normalized_screen combined
#> 11     BPOGV             best_gaze_valid normalized_screen combined
#> 12      LPCX         left_pupil_camera_x     camera_pixels     left
#> 13      LPCY         left_pupil_camera_y     camera_pixels     left
#> 14       LPD  left_pupil_diameter_pixels            pixels     left
#> 15       LPS            left_pupil_scale             scale     left
#> 16       LPV            left_pupil_valid         indicator     left
#> 17      RPCX        right_pupil_camera_x     camera_pixels    right
#> 18      RPCY        right_pupil_camera_y     camera_pixels    right
#> 19       RPD right_pupil_diameter_pixels            pixels    right
#> 20       RPS           right_pupil_scale             scale    right
#> 21       RPV           right_pupil_valid         indicator    right
#> 22     LEYEX                  left_eye_x            metres     left
#> 23     LEYEY                  left_eye_y            metres     left
#> 24     LEYEZ                  left_eye_z            metres     left
#> 25   LPUPILD  left_pupil_diameter_metres            metres     left
#> 26   LPUPILV         left_pupil_3d_valid         indicator     left
#> 27     REYEX                 right_eye_x            metres    right
#> 28     REYEY                 right_eye_y            metres    right
#> 29     REYEZ                 right_eye_z            metres    right
#> 30   RPUPILD right_pupil_diameter_metres            metres    right
#> 31   RPUPILV        right_pupil_3d_valid         indicator    right
#>                                  source_specification present
#> 1  Gazepoint Open Gaze API v2-era field specification    TRUE
#> 2  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 3  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 4  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 5  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 6  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 7  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 8  Gazepoint Open Gaze API v2-era field specification   FALSE
#> 9  Gazepoint Open Gaze API v2-era field specification    TRUE
#> 10 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 11 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 12 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 13 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 14 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 15 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 16 Gazepoint Open Gaze API v2-era field specification    TRUE
#> 17 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 18 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 19 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 20 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 21 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 22 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 23 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 24 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 25 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 26 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 27 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 28 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 29 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 30 Gazepoint Open Gaze API v2-era field specification   FALSE
#> 31 Gazepoint Open Gaze API v2-era field specification   FALSE
```

`LPD` is labelled as pixels by the bridge. The example does not convert
those synthetic values into millimetres.

## Contract through specification

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
  blink_col = "blink",
  interpolation_col = "interpolated",
  gaze_x_col = "gaze_x",
  gaze_y_col = "gaze_y",
  luminance_col = "luminance",
  baseline_window = c(-0.5, 0),
  preprocessing_provenance = "gp3bayes deterministic simulator"
)
prepared <- prepare_pupil_timecourse(sim$data, contract)
readiness <- audit_pupil_readiness(prepared)
measurement <- audit_pupil_measurement_context(prepared)
spec <- specify_pupil_timecourse_model(
  prepared,
  smooth_basis_dimension = 6,
  autocorrelation = "ar1"
)
pupil_readiness_table(readiness)
#>                                   metric       value status
#> 1                                   rows        1968   pass
#> 2                           participants           8   pass
#> 3                                 trials          48   pass
#> 4                                  items          12   pass
#> 5                             conditions           2   pass
#> 6                  estimated_sampling_hz          20   pass
#> 7               median_sampling_interval        0.05   pass
#> 8                   sampling_interval_cv 1.38814e-15   pass
#> 9               missing_pupil_proportion           0   pass
#> 10                      blink_proportion           0   pass
#> 11               interpolated_proportion           0   pass
#> 12                    invalid_proportion        <NA> review
#> 13                     baseline_coverage           1   pass
#> 14               trials_lacking_baseline           0   pass
#> 15                             pupil_min     3.18649   pass
#> 16                             pupil_max     5.30599   pass
#> 17               minimum_trial_time_span           2 review
#> 18               maximum_trial_time_span           2 review
#> 19                        gaze_available        TRUE   pass
#> 20              gaze_condition_imbalance  0.00178244 review
#> 21         left_right_pupil_disagreement        <NA> review
#> 22                   luminance_available        TRUE   pass
#> 23         luminance_condition_imbalance 3.24082e-05 review
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
pupil_measurement_audit_table(measurement)
#>          domain available   observed
#> 1         blink      TRUE          0
#> 2 interpolation      TRUE          0
#> 3      baseline      TRUE  -0.5 to 0
#> 4      sampling      TRUE 1.3881e-15
#> 5 gaze_position      TRUE  0.0031145
#> 6           pfe      TRUE      FALSE
#> 7     luminance      TRUE 3.2408e-05
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

## Backend and post-fit stages

``` r

fit <- fit_pupil_model_backend(spec, backend = "cmdstanr", cores = 2)
trajectory <- estimate_pupil_trajectory(
  predict_pupil_trajectory(fit, ndraws = 200)
)
window <- estimate_pupil_window(
  predict_pupil_trajectory(fit, ndraws = 200),
  window = c(0.3, 1.0)
)
auc <- estimate_pupil_auc(
  predict_pupil_trajectory(fit, ndraws = 200),
  window = c(0.3, 1.0)
)
ppc <- check_pupil_posterior_predictive(fit, ndraws = 200)
diag <- diagnose_pupil_fit(fit)

plan <- create_pupil_validation_plan(
  prepared,
  target = "new_trial_known_participant",
  K = 4
)
validation <- validate_pupil_model(fit, plan, execute = TRUE)
```

## Sensitivity

``` r

spec_for_sensitivity <- specify_pupil_timecourse_model(
  prepared,
  autocorrelation = "none",
  smooth_basis_dimension = 5
)
suite <- create_pupil_sensitivity_suite(
  spec_for_sensitivity,
  baseline_windows = list(c(-0.5, -0.1), c(-0.4, -0.1)),
  baseline_window_operation = "subtract",
  interpolation_policy = c("retain", "exclude_flagged"),
  gaze_adjustment = c("none", "declared_covariates"),
  luminance_adjustment = c("none", "declared_covariate"),
  analysis_windows = list(c(0.3, 1.0))
)
head(pupil_sensitivity_table(suite))
#>   scenario_id                 axis               value
#> 1        S001      baseline_window           -0.5,-0.1
#> 2        S002      baseline_window           -0.4,-0.1
#> 3        S003 interpolation_policy              retain
#> 4        S004 interpolation_policy     exclude_flagged
#> 5        S005      gaze_adjustment                none
#> 6        S006      gaze_adjustment declared_covariates
```

All windows and sensitivity dimensions are declared. No simulated
estimate is presented as an empirical effect, and no scenario is
automatically selected.
