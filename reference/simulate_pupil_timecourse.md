# Simulate deterministic hierarchical pupil time courses

Generates event-aligned synthetic pupil data with participant/item
heterogeneity, a smooth non-universal response waveform, AR(1) residual
dependence, optional blink/data-loss segments, gaze drift, and luminance
nuisance variation.

## Usage

``` r
simulate_pupil_timecourse(
  n_participants = 20L,
  trials_per_participant = 12L,
  n_items = 12L,
  sampling_frequency = 60,
  time_window = c(-0.5, 2.5),
  baseline_window = c(-0.5, 0),
  conditions = c("control", "treatment"),
  baseline_pupil = 4,
  response_amplitude = 0.45,
  condition_difference = 0.18,
  peak_latency = 0.9,
  participant_sd = 0.25,
  item_sd = 0.08,
  residual_sd = 0.08,
  ar1 = 0.55,
  blink_trial_probability = 0.15,
  blink_duration = 0.12,
  include_gaze = TRUE,
  include_luminance = TRUE,
  gaze_drift_sd = 0.002,
  luminance_amplitude = 0.12,
  seed = 2026,
  max_rows = 500000L
)
```

## Arguments

- n_participants:

  Number of participants.

- trials_per_participant:

  Number of trials per participant.

- n_items:

  Number of crossed items; use `NULL` for no item effect.

- sampling_frequency:

  Sampling frequency in Hz.

- time_window:

  Two-element event-relative time window in seconds.

- baseline_window:

  Two-element pre-event baseline window.

- conditions:

  Character vector of condition labels.

- baseline_pupil:

  Baseline pupil level in millimetres.

- response_amplitude:

  Peak amplitude of the common simulated response.

- condition_difference:

  Additional peak amplitude in the second condition. For more than two
  conditions it is multiplied by the zero-based condition index.

- peak_latency:

  Time of the simulated waveform peak in seconds.

- participant_sd, item_sd:

  Standard deviations for simulated hierarchy.

- residual_sd:

  Innovation standard deviation.

- ar1:

  AR(1) residual coefficient with absolute value below one.

- blink_trial_probability:

  Probability that a trial contains one synthetic blink/data-loss
  interval.

- blink_duration:

  Blink interval duration in seconds.

- include_gaze, include_luminance:

  Whether to add nuisance signals.

- gaze_drift_sd:

  Standard deviation of gaze drift increments.

- luminance_amplitude:

  Amplitude of the synthetic luminance nuisance.

- seed:

  Reproducibility seed.

- max_rows:

  Maximum allowed output rows.

## Value

A `gp3bayes_pupil_simulation` containing `data` and separate `truth`.

## Interpretation

The waveform is a convenient synthetic data-generating shape, not a
claim about a universal biological pupil response.

## Examples

``` r
sim <- simulate_pupil_timecourse(
  n_participants = 4, trials_per_participant = 4,
  sampling_frequency = 20, seed = 2026
)
head(sim$data)
#>   participant_id  trial_id item_id condition event_time timestamp pupil_mm
#> 1           P001 P001_T001    I001   control      -0.50      0.00 4.137206
#> 2           P001 P001_T001    I001   control      -0.45      0.05 4.129026
#> 3           P001 P001_T001    I001   control      -0.40      0.10 4.043163
#> 4           P001 P001_T001    I001   control      -0.35      0.15 4.115891
#> 5           P001 P001_T001    I001   control      -0.30      0.20 4.074787
#> 6           P001 P001_T001    I001   control      -0.25      0.25 4.065638
#>   pupil_signal_truth_mm blink interpolated valid    gaze_x    gaze_y luminance
#> 1              4.078160 FALSE        FALSE  TRUE 0.5012802 0.5010844 0.5170846
#> 2              4.079146 FALSE        FALSE  TRUE 0.5023126 0.4972550 0.5294067
#> 3              4.080106 FALSE        FALSE  TRUE 0.4984555 0.4986794 0.5414065
#> 4              4.081030 FALSE        FALSE  TRUE 0.4984826 0.4994534 0.5529527
#> 5              4.081907 FALSE        FALSE  TRUE 0.4960592 0.4996240 0.5639188
#> 6              4.082728 FALSE        FALSE  TRUE 0.4954407 0.4966865 0.5741845
```
