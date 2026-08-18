# Simulate data from the experimental nonlinear response-shape family

Simulate data from the experimental nonlinear response-shape family

## Usage

``` r
simulate_pupil_response_shape(
  n_participants = 20L,
  trials_per_participant = 6L,
  time_points = 41L,
  conditions = c("control", "treatment"),
  baseline = 3.2,
  amplitude = 0.7,
  onset = 250,
  rise = 180,
  duration = 1200,
  decay = 260,
  condition_amplitude_ratio = 1.2,
  condition_onset_shift = 80,
  residual_sd = 0.08,
  seed = 2026
)
```

## Arguments

- n_participants, trials_per_participant, time_points:

  Simulation sizes.

- conditions:

  Condition labels.

- baseline, amplitude, onset, rise, duration, decay:

  Shape parameters.

- condition_amplitude_ratio:

  Multiplicative amplitude ratio for condition 2.

- condition_onset_shift:

  Onset shift for condition 2.

- residual_sd:

  Residual SD.

- seed:

  Seed.
