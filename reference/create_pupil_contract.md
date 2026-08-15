# Create a governed pupil-timecourse contract

Records the measurement and analysis declarations required for the
restricted Gaussian hierarchical pupil-timecourse family. Contract
creation performs no preprocessing, exclusion, correction, model
fitting, or psychological interpretation.

## Usage

``` r
create_pupil_contract(
  outcome_col,
  participant_col,
  trial_col,
  time_col,
  pupil_unit,
  sampling_frequency,
  time_unit = c("seconds", "milliseconds"),
  item_col = NULL,
  condition_col = NULL,
  timestamp_col = NULL,
  eye = c("unknown", "left", "right", "combined"),
  left_pupil_col = NULL,
  right_pupil_col = NULL,
  channel_audit_unit = NULL,
  validity_col = NULL,
  interpolation_col = NULL,
  blink_col = NULL,
  gaze_x_col = NULL,
  gaze_y_col = NULL,
  luminance_col = NULL,
  contrast_col = NULL,
  screen_width = NA_real_,
  screen_height = NA_real_,
  baseline_window = NULL,
  baseline_method = c("unknown", "none", "subtract", "divide", "proportion_change",
    "percent_change"),
  baseline_applied = FALSE,
  pfe_corrected = FALSE,
  pfe_method = NULL,
  source_vendor = NA_character_,
  device_model = NA_character_,
  preprocessing_provenance = NA_character_,
  upstream_package = NA_character_,
  upstream_version = NA_character_,
  notes = character()
)
```

## Arguments

- outcome_col:

  Numeric pupil-response column to model.

- participant_col:

  Participant identifier column.

- trial_col:

  Trial identifier column.

- time_col:

  Event-relative time column.

- pupil_unit:

  One of `"millimetres"`, `"metres"`, `"pixels"`, `"arbitrary_units"`,
  `"standardized"`, `"ratio"`, `"proportion_change"`, or
  `"percent_change"`.

- sampling_frequency:

  Declared nominal sampling frequency in Hz.

- time_unit:

  Unit of `time_col`: `"seconds"` or `"milliseconds"`. Preparation
  converts the canonical event-time column to seconds and records that
  deterministic conversion.

- item_col:

  Optional item/stimulus identifier.

- condition_col:

  Optional experimental condition.

- timestamp_col:

  Optional absolute or recording timestamp.

- eye:

  Declared channel: `"left"`, `"right"`, `"combined"`, or `"unknown"`.
  The function never chooses an eye automatically.

- left_pupil_col, right_pupil_col:

  Optional paired pupil channels retained only for left/right
  disagreement auditing. Either may equal `outcome_col`.

- channel_audit_unit:

  Unit for paired audit channels; defaults to `pupil_unit` when either
  paired channel is declared.

- validity_col, interpolation_col, blink_col:

  Optional measurement-quality indicator columns.

- gaze_x_col, gaze_y_col:

  Optional gaze-position columns.

- luminance_col, contrast_col:

  Optional visual-stimulus nuisance columns.

- screen_width, screen_height:

  Optional screen dimensions in declared screen units; use `NA_real_`
  when unknown.

- baseline_window:

  Optional two-element event-relative baseline window expressed in
  `time_unit`.

- baseline_method:

  Declared upstream/current baseline state: `"none"`, `"subtract"`,
  `"divide"`, `"proportion_change"`, `"percent_change"`, or `"unknown"`.

- baseline_applied:

  Whether baseline correction has already been applied.

- pfe_corrected:

  Whether pupil-foreshortening correction was applied upstream.

- pfe_method:

  Optional description of the upstream PFE method.

- source_vendor, device_model:

  Optional source metadata. Missing metadata remain explicitly unknown.

- preprocessing_provenance:

  Optional free-text provenance.

- upstream_package, upstream_version:

  Optional upstream package metadata.

- notes:

  Optional user notes.

## Value

A `gp3bayes_pupil_contract`.

## Governance boundary

The contract records decisions but does not detect blinks, interpolate,
smooth, correct PFE, correct luminance, choose a baseline, or exclude
data. gp3bayes does not automatically correct blink/data-loss, PFE,
gaze-position, luminance, contrast, or baseline decisions recorded
upstream. gp3bayes does not infer cognitive load, attention, arousal,
stress, emotion, surprise, or effort from a pupil measurement or
posterior pupil contrast. Interpretation remains the researcher's
responsibility and must be justified by the study design, measurement
context, and substantive scientific argument.

## Examples

``` r
contract <- create_pupil_contract(
  outcome_col = "pupil_mm",
  participant_col = "participant_id",
  trial_col = "trial_id",
  time_col = "event_time",
  pupil_unit = "millimetres",
  sampling_frequency = 60,
  condition_col = "condition",
  eye = "combined"
)
contract
#> <gp3bayes_pupil_contract>
#>   Family: pupil
#>   Model family: Restricted Gaussian hierarchical pupil time-course
#>   Outcome: pupil_mm [millimetres]
#>   Participant: participant_id
#>   Trial: trial_id
#>   Event time: event_time
#>   Nominal sampling: 60 Hz
#>   Event-time unit: seconds
#>   Eye/channel: combined
#>   Baseline state: unknown (applied=FALSE)
#>   PFE corrected upstream: FALSE
#>   Fitting performed: FALSE
```
