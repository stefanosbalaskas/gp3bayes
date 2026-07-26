# Create a Contrast-Coding Sensitivity Specification

Replays the prepared data back to the recorded raw scale, applies an
alternative two-level condition coding, and rebuilds the approved
specification. Because the intercept meaning changes with coding, an
explicit new `baseline` is required.

## Usage

``` r
create_contrast_coding_sensitivity_specification(
  specification,
  condition_coding,
  baseline
)
```

## Arguments

- specification:

  An approved model specification.

- condition_coding:

  Two distinct numeric condition codes.

- baseline:

  Explicit baseline probability or median under the new coding.

## Value

An approved alternative specification.
