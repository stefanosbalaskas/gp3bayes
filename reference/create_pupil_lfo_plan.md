# Create an explicit leave-future-out validation plan

The plan defines sequential training cut-points within a single ordered
series. Execution is deliberately separate because it requires
refitting.

## Usage

``` r
create_pupil_lfo_plan(
  fit,
  initial_fraction = 0.6,
  horizon = 5L,
  step = 5L,
  max_refits = 8L
)
```

## Arguments

- fit:

  An advanced fitted model.

- initial_fraction:

  Initial fraction of each series available for the earliest training
  set.

- horizon:

  Number of future samples scored at each refit.

- step:

  Number of samples by which the training cut moves.

- max_refits:

  Maximum refits per model.

## Value

A `gp3bayes_pupil_lfo_plan` object.
