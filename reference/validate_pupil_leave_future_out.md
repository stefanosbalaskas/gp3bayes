# Execute or materialize leave-future-out validation

Execute or materialize leave-future-out validation

## Usage

``` r
validate_pupil_leave_future_out(
  fit,
  plan,
  execute = FALSE,
  cores = 1L,
  seed = 2026
)
```

## Arguments

- fit:

  An advanced fitted model.

- plan:

  An LFO plan.

- execute:

  If FALSE, returns the plan without refitting. TRUE performs sequential
  model refits and future-block log scoring.

- cores:

  Maximum cores passed to brms update; restricted to 2.

- seed:

  Base seed for refits.

## Value

A `gp3bayes_pupil_lfo_validation` object.
