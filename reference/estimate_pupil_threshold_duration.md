# Estimate posterior duration above a prespecified dynamic threshold

Duration is computed draw-by-draw from the dynamic contrast on its
existing time grid. The threshold and direction must be supplied before
interpretation; the function performs no threshold or window
optimization.

## Usage

``` r
estimate_pupil_threshold_duration(
  contrast,
  direction = c("above", "below", "absolute"),
  threshold = contrast$threshold,
  probability = 0.95
)
```

## Arguments

- contrast:

  A dynamic-contrast object.

- direction:

  `"above"`, `"below"`, or `"absolute"`.

- threshold:

  Optional threshold overriding the contrast's stored threshold.

- probability:

  Central interval probability.

## Value

A `gp3bayes_pupil_threshold_duration` object.
