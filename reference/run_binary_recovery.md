# Run Binary Parameter Recovery

Repeatedly simulates from the approved hierarchical Bernoulli-logit
generator, fits the restricted model, and compares posterior intervals
with known generating values.

## Usage

``` r
run_binary_recovery(
  repetitions = 20L,
  n_participants = 30L,
  trials_per_participant = 16L,
  n_items = 12L,
  include_items = TRUE,
  random_slope = TRUE,
  seed = 1001L,
  chains = 4L,
  iter = 1500L,
  warmup = 750L,
  cores = min(chains, .gp3b_default_cores(chains)),
  adapt_delta = 0.95,
  max_treedepth = 12L,
  refresh = 0L,
  interval_probability = 0.95,
  minimum_repetitions = 20L,
  maximum_standardized_bias = 0.25,
  minimum_coverage = 0.8,
  minimum_diagnostic_pass_fraction = 0.8,
  continue_on_error = TRUE
)
```

## Arguments

- repetitions:

  Number of simulation-fit repetitions.

- n_participants, trials_per_participant, n_items:

  Synthetic design sizes.

- include_items:

  Whether crossed item effects are included.

- random_slope:

  Whether a participant condition slope is generated and fitted.

- seed:

  First simulation seed.

- chains, iter, warmup, cores, adapt_delta, max_treedepth, refresh:

  Restricted sampling controls.

- interval_probability:

  Central posterior interval probability.

- minimum_repetitions:

  Repetitions required before an overall pass is possible.

- maximum_standardized_bias:

  Maximum absolute bias divided by the empirical standard deviation of
  estimates for a pass.

- minimum_coverage:

  Minimum empirical interval coverage for a pass.

- minimum_diagnostic_pass_fraction:

  Minimum fraction of fits with a diagnostic pass.

- continue_on_error:

  Whether failed repetitions are recorded instead of stopping.

## Value

A `gp3bayes_binary_recovery` object.

## Details

A small recovery run is a smoke test, not validation. Even when all
declared thresholds pass, the object records no automatic validation
claim.
