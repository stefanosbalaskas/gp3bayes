# Check Duration Prior Predictive Behaviour

Simulates positive-duration data from the declared prior specification
and prepared design without fitting a model.

## Usage

``` r
check_duration_prior_predictive(
  specification,
  draws = 500L,
  seed = 1L,
  plausible_median = NULL,
  maximum_q99 = NULL,
  maximum_cv = 5,
  maximum_condition_ratio = 10,
  maximum_extreme_probability = 0.25
)
```

## Arguments

- specification:

  A `gp3bayes_duration_model_specification`.

- draws:

  Number of prior predictive data sets.

- seed:

  Non-negative integer seed.

- plausible_median:

  Optional increasing pair for plausible overall medians in the prepared
  outcome unit.

- maximum_q99:

  Maximum plausible 99th percentile.

- maximum_cv:

  Maximum plausible coefficient of variation.

- maximum_condition_ratio:

  Maximum plausible ratio between condition medians in either direction.

- maximum_extreme_probability:

  Maximum fraction of prior predictive draws allowed to violate each
  criterion.

## Value

A `gp3bayes_duration_prior_predictive_check`.

## Details

Failure requests substantive prior review; it does not select or alter
priors automatically.
