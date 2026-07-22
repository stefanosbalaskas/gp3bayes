# Specify a Backend-Independent Duration Model

Combines prepared positive-duration data with the approved hierarchical
lognormal formula and explicit prior specification.

## Usage

``` r
specify_duration_model(
  prepared,
  baseline,
  intercept_scale = 1,
  coefficient_scale = 0.5,
  group_sd_scale = 1,
  residual_scale = 1,
  correlation_eta = 2,
  student_df = 3
)
```

## Arguments

- prepared:

  A `gp3bayes_duration_prepared`.

- baseline:

  Plausible baseline median in the prepared outcome unit.

- intercept_scale:

  Positive normal-intercept prior scale.

- coefficient_scale:

  Positive population-coefficient prior scale.

- group_sd_scale:

  Positive group standard-deviation prior scale.

- residual_scale:

  Positive lognormal residual-scale prior scale.

- correlation_eta:

  LKJ shape for an approved random slope.

- student_df:

  Degrees of freedom for half-Student-t scale priors.

## Value

A `gp3bayes_duration_model_specification`.
