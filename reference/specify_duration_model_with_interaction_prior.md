# Specify a Duration Model with a Separate Interaction Prior

Uses the candidate duration defaults of 0.35 for population main effects
and 0.25 for the approved two-way interaction.

## Usage

``` r
specify_duration_model_with_interaction_prior(
  prepared,
  baseline,
  intercept_scale = 1,
  main_effect_scale = 0.35,
  interaction_scale = 0.25,
  group_sd_scale = 0.5,
  residual_scale = 0.5,
  correlation_eta = 2,
  student_df = 3
)
```

## Arguments

- prepared:

  A `gp3bayes_binary_prepared` object containing one approved two-way
  interaction.

- baseline:

  Plausible baseline median in the recorded outcome unit.

- intercept_scale:

  Intercept-prior scale.

- main_effect_scale:

  Common population main-effect prior scale.

- interaction_scale:

  Interaction prior scale.

- group_sd_scale:

  Group-level standard-deviation prior scale.

- residual_scale:

  Lognormal residual-scale prior scale.

- correlation_eta:

  LKJ shape.

- student_df:

  Student-t degrees of freedom.

## Value

A duration model specification with advanced prior metadata.
