# Assess Power-Scaled Prior and Likelihood Sensitivity

Delegates to priorsense while retaining a conservative review status.
Low local sensitivity does not prove that prior choices are irrelevant
or that a model is robust.

## Usage

``` r
assess_powerscaled_sensitivity(
  fit,
  variable = NULL,
  prior_selection = NULL,
  likelihood_selection = NULL
)
```

## Arguments

- fit:

  A gp3bayes fit or `brmsfit`.

- variable:

  Optional posterior variables to inspect.

- prior_selection:

  Optional tagged priors to perturb.

- likelihood_selection:

  Optional likelihood subset.

## Value

A `gp3bayes_powerscale_sensitivity`.
