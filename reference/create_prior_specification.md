# Create a Backend-Independent Prior Specification

Creates an inspectable prior table for one approved model family. The
returned object contains no backend-specific prior objects and performs
no sampling.

## Usage

``` r
create_prior_specification(
  contract,
  baseline = NULL,
  intercept_scale = NULL,
  coefficient_scale = NULL,
  group_sd_scale = 1,
  residual_scale = NULL,
  correlation_eta = 2,
  student_df = 3
)
```

## Arguments

- contract:

  A `gp3bayes_model_contract` created by
  [`create_model_contract()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_model_contract.md).

- baseline:

  Numeric scalar describing the expected baseline outcome. For binary
  models this is a probability strictly between zero and one. The
  default is `0.5`. For duration models this is a strictly positive
  baseline median in the recorded outcome unit and must be supplied.

- intercept_scale:

  Optional positive numeric scalar for the normal intercept prior.
  Defaults to `1.5` for binary models and `1` for duration models.

- coefficient_scale:

  Optional positive numeric scalar for normal population-level
  coefficient priors. Defaults to `1` for binary models and `0.5` for
  duration models.

- group_sd_scale:

  Positive numeric scalar for half-Student-t group-level
  standard-deviation priors.

- residual_scale:

  Optional positive numeric scalar for the half-Student-t residual
  standard-deviation prior. It applies only to the duration family and
  defaults to `1`.

- correlation_eta:

  Numeric scalar greater than or equal to one for the LKJ prior used
  when a participant-level random slope is requested.

- student_df:

  Positive numeric scalar giving the degrees of freedom for
  half-Student-t scale priors.

## Value

An object of class `gp3bayes_prior_specification`.

## Details

Binary baseline probabilities are transformed with the logit function.
Duration baseline medians are transformed with the natural logarithm.

## Examples

``` r
binary_contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id"
)

create_prior_specification(
  binary_contract,
  baseline = 0.35
)
#> <gp3bayes_prior_specification>
#>   Family: binary
#>   Baseline: 0.35
#>   Parameter classes: Intercept, b, sd
#>   Backend: none
#>   Executable: FALSE
```
