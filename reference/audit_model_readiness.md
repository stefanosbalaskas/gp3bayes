# Audit Data Readiness for an Approved Model Contract

Audits whether a data frame satisfies the observable data requirements
of an existing model contract created by
[`create_model_contract()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_model_contract.md).
The audit is backend-independent and does not construct a formula,
define executable priors, fit a model, or establish model adequacy.

## Usage

``` r
audit_model_readiness(data, contract)
```

## Arguments

- data:

  A data frame containing the declared outcome, grouping identifiers,
  predictors, and optional design columns.

- contract:

  A `gp3bayes_model_contract` created by
  [`create_model_contract()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_model_contract.md).

## Value

An object of class `gp3bayes_readiness_audit`. The object records:

- whether the data are ready to proceed to a later model-building gate;

- pass, warning, and failure counts;

- one structured row per readiness check;

- declared and observed column summaries; and

- the audited model contract.

The input data are not retained in the returned object.

## Details

A readiness audit evaluates observable data properties only. Passing the
audit does not establish convergence, model adequacy, predictive
validity, causal identification, or substantive validity.

Failures block progression to a later model-building gate. Warnings
identify weak or unusual structures that require review but do not
automatically block progression.

Binary outcomes must be logical or numeric values encoded exclusively as
zero and one, with both classes observed. Duration outcomes must be
numeric, finite, strictly positive, uncensored, and variable.

## Interpretation boundaries

Readiness checks cannot determine whether a model is scientifically
justified. Behavioural measurements must not be interpreted as direct
measures of latent psychological or protected attributes.

## Examples

``` r
binary_data <- data.frame(
  participant_id = rep(c("p1", "p2"), each = 4),
  trial_id = rep(1:4, times = 2),
  condition = rep(c("control", "treatment"), times = 4),
  selected = c(0, 1, 0, 1, 1, 0, 1, 0)
)

binary_contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  trial_col = "trial_id",
  condition_col = "condition"
)

audit_model_readiness(binary_data, binary_contract)
#> <gp3bayes_readiness_audit>
#>   Family: binary
#>   Rows: 8
#>   Status: ready
#>   Ready: TRUE
#>   Checks: 17 passed, 0 warnings, 0 failures
```
