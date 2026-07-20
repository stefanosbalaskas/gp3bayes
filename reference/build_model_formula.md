# Build an Approved Model Formula

Constructs a backend-independent R formula from a
[`create_model_contract()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_model_contract.md)
result. The formula records the approved fixed effects, one optional
interaction, the participant grouping structure, an optional
participant-level random slope, and an optional crossed item intercept.

## Usage

``` r
build_model_formula(contract)
```

## Arguments

- contract:

  A `gp3bayes_model_contract` created by
  [`create_model_contract()`](https://stefanosbalaskas.github.io/gp3bayes/reference/create_model_contract.md).

## Value

An R formula. The formula is a specification only and has not been
translated to, validated by, or fitted with a Bayesian backend.

## Details

The participant random intercept is always included. When
`contract$random_slope` is `TRUE`, it is replaced by a correlated
participant intercept-and-condition-slope term. A declared item
identifier adds a crossed item random intercept.

The trial identifier is treated as a row key and is not added as a
predictor or grouping factor. A declared time column is included as a
linear population-level term only.

## Examples

``` r
contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "stimulus_id",
  condition_col = "condition",
  predictors = "age_z"
)

build_model_formula(contract)
#> selected ~ condition + age_z + (1 | participant_id) + (1 | stimulus_id)
#> <environment: base>
```
