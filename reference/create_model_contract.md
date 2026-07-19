# Create an Approved Bayesian Model Contract

Creates an inspectable model-contract object for one of the two model
families approved for the initial `gp3bayes` development scope. The
function records neutral data-column mappings while preserving the
approved likelihood, link, estimands, assumptions, diagnostics,
sensitivity requirements, and interpretation boundaries.

## Usage

``` r
create_model_contract(
  family,
  outcome_col,
  participant_col,
  item_col = NULL,
  trial_col = NULL,
  condition_col = NULL,
  time_col = NULL,
  predictors = character(),
  interaction = NULL,
  random_slope = FALSE,
  outcome_unit = NULL,
  notes = character()
)
```

## Arguments

- family:

  Character scalar identifying the approved model family. Supported
  values are `"binary"` and `"duration"`.

- outcome_col:

  Character scalar naming the outcome column.

- participant_col:

  Character scalar naming the participant identifier column.

- item_col:

  Optional character scalar naming an item or stimulus identifier
  column.

- trial_col:

  Optional character scalar naming a trial identifier column.

- condition_col:

  Optional character scalar naming the focal condition column.

- time_col:

  Optional character scalar naming a linear time or trial order column.
  This does not define a time-course or autocorrelation model.

- predictors:

  Character vector naming additional predictors.

- interaction:

  Optional character vector of length two naming one prespecified
  two-way interaction. Higher-order or multiple interactions are not
  supported by the initial contract.

- random_slope:

  Logical scalar indicating whether one participant-level random slope
  for the focal condition is requested. Readiness must be assessed
  separately before fitting.

- outcome_unit:

  Optional character scalar recording the outcome unit. It is required
  for the duration family and must be `NULL` for the binary family.

- notes:

  Optional character vector containing user-supplied design or analysis
  notes. Notes do not override the approved model contract.

## Value

An object of class `gp3bayes_model_contract`. It is a named list
containing the approved methodological specification, neutral column
mappings, requested model structure, assumptions, diagnostics,
sensitivity requirements, limitations, and unsupported uses.

## Details

The returned object is a specification and audit record. It does not
validate a data frame, construct a backend formula, fit a model, or
imply that the proposed analysis is appropriate. Those gates are handled
by separate workflows.

The binary contract uses a Bernoulli likelihood with a logit link. The
duration contract uses a lognormal likelihood for strictly positive,
finite, uncensored durations.

## Interpretation boundaries

Contract creation does not establish causal identification, model
adequacy, convergence, predictive validity, or substantive validity.
Behavioural measurements must not be interpreted as direct measures of
latent psychological or protected attributes.

## Examples

``` r
binary_contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "stimulus_id",
  trial_col = "trial_id",
  condition_col = "condition"
)

binary_contract
#> <gp3bayes_model_contract>
#>   Family: binary
#>   Likelihood: Bernoulli
#>   Link: logit
#>   Outcome: selected
#>   Participant: participant_id
#>   Item: stimulus_id
#>   Condition: condition
#>   Random slope requested: FALSE
#>   Fitting performed: FALSE

duration_contract <- create_model_contract(
  family = "duration",
  outcome_col = "response_time",
  participant_col = "participant_id",
  trial_col = "trial_id",
  condition_col = "condition",
  outcome_unit = "milliseconds"
)

duration_contract
#> <gp3bayes_model_contract>
#>   Family: duration
#>   Likelihood: lognormal
#>   Link: identity on mean log duration
#>   Outcome: response_time
#>   Participant: participant_id
#>   Condition: condition
#>   Outcome unit: milliseconds
#>   Random slope requested: FALSE
#>   Fitting performed: FALSE
```
