# End-to-End Hierarchical Lognormal Duration Workflow

## Scope

The duration workflow is restricted to strictly positive, finite,
uncensored durations modeled with a hierarchical lognormal likelihood.
Zero, negative, censored, truncated, shifted-lognormal, Gamma, Weibull,
survival, and mixture outcomes are outside this contract.

## Simulate durations with stored truth

``` r

library(gp3bayes)

duration_simulation <- simulate_hierarchical_duration_data(
  n_participants = 16,
  trials_per_participant = 10,
  n_items = 8,
  baseline_median = 500,
  random_slope_sd = 0.15,
  residual_sd = 0.40,
  outcome_unit = "milliseconds",
  seed = 2030
)

duration_simulation
#> <gp3bayes_duration_simulation>
#>   Rows: 160
#>   Participants: 16
#>   Outcome unit: milliseconds
#>   Strictly positive: TRUE
#>   Censored: FALSE
#>   Fit performed: FALSE
head(duration_simulation$data)
#>   participant_id item_id trial_id condition participant_covariate
#> 1           p001    i001        1   control             0.2115378
#> 2           p001    i002        2   control             0.2115378
#> 3           p001    i003        3 treatment             0.2115378
#> 4           p001    i004        4   control             0.2115378
#> 5           p001    i005        5 treatment             0.2115378
#> 6           p001    i006        6 treatment             0.2115378
#>   trial_covariate duration true_median true_mean
#> 1       0.3422712 377.9327    589.0824  638.1453
#> 2      -1.0042921 215.0466    396.8953  429.9516
#> 3       0.2455131 552.0358    443.1421  480.0501
#> 4       1.4803098 594.2899    465.2464  503.9954
#> 5      -0.2710918 724.4346    376.7323  408.1092
#> 6      -0.6263812 631.7951    615.8786  667.1733
```

The fixed effects and grouping scales are on the log-duration scale. The
baseline is stored as a median in the declared outcome unit.

## Declare the duration contract

``` r

duration_contract <- create_model_contract(
  family = "duration",
  outcome_col = "duration",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  predictors = c(
    "participant_covariate",
    "trial_covariate"
  ),
  interaction = c(
    "condition",
    "participant_covariate"
  ),
  random_slope = TRUE,
  outcome_unit = "milliseconds"
)

duration_contract
#> <gp3bayes_model_contract>
#>   Family: duration
#>   Likelihood: lognormal
#>   Link: identity on mean log duration
#>   Outcome: duration
#>   Participant: participant_id
#>   Item: item_id
#>   Condition: condition
#>   Outcome unit: milliseconds
#>   Random slope requested: TRUE
#>   Fitting performed: FALSE
```

The outcome unit is mandatory. The package never guesses a unit from
column names or magnitudes.

## Prepare, convert, and audit

``` r

duration_prepared <- prepare_hierarchical_duration_data(
  duration_simulation$data,
  duration_contract,
  condition_levels = c(
    "control",
    "treatment"
  )
)

duration_prepared
#> <gp3bayes_duration_prepared>
#>   Analysis rows: 160
#>   Rows removed: 0
#>   Outcome unit: milliseconds
#>   Readiness status: ready
#>   Fit performed: FALSE
duration_prepared$decision_log
#>             decision                               value
#> 1 outcome_validation strictly positive finite uncensored
#> 2    unit_conversion    milliseconds x 1 -> milliseconds
#> 3     missing_values             error; rows removed = 0
#> 4   condition_coding         control=-0.5, treatment=0.5
#> 5  predictor_scaling                                none
```

Explicit unit conversion uses `outcome_multiplier` and `converted_unit`
together. For example, converting milliseconds to seconds is recorded
as:

``` r

duration_seconds <- prepare_hierarchical_duration_data(
  duration_simulation$data,
  duration_contract,
  condition_levels = c(
    "control",
    "treatment"
  ),
  outcome_multiplier = 0.001,
  converted_unit = "seconds"
)

duration_seconds$transformations$outcome
#> $source_unit
#> [1] "milliseconds"
#> 
#> $analysis_unit
#> [1] "seconds"
#> 
#> $multiplier
#> [1] 0.001
#> 
#> $strictly_positive
#> [1] TRUE
#> 
#> $finite
#> [1] TRUE
#> 
#> $censored
#> [1] FALSE
```

## Specify priors and check prior implications

``` r

duration_specification <- specify_duration_model(
  duration_prepared,
  baseline = 500,
  intercept_scale = 1,
  coefficient_scale = 0.5,
  group_sd_scale = 1,
  residual_scale = 1,
  correlation_eta = 2,
  student_df = 3
)

duration_specification
#> <gp3bayes_duration_model_specification>
#>   Formula: duration ~ condition + participant_covariate + trial_covariate + condition:participant_covariate + (1 + condition | participant_id) + (1 | item_id)
#>   Family: lognormal
#>   Outcome unit: milliseconds
#>   Fitting engine: none
#>   Fit performed: FALSE
duration_specification$priors$table
#>   parameter_class distribution                                  target location
#> 1       Intercept       normal              Population-level intercept 6.214608
#> 2               b       normal           Population-level coefficients 0.000000
#> 3              sd    student_t         Group-level standard deviations 0.000000
#> 4           sigma    student_t             Residual standard deviation 0.000000
#> 5             cor          lkj Participant intercept-slope correlation       NA
#>   scale df shape lower upper
#> 1   1.0 NA    NA  -Inf   Inf
#> 2   0.5 NA    NA  -Inf   Inf
#> 3   1.0  3    NA     0   Inf
#> 4   1.0  3    NA     0   Inf
#> 5    NA NA     2    -1     1
#>                                                                                                             rationale
#> 1 The intercept prior must reflect a plausible baseline median duration in the recorded unit after log transformation
#> 2                                         Coefficient priors regularise implausible multiplicative duration contrasts
#> 3           Residual and group-level scale priors constrain implausible dispersion without fixing variability to zero
#> 4           Residual and group-level scale priors constrain implausible dispersion without fixing variability to zero
#> 5                                           The correlation prior regularises an included intercept-slope correlation
```

``` r

duration_prior_predictive <- check_duration_prior_predictive(
  duration_specification,
  draws = 100,
  seed = 2031
)

duration_prior_predictive
#> <gp3bayes_duration_prior_predictive_check>
#>   Draws: 100
#>   Outcome unit: milliseconds
#>   Prior predictive adequate: FALSE
#>   Fit performed: FALSE
#>   Posterior adequacy established: FALSE
duration_prior_predictive$checks
#>                      check violation_probability maximum_probability status
#> 1           overall_median                  0.05                0.25   pass
#> 2           upper_tail_q99                  0.69                0.25   fail
#> 3 coefficient_of_variation                  0.27                0.25   fail
#> 4   condition_median_ratio                  0.01                0.25   pass
#> 5    nonfinite_predictions                  0.00                0.25   pass
```

The prior check examines overall medians, upper tails, coefficients of
variation, and condition median ratios. A failure requests review and
does not automatically alter the priors.

## Translate and fit through the restricted backend

``` r

duration_translation <- translate_duration_model_to_brms(
  duration_specification
)

duration_fit <- fit_duration_model(
  duration_specification,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 2032,
  adapt_delta = 0.95,
  max_treedepth = 12,
  refresh = 100
)
```

The likelihood, formula, priors, backend, and sampling algorithm are
derived from the approved package specification. There is no
unrestricted formula or backend argument.

## Diagnostics and posterior interpretation

``` r

duration_diagnostics <- diagnose_duration_fit(
  duration_fit
)

duration_posterior <- summarise_duration_posterior(
  duration_fit
)

duration_predictive <- check_duration_posterior_predictive(
  duration_fit,
  draws = 500,
  seed = 2033
)
```

Exponentiating a population-level coefficient gives a conditional median
duration ratio under the lognormal model. This ratio is not
automatically a causal effect.

The predictive check compares observed and replicated median, mean,
upper-tail, dispersion, condition-ratio, and grouping summaries. Passing
those summaries does not prove global model adequacy.

## Sensitivity, recovery, and reporting

``` r

duration_sensitivity <- assess_duration_prior_sensitivity(
  duration_fit,
  scale_multipliers = c(
    tighter = 0.5,
    wider = 2
  )
)

duration_recovery <- run_duration_recovery(
  repetitions = 20,
  baseline_median = 500,
  outcome_unit = "milliseconds",
  seed = 4001
)

duration_report <- create_duration_model_report(
  duration_fit,
  diagnostics = duration_diagnostics,
  posterior_summary = duration_posterior,
  posterior_predictive = duration_predictive,
  prior_sensitivity = duration_sensitivity,
  recovery = duration_recovery,
  file = "duration-model-report.md"
)
```

Recovery results apply to the declared synthetic data-generating
process. Reports preserve the distinction between successful fitting,
numerical sampling diagnostics, predictive behavior, robustness checks,
and substantive interpretation.
