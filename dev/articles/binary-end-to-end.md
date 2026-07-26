# End-to-End Hierarchical Binary Workflow

## Scope

This article presents the approved Bernoulli-logit workflow for binary
trial-level outcomes with repeated observations, participant effects,
and optional crossed item effects. The interface is deliberately
restricted: users do not supply an arbitrary formula, likelihood,
backend, Stan program, or automatic model-selection rule.

The core contract, simulation, preparation, model specification, and
prior predictive check do not require a Bayesian backend.

## Simulate a known data-generating process

``` r

library(gp3bayes)

binary_simulation <- simulate_hierarchical_binary_data(
  n_participants = 16,
  trials_per_participant = 10,
  n_items = 8,
  random_slope_sd = 0.20,
  seed = 2026
)

binary_simulation
#> <gp3bayes_binary_simulation>
#>   Rows: 160
#>   Participants: 16
#>   Items: 8
#>   True condition effect: 0.8
#>   Seed: 2026
head(binary_simulation$data)
#>   participant_id item_id trial_id condition participant_covariate
#> 1           p001    i001        1   control            -0.5695716
#> 2           p001    i002        2   control            -0.5695716
#> 3           p001    i003        3 treatment            -0.5695716
#> 4           p001    i004        4   control            -0.5695716
#> 5           p001    i005        5   control            -0.5695716
#> 6           p001    i006        6 treatment            -0.5695716
#>   trial_covariate selected true_probability
#> 1      -1.1536152        0        0.2296494
#> 2      -0.3948543        0        0.2656513
#> 3       0.4120546        0        0.2472167
#> 4       0.7746397        0        0.2859311
#> 5       1.2926217        0        0.3621687
#> 6      -1.2061598        0        0.2672820
```

The simulation object retains fixed effects, grouping scales, the
condition coding, random effects, and the random-number seed. Stored
truth enables later recovery assessment without a private or external
data set.

## Declare the model contract

``` r

binary_contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
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
  random_slope = TRUE
)

binary_contract
#> <gp3bayes_model_contract>
#>   Family: binary
#>   Likelihood: Bernoulli
#>   Link: logit
#>   Outcome: selected
#>   Participant: participant_id
#>   Item: item_id
#>   Condition: condition
#>   Random slope requested: TRUE
#>   Fitting performed: FALSE
```

Contract creation records the intended outcome, grouping structure,
likelihood, link, supported interaction, assumptions, diagnostics,
limitations, and interpretation boundaries. It does not validate the
data or establish model adequacy.

## Prepare and audit the data

``` r

binary_prepared <- prepare_hierarchical_binary_data(
  binary_simulation$data,
  binary_contract,
  condition_levels = c(
    "control",
    "treatment"
  )
)

binary_prepared
#> <gp3bayes_binary_prepared>
#>   Input rows: 160
#>   Analysis rows: 160
#>   Rows removed: 0
#>   Readiness: ready
#>   Fixed matrix columns: (Intercept), condition, participant_covariate, trial_covariate, condition:participant_covariate
#>   Backend: none
#>   Fit performed: FALSE
binary_prepared$decision_log
#>                 decision                       value
#> 1 binary_outcome_mapping                    0=0; 1=1
#> 2       condition_coding control=-0.5; treatment=0.5
#> 3        numeric_scaling                        none
#> 4           missing_rows            error; dropped=0
```

Outcome mapping, condition coding, missing-value decisions, and
requested scaling are explicit and recorded. No variable is silently
transformed.

## Specify priors and inspect prior implications

``` r

binary_specification <- specify_binary_model(
  binary_prepared,
  baseline = 0.35,
  intercept_scale = 1.5,
  coefficient_scale = 0.75,
  group_sd_scale = 1,
  correlation_eta = 2,
  student_df = 3
)

binary_specification
#> <gp3bayes_binary_model_specification>
#>   Formula: selected ~ condition + participant_covariate + trial_covariate + condition:participant_covariate + (1 + condition | participant_id) + (1 | item_id)
#>   Fixed formula: selected ~ condition + participant_covariate + trial_covariate + condition:participant_covariate
#>   Baseline probability: 0.35
#>   Readiness: ready
#>   Fitting engine: none
#>   Backend dependency: none
#>   Fit performed: FALSE
binary_specification$priors$table
#>   parameter_class distribution                                  target
#> 1       Intercept       normal              Population-level intercept
#> 2               b       normal           Population-level coefficients
#> 3              sd    student_t         Group-level standard deviations
#> 4             cor          lkj Participant intercept-slope correlation
#>     location scale df shape lower upper
#> 1 -0.6190392  1.50 NA    NA  -Inf   Inf
#> 2  0.0000000  0.75 NA    NA  -Inf   Inf
#> 3  0.0000000  1.00  3    NA     0   Inf
#> 4         NA    NA NA     2    -1     1
#>                                                                                                    rationale
#> 1                  The intercept prior must encode plausible baseline event probabilities on the logit scale
#> 2 Coefficient priors regularise implausibly large log-odds contrasts while retaining substantive uncertainty
#> 3                         Group-level scale priors constrain extreme heterogeneity without fixing it to zero
#> 4                                  The correlation prior regularises an included intercept-slope correlation
```

``` r

binary_prior_predictive <- check_binary_prior_predictive(
  binary_specification,
  draws = 100,
  seed = 2027
)

binary_prior_predictive
#> <gp3bayes_binary_prior_predictive_check>
#>   Adequate: TRUE
#>   Draws: 100
#>   Failed checks: 0
#>   Backend: none
#>   Fit performed: FALSE
binary_prior_predictive$checks
#>                       check probability threshold status
#> 1              overall_rate        0.00      0.25   pass
#> 2           condition_rates        0.00      0.25   pass
#> 3        condition_contrast        0.00      0.25   pass
#> 4    participant_degeneracy        0.06      0.25   pass
#> 5 boundary_probability_mass        0.05      0.25   pass
```

A prior-predictive failure requests substantive review. The function
does not automatically change priors or select a different model.

## Translate and fit with the optional backend

The fitting route is fixed to `brms`, `rstan`, and full MCMC sampling.
The following code requires the optional backend and a working C++
toolchain.

``` r

binary_translation <- translate_binary_model_to_brms(
  binary_specification
)

binary_fit <- fit_binary_model(
  binary_specification,
  chains = 4,
  iter = 2000,
  warmup = 1000,
  cores = 4,
  seed = 2028,
  adapt_delta = 0.95,
  max_treedepth = 12,
  refresh = 100
)
```

A returned fit confirms that sampling completed. It does not by itself
establish convergence or posterior adequacy.

## Diagnose, interpret, and validate

``` r

binary_diagnostics <- diagnose_binary_fit(
  binary_fit
)

binary_posterior <- summarise_binary_posterior(
  binary_fit,
  probability = 0.95
)

binary_predictive <- check_binary_posterior_predictive(
  binary_fit,
  draws = 500,
  seed = 2029
)

binary_diagnostics
binary_posterior
binary_predictive
```

The diagnostic object reports R-hat, bulk and tail ESS, divergent
transitions, maximum-treedepth saturation, and chain-level energy
diagnostics. Its pass/review/fail status is a threshold report, not an
automatic declaration that the model converged.

Population-level coefficients are reported on the log-odds and
odds-ratio scales. Posterior probabilities and intervals are not
frequentist significance tests and are not automatically causal.

## Prior sensitivity and simulation recovery

``` r

binary_sensitivity <- assess_binary_prior_sensitivity(
  binary_fit,
  scale_multipliers = c(
    tighter = 0.5,
    wider = 2
  )
)

binary_recovery <- run_binary_recovery(
  repetitions = 20,
  seed = 3001
)
```

A small recovery run is only a smoke test. A larger run assesses the
declared synthetic design and does not validate every future use of the
package.

## Structured reporting

``` r

binary_report <- create_binary_model_report(
  binary_fit,
  diagnostics = binary_diagnostics,
  posterior_summary = binary_posterior,
  posterior_predictive = binary_predictive,
  prior_sensitivity = binary_sensitivity,
  recovery = binary_recovery,
  file = "binary-model-report.md"
)

binary_report
```

The report keeps fitting, diagnostics, predictive checks, sensitivity,
and recovery as separate evidence layers. It does not automatically
claim convergence, predictive validity, causal identification, or
substantive validity.
