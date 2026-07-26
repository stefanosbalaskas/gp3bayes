# Transformation Replay and Detailed Posterior Predictive Checks

## Replay recorded transformations

``` r

sim <- simulate_hierarchical_binary_data(
  n_participants = 16,
  trials_per_participant = 8,
  n_items = 8,
  seed = 2030
)

contract <- create_model_contract(
  family = "binary",
  outcome_col = "selected",
  participant_col = "participant_id",
  item_col = "item_id",
  trial_col = "trial_id",
  condition_col = "condition",
  predictors = c("participant_covariate", "trial_covariate"),
  interaction = c("condition", "participant_covariate")
)

prepared <- prepare_hierarchical_binary_data(
  sim$data,
  contract,
  condition_levels = c("control", "treatment"),
  scale_predictors = c("participant_covariate", "trial_covariate")
)

recipe <- create_transformation_recipe(prepared)
recipe
#> 
#> Transformation recipe
#>  Family: binary
#>  Fixed formula: selected ~ condition + participant_covariate + trial_covariate + condition:participant_covariate
#> This recipe replays only transformations recorded by gp3bayes. It does not infer new scaling, recode unseen levels, or repair missing data silently.

replay_audit <- validate_transformation_replay(prepared)
replay_audit
#> 
#> Transformation replay audit
#>  Family: binary
#>  Status: pass
#>  Model-matrix columns identical: TRUE
#>  Maximum model-matrix difference: 0
```

The recipe stores the already-approved mapping, condition coding,
scaling centres/scales, formula, and model-matrix columns. It does not
learn a new transformation from new data.

``` r

plot(replay_audit)
```

![](transformation-replay-and-detailed-ppc_files/figure-html/unnamed-chunk-2-1.png)

For prediction data, replay is explicit:

``` r

raw_again <- invert_transformation_recipe(prepared$data, recipe)
replayed <- apply_transformation_recipe(
  raw_again,
  recipe,
  input_scale = "raw",
  require_outcome = TRUE
)
head(replayed)
#>   participant_id item_id trial_id condition participant_covariate
#> 1           p001    i001        1      -0.5           -0.05889804
#> 2           p001    i002        2      -0.5           -0.05889804
#> 3           p001    i003        3       0.5           -0.05889804
#> 4           p001    i004        4       0.5           -0.05889804
#> 5           p001    i005        5       0.5           -0.05889804
#> 6           p001    i006        6      -0.5           -0.05889804
#>   trial_covariate selected true_probability
#> 1      -0.3536421        1        0.2192870
#> 2       1.2658581        1        0.2068486
#> 3       0.6766974        1        0.4552207
#> 4       0.5555484        1        0.4605616
#> 5       1.5225971        0        0.3446031
#> 6      -1.2812988        0        0.2109570
```

Unseen condition values, missing required transformed predictors, or a
duration unit inconsistent with the stored source unit produce errors
rather than silent recoding.

## Detailed binary PPC

``` r

binary_detail <- check_binary_ppc_details(
  fit_binary,
  draws = 500,
  calibration_bins = 10,
  sparse_cell_min = 3
)

binary_detail$calibration
binary_detail$participant_rates
binary_detail$item_rates
binary_detail$sparse_cells

plot(binary_detail, type = "calibration")
plot(binary_detail, type = "condition")
plot(binary_detail, type = "participant")
```

The detailed binary object exposes calibration gaps, participant and
item event rates, participant-condition sparsity, and replicated
all-zero/all-one participant patterns. These are descriptive discrepancy
checks, not a single pass/fail goodness-of-fit test.

## Detailed duration PPC

``` r

duration_detail <- check_duration_ppc_details(
  fit_duration,
  draws = 500,
  quantiles = c(0.50, 0.90, 0.95),
  tail_threshold = 2000
)

duration_detail$quantile_table
duration_detail$participant_medians
duration_detail$item_medians
duration_detail$within_participant_condition_ratios

plot(duration_detail, type = "ecdf")
plot(duration_detail, type = "log_ecdf")
plot(duration_detail, type = "condition")
plot(duration_detail, type = "tail")
```

Raw- and log-scale distributions are both retained because a lognormal
model can appear reasonable on one scale while still missing
substantively important tail or grouping structure. Persistent
discrepancies request model-contract review and never trigger an
automatic likelihood switch.
