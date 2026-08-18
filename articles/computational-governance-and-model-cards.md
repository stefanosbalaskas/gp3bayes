# Computational Governance and Model Cards

``` r

library(gp3bayes)
sim <- simulate_advanced_pupil_timecourse(
  n_participants = 10,
  trials_per_participant = 4,
  time_points = 35,
  seed = 3070
)

spec <- specify_advanced_pupil_timecourse_model(
  sim$data,
  temporal_structure = "gaussian_process",
  gp_spec = create_pupil_gp_spec("matern32", "approximate", k = 30),
  residual_scale = "condition_time",
  participant_trajectory = "none",
  predictive_target = "new_trial_known_participant"
)
```

## Complexity is audited before Stan

``` r

budget <- audit_pupil_computational_budget(spec)
budget
#> <gp3bayes_pupil_complexity_audit>
#>   Status: ok 
#>   Rows: 1400 
#>   Participants: 10 
#>   Series: 40 
#>               check status
#>                rows     ok
#>              series     ok
#>      approximate_gp     ok
#>  layered_complexity     ok
#>                                                message
#>                                     1400 analysis rows
#>                            40 participant/trial series
#>                             approximate GP with k = 30
#>  2 advanced complexity layers requested simultaneously
plot_pupil_model_complexity(budget)
```

![](computational-governance-and-model-cards_files/figure-html/unnamed-chunk-2-1.png)

The complexity gate is not a statistical adequacy test. It is a
reproducible guard against accidentally requesting models that combine
many expensive layers or exact Gaussian processes over very large grids.

## Model card

``` r

card <- pupil_model_card(spec)
card
#> <gp3bayes_pupil_model_card>
#>                   field                       value
#>        gp3bayes_version                  0.5.0.9000
#>           fit_performed                       FALSE
#>                 backend                        none
#>                    rows                        1400
#>            participants                          10
#>              conditions                           2
#>                  family                    gaussian
#>      temporal_structure            gaussian_process
#>          residual_scale              condition_time
#>         autocorrelation                        none
#>  participant_trajectory                        none
#>       measurement_model                       FALSE
#>       missingness_model                       FALSE
#>       predictive_target new_trial_known_participant
#>       complexity_status                          ok
#> Governance:
#>   - No automatic preprocessing, interpolation, exclusion, or model selection.
#>   - No automatic cognitive-state, causal, or adequacy interpretation.
#>   - Measurement and missingness models remain assumption-conditional.
#>   - Predictive comparison is tied to an explicitly declared target.
pupil_model_card_table(card)
#>                     field                       value
#> 1        gp3bayes_version                  0.5.0.9000
#> 2           fit_performed                       FALSE
#> 3                 backend                        none
#> 4                    rows                        1400
#> 5            participants                          10
#> 6              conditions                           2
#> 7                  family                    gaussian
#> 8      temporal_structure            gaussian_process
#> 9          residual_scale              condition_time
#> 10        autocorrelation                        none
#> 11 participant_trajectory                        none
#> 12      measurement_model                       FALSE
#> 13      missingness_model                       FALSE
#> 14      predictive_target new_trial_known_participant
#> 15      complexity_status                          ok
```

A model card records family, temporal structure, residual scale,
autocorrelation, data dimensions, measurement/missingness declarations,
predictive target, complexity status, and governance text. It is
designed to support methods supplements and audit trails without
becoming a validity certificate.
