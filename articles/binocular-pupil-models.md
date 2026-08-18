# Joint Binocular Pupil Models

``` r

library(gp3bayes)
sim <- simulate_binocular_pupil_timecourse(
  n_participants = 12,
  trials_per_participant = 4,
  time_points = 31,
  residual_correlation = 0.70,
  eye_bias = 0.02,
  seed = 3040
)
prep <- prepare_binocular_pupil_timecourse(sim$data)
prep
#> <gp3bayes_binocular_pupil_prepared>
#>   Rows: 1488 
#>   Left: pupil_left 
#>   Right: pupil_right
audit_binocular_pupil_readiness(prep)
#> <gp3bayes_binocular_pupil_audit>
#>   Status: pass 
#>                    metric        value
#>                      rows 1.488000e+03
#>   left_available_fraction 9.791667e-01
#>  right_available_fraction 9.805108e-01
#>   both_available_fraction 9.744624e-01
#>     mean_right_minus_left 2.025271e-02
#>       sd_right_minus_left 2.705794e-02
#>       pearson_correlation 9.892452e-01
```

No averaged pupil column is created. Left and right eyes remain separate
responses.

``` r

spec <- specify_binocular_pupil_model(
  prep,
  temporal_structure = "smooth",
  family = "gaussian",
  residual_correlation = TRUE
)
spec
#> <gp3bayes_binocular_pupil_specification>
#>   Family: gaussian 
#>   Temporal structure: smooth 
#>   Residual correlation: TRUE 
#>   Fit performed: FALSE
```

``` r

fit <- fit_binocular_pupil_model(
  spec,
  backend = "cmdstanr",
  chains = 4,
  cores = 2
)

trajectory <- estimate_binocular_pupil_trajectory(fit)
plot_binocular_pupil_trajectory(trajectory)

pupil_binocular_correlation(fit)
pupil_binocular_difference(trajectory)
pupil_binocular_agreement_table(trajectory, tolerance = 0.10)
```

Residual eye correlation is an association parameter. High posterior
correlation does not establish that the two eyes are interchangeable or
justify arbitrary eye substitution.
