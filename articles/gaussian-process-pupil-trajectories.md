# Gaussian-Process Pupil Trajectories

``` r

library(gp3bayes)
sim <- simulate_advanced_pupil_timecourse(
  n_participants = 12,
  trials_per_participant = 4,
  time_points = 41,
  seed = 3020
)
```

## Approximate GP is the default

``` r

gp32 <- create_pupil_gp_spec("matern32", "approximate", k = 30)
gp52 <- create_pupil_gp_spec("matern52", "approximate", k = 30)
gpeq <- create_pupil_gp_spec("exp_quad", "approximate", k = 30)

gp32
#> $kernel
#> [1] "matern32"
#> 
#> $basis
#> [1] "approximate"
#> 
#> $k
#> [1] 30
#> 
#> $scale
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "gp3bayes_pupil_gp_spec"
gp52
#> $kernel
#> [1] "matern52"
#> 
#> $basis
#> [1] "approximate"
#> 
#> $k
#> [1] 30
#> 
#> $scale
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "gp3bayes_pupil_gp_spec"
gpeq
#> $kernel
#> [1] "exp_quad"
#> 
#> $basis
#> [1] "approximate"
#> 
#> $k
#> [1] 30
#> 
#> $scale
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "gp3bayes_pupil_gp_spec"
```

``` r

spec <- specify_advanced_pupil_timecourse_model(
  sim$data,
  temporal_structure = "gaussian_process",
  gp_spec = gp32,
  family = "gaussian",
  autocorrelation = "none",
  predictive_target = "future_segment"
)

spec
#> <gp3bayes_pupil_advanced_specification>
#>   Version: 0.5.0.9000 
#>   Family: gaussian 
#>   Temporal structure: gaussian_process 
#>   Residual scale: constant 
#>   GP: matern32 / approximate 
#>   ARMA: (0,0) 
#>   Predictive target: future_segment 
#>   Complexity: ok 
#>   Fit performed: FALSE
plot_pupil_model_complexity(spec)
```

![](gaussian-process-pupil-trajectories_files/figure-html/unnamed-chunk-3-1.png)

Exact GP computation remains available, but the complexity audit
requires explicit review when the unique time-by-condition grid becomes
large.

## Hyperparameters are posterior estimands

``` r

fit <- fit_advanced_pupil_model_backend(spec, backend = "cmdstanr")
hyper <- pupil_gp_hyperparameters(fit)
pupil_gp_table(hyper)
plot_pupil_gp_hyperparameters(hyper)

trajectory <- predict_advanced_pupil_trajectory(fit)
plot_advanced_pupil_trajectory(trajectory)
```

Length scale and marginal GP standard deviation describe the fitted
temporal function prior/posterior. They are not direct psychological
constructs.
