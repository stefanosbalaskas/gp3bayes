# Posterior pupil trajectories and declared estimands

## Lightweight posterior-draw contract

The estimand layer operates on posterior prediction draws. For
documentation and tests, a deterministic draw matrix can be used without
compiling Stan.

``` r

grid <- expand.grid(
  .event_time = seq(0, 1, length.out = 21),
  .condition = factor(c("control", "treatment")),
  KEEP.OUT.ATTRS = FALSE
)
set.seed(2026)
mu <- ifelse(grid$.condition == "treatment", 0.15, 0) +
  0.25 * sin(pi * grid$.event_time)
draws <- matrix(
  rnorm(300 * nrow(grid), rep(mu, each = 300), 0.06),
  nrow = 300
)
pred <- as_pupil_prediction_draws(draws, grid, "millimetres")
```

## Trajectory uncertainty

``` r

trajectory <- estimate_pupil_trajectory(pred, probability = 0.95)
head(pupil_trajectory_table(trajectory))
#>   .event_time .condition    estimate        median        lower     upper
#> 1        0.00    control 0.002499136 -0.0003379517 -0.109771080 0.1237874
#> 2        0.05    control 0.043415282  0.0501940460 -0.081746697 0.1660529
#> 3        0.10    control 0.074564976  0.0717499708 -0.031731909 0.1971629
#> 4        0.15    control 0.114372026  0.1152661166 -0.002348101 0.2363749
#> 5        0.20    control 0.142101058  0.1402868252  0.028388224 0.2545414
#> 6        0.25    control 0.180003590  0.1786603138  0.068083824 0.2910737
```

Pointwise intervals describe uncertainty at each grid value. A
finite-grid simultaneous band can be requested explicitly; it is
qualified as a grid-based posterior band rather than a universal
continuous-time guarantee.

## Declared contrasts and windows

``` r

contrast <- pupil_condition_contrast(
  pred,
  contrast = c("treatment", "control"),
  threshold = 0.10
)
window <- estimate_pupil_window(pred, window = c(0.3, 0.8))
auc <- estimate_pupil_auc(pred, window = c(0.3, 0.8))
peak <- estimate_pupil_peak(pred, window = c(0.3, 0.8))
latency <- estimate_pupil_peak_latency(pred, window = c(0.3, 0.8))

head(as.data.frame(contrast))
#>    .event_time            contrast  estimate    median        lower     upper
#> 22        0.00 treatment - control 0.1512366 0.1480115 -0.002193476 0.3195893
#> 23        0.05 treatment - control 0.1456269 0.1386465 -0.002451938 0.3172826
#> 24        0.10 treatment - control 0.1554272 0.1563937 -0.017678839 0.3005029
#> 25        0.15 treatment - control 0.1461007 0.1519917 -0.018632237 0.3023661
#> 26        0.20 treatment - control 0.1611109 0.1606923 -0.011704219 0.3428709
#> 27        0.25 treatment - control 0.1463584 0.1494409 -0.019417686 0.3219812
#>    threshold probability_gt_threshold
#> 22       0.1                0.7533333
#> 23       0.1                0.7066667
#> 24       0.1                0.7500000
#> 25       0.1                0.7033333
#> 26       0.1                0.7266667
#> 27       0.1                0.7200000
as.data.frame(window)
#>   .condition    estimand  estimate    median     lower     upper window_start
#> 1    control window_mean 0.2164691 0.2171261 0.1804861 0.2549529          0.3
#> 2  treatment window_mean 0.3680499 0.3680008 0.3300174 0.4051905          0.3
#>   window_end
#> 1        0.8
#> 2        0.8
as.data.frame(auc)
#>   .condition estimand  estimate    median      lower     upper window_start
#> 1    control      auc 0.1105039 0.1107491 0.09204704 0.1303858          0.3
#> 2  treatment      auc 0.1861384 0.1863730 0.16620597 0.2057661          0.3
#>   window_end
#> 1        0.8
#> 2        0.8
as.data.frame(peak)
#>   .condition estimand  estimate    median     lower     upper window_start
#> 1    control     peak 0.3231959 0.3239621 0.2594610 0.3998760          0.3
#> 2  treatment     peak 0.4753597 0.4671045 0.4115866 0.5624902          0.3
#>   window_end
#> 1        0.8
#> 2        0.8
as.data.frame(latency)
#>   .condition     estimand  estimate median     lower upper window_start
#> 1    control peak_latency 0.5131667    0.5 0.3420833  0.75          0.3
#> 2  treatment peak_latency 0.4960000    0.5 0.3000000  0.70          0.3
#>   window_end
#> 1        0.8
#> 2        0.8
```

Windows are supplied by the analyst. The package does not search across
time for the most favourable interval and relabel it confirmatory. Peak
and peak-latency summaries propagate posterior-draw uncertainty within
the declared evaluation grid.
