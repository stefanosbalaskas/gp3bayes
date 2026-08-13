# Simulation-Based Calibration Diagnostics

The existing SBC workflow delegates computation to `SBC` and retains a
conservative gp3bayes result.

``` r

sbc_overview_table(result)
sbc_stats_table(result)

plot_sbc_rank_gg(result)
plot_sbc_ecdf_gg(result)
plot_sbc_coverage_gg(result)
plot_sbc_simulated_vs_estimated_gg(result)
```

Graphical agreement is not converted into an automatic
implementation-validity claim.
