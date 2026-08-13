# Grouped PSIS-LOO Influence

Pointwise PSIS-LOO diagnostics may be aggregated by a declared
participant, item, condition, or other observation-level grouping
variable.

``` r

group_loo <- loo_group_influence_table(
  loo_atlas,
  group = "participant_id"
)

plot_loo_group_influence(group_loo)
plot_loo_group_elpd(group_loo)
```

Aggregation supports review of concentrated predictive influence. It
never removes a group automatically.
