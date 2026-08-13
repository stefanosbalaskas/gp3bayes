# Hierarchical Effect and Variance Atlases

The expanded group-level layer can inspect raw posterior deviations,
rank uncertainty, and baseline latent variance partitioning.

``` r

draws <- group_effect_draws_table(
  fit,
  groups = "participant_id",
  coefficients = "Intercept",
  ndraws = 1000
)

ranks <- group_effect_rank_probability_table(
  fit,
  group = "participant_id",
  coefficient = "Intercept"
)

partition <- random_intercept_variance_partition(fit)

plot_group_effect_distribution(draws)
plot_group_effect_rank_probability(ranks)
plot_random_intercept_variance_partition(partition)
```

Rank probabilities and variance fractions remain descriptive posterior
quantities; they do not establish substantive group importance or causal
variance attribution.
