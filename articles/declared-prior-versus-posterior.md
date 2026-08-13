# Declared Priors versus Fitted Posteriors

The bridge uses the backend-independent prior specification retained by
`gp3bayes`; saved backend prior draws are not required.

``` r

bridge <- prior_posterior_bridge(fit)

prior_posterior_summary_table(bridge)
prior_posterior_distance_table(bridge)

plot_prior_posterior_density(bridge)
plot_prior_posterior_intervals(bridge)
plot_prior_posterior_shift(bridge)
plot_prior_posterior_contraction(bridge)
```

Shift, contraction, interval overlap, Kolmogorov-Smirnov distance, and
quantile-based Wasserstein distance are descriptive marginal summaries,
not automatic measures of prior adequacy.
