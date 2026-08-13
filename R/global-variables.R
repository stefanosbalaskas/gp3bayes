# Global variables used by ggplot2 non-standard evaluation.
#
# Registered centrally so R CMD check can distinguish data-frame
# column names used inside aes() from unresolved R objects.

utils::globalVariables(
    c(
        ".group",
        "available",
        "changed",
        "check",
        "cmdstanr_mean",
        "column",
        "correlation",
        "expected",
        "fraction_missing",
        "item",
        "observed_tail_rate",
        "path",
        "predictive_lower_tail_rate",
        "predictive_mean_tail_rate",
        "predictive_upper_tail_rate",
        "rstan_mean",
        "status_code",
        "variable_1",
        "variable_2"
    )
)

# Phase 3 ggplot2 NSE variables.
utils::globalVariables(
    c(
        "coverage",
        "standardized_bias",
        "rmse",
        "repetition",
        "truth",
        "diagnostic_status",
        "completed",
        "count",
        "scenario",
        "scale_multiplier",
        "standardized_shift",
        "maximum_standardized_shift",
        "alternative",
        "alternative_median",
        "alternative_lower",
        "alternative_upper",
        "reference_median",
        "omitted_unit",
        "median_shift",
        "sensitivity",
        "distribution",
        "standardized_location_shift",
        "contraction",
        "elpd_loo",
        "influence_rank"
    )
)
