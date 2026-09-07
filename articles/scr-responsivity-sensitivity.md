# Retention-first Bayesian SCR responsivity

[`estimate_scr_responsivity_bayes()`](https://stefanosbalaskas.github.io/gp3bayes/reference/estimate_scr_responsivity_bayes.md)
treats SCR responsivity as graded evidence rather than a binary
preprocessing deletion rule. It keeps the conventional median-amplitude
non-responder flag, estimates a Beta-Binomial posterior response
probability, and retains every participant with finite trial data for
model-based sensitivity analyses.

This supports a primary-model/robustness workflow in which hard
exclusion can be compared with hierarchical retention. The
implementation is deliberately transparent and dependency-light; it does
not claim to reproduce a Dirichlet-process mixture model.

Methodological motivation includes Thomas & Rabinak (2026), DOI
10.1016/j.biopsycho.2026.109336.
