# Ecosystem update — September 2026

## Related crossed location–scale development

The related Python package **gpbiometricspy** now includes a fully
exact-main-certified crossed participant–item Gaussian hierarchical
location–scale model with **one location random slope for each crossed
factor**.

This does not change `gp3bayes`, its Bayesian model families, or its
release evidence. The gpbiometricspy implementation is a separate
frequentist/Laplace-approximation path for crossed conditional location
and residual-scale heterogeneity, with participant- and item-specific
location slopes, explicit seen/unseen-level prediction semantics,
fail-closed design guards, and deterministic certificates.

Certified gpbiometricspy PR \#129 is pinned to merge SHA
`d078e0366ace49c3ebeb2f6800bad6394d70631e`: 14/14 exact-main push
workflow families, 12/12 OS/Python matrix lanes, 782/782 tests,
14,015/14,015 statements, and 6,757/6,776 raw branches (99.7196%). The
frozen `gpbiometrics 2.0.0` R-parity surface remains 406/406 and is
unchanged.

- [Crossed participant–item random-slope
  guide](https://stefanosbalaskas.github.io/gpbiometricspy/methods/crossed-random-slopes-location-scale/)
- [gpbiometricspy PR
  \#129](https://github.com/stefanosbalaskas/gpbiometricspy/pull/129)

This ecosystem note is informational: it does not imply Bayesian
equivalence, coefficient identity, or replacement of `gp3bayes`
workflows.
