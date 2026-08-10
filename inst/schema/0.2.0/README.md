# gp3bayes 0.2.0 object-schema baselines

These files freeze structural compatibility baselines for selected
public gp3bayes objects at the 0.2.0 release boundary.

The schemas record classes, R types, names, nested field paths, and
structural information. They do not record analysis values.

Compatibility checks intentionally default to `compare_lengths = FALSE`
because analysis-specific cardinalities may legitimately vary.

These baselines do not establish numerical equivalence, model adequacy,
causal validity, or substantive validity.
