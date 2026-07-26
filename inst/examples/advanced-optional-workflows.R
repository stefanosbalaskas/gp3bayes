# Advanced optional gp3bayes examples

library(gp3bayes)

bayesian_backend_capabilities()

binary_pathology <- simulate_binary_pathology(
  "near_separation",
  seed = 2026
)
binary_pathology
plot(binary_pathology)

duration_pathology <- simulate_duration_pathology(
  "heavy_tailed_contamination",
  seed = 2026
)
duration_pathology
plot(duration_pathology)

# Optional fixed-effects separation screen
if (requireNamespace("detectseparation", quietly = TRUE)) {
  separated <- data.frame(
    y = c(0, 0, 0, 1, 1, 1),
    x = c(-3, -2, -1, 1, 2, 3)
  )
  screen <- detect_binary_separation(separated, y ~ x)
  print(screen)
  plot(screen)
}

# Optional matrix-based PSIS-LOO example
if (requireNamespace("loo", quietly = TRUE)) {
  set.seed(2026)
  log_lik <- matrix(rnorm(800, -1, 0.25), nrow = 200, ncol = 4)
  loo_result <- compute_psis_loo_from_log_lik(log_lik)
  print(loo_result)
  plot(loo_result)
}
