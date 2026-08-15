# gp3bayes 0.4.0.9000 Bayesian Dynamic Pupillometry structural audit
# This audit is intentionally backend-independent and performs no Stan fitting.

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
expected_branch <- "feature/bayesian-pupillometry-foundation"
expected_version <- "0.4.0.9000"
expected_tag_target <- "59d29668967916c1993255a51905514080955fad"
expected_old_exports <- 324L
expected_total_exports <- 370L

expected_new_exports <- c(
  "create_pupil_contract",
  "inspect_gazepoint_pupil_schema",
  "gazepoint_pupil_mapping_table",
  "simulate_pupil_timecourse",
  "prepare_pupil_timecourse",
  "audit_pupil_readiness",
  "pupil_readiness_table",
  "audit_pupil_measurement_context",
  "pupil_measurement_audit_table",
  "specify_pupil_timecourse_model",
  "pupil_specification_table",
  "translate_pupil_model_to_brms",
  "check_pupil_prior_predictive",
  "fit_pupil_model_backend",
  "fit_pupil_model",
  "fit_pupil_model_cmdstanr",
  "as_pupil_prediction_draws",
  "predict_pupil_trajectory",
  "estimate_pupil_trajectory",
  "pupil_trajectory_table",
  "estimate_pupil_window",
  "estimate_pupil_auc",
  "estimate_pupil_peak",
  "estimate_pupil_peak_latency",
  "pupil_condition_contrast",
  "diagnose_pupil_fit",
  "pupil_residual_acf",
  "check_pupil_posterior_predictive",
  "pupil_ppc_table",
  "summarise_pupil_posterior",
  "create_pupil_validation_plan",
  "validate_pupil_model",
  "pupil_validation_table",
  "create_pupil_sensitivity_suite",
  "materialize_pupil_sensitivity_scenario",
  "compare_pupil_sensitivity_estimands",
  "pupil_sensitivity_table",
  "plot_pupil_readiness",
  "plot_pupil_observed_trajectory",
  "plot_pupil_posterior_trajectory",
  "plot_pupil_estimand",
  "plot_pupil_ppc",
  "plot_pupil_residual_acf",
  "plot_pupil_validation",
  "plot_pupil_sensitivity",
  "plot_pupil_measurement_audit"
)
expected_s3_methods <- c(
  "print.gp3bayes_pupil_contract",
  "as.data.frame.gp3bayes_pupil_contract",
  "print.gp3bayes_gazepoint_pupil_schema",
  "as.data.frame.gp3bayes_gazepoint_pupil_schema",
  "print.gp3bayes_pupil_simulation",
  "print.gp3bayes_pupil_prepared",
  "print.gp3bayes_pupil_readiness",
  "as.data.frame.gp3bayes_pupil_readiness",
  "as.data.frame.gp3bayes_pupil_measurement_audit",
  "print.gp3bayes_pupil_prior_predictive",
  "as.data.frame.gp3bayes_pupil_prior_predictive",
  "print.gp3bayes_pupil_model_specification",
  "as.data.frame.gp3bayes_pupil_model_specification",
  "print.gp3bayes_pupil_fit",
  "as.data.frame.gp3bayes_pupil_estimand",
  "as.data.frame.gp3bayes_pupil_trajectory",
  "print.gp3bayes_pupil_estimand",
  "print.gp3bayes_pupil_diagnostics",
  "print.gp3bayes_pupil_ppc",
  "as.data.frame.gp3bayes_pupil_diagnostics",
  "as.data.frame.gp3bayes_pupil_ppc",
  "print.gp3bayes_pupil_posterior_summary",
  "as.data.frame.gp3bayes_pupil_posterior_summary",
  "print.gp3bayes_pupil_validation_plan",
  "print.gp3bayes_pupil_validation",
  "as.data.frame.gp3bayes_pupil_validation",
  "print.gp3bayes_pupil_sensitivity",
  "as.data.frame.gp3bayes_pupil_sensitivity"
)
expected_sources <- c(
  "R/pupil-contract.R",
  "R/pupil-gazepoint.R",
  "R/pupil-simulation.R",
  "R/pupil-preparation.R",
  "R/pupil-specification.R",
  "R/pupil-fit.R",
  "R/pupil-estimands.R",
  "R/pupil-diagnostics.R",
  "R/pupil-validation.R",
  "R/pupil-sensitivity.R",
  "R/pupil-graphics.R"
)
expected_tests <- c(
  "tests/testthat/test-pupil-contract-gazepoint.R",
  "tests/testthat/test-pupil-simulation-preparation.R",
  "tests/testthat/test-pupil-specification-estimands.R",
  "tests/testthat/test-pupil-validation-sensitivity.R",
  "tests/testthat/test-pupil-graphics-failures.R"
)
expected_articles <- c(
  "vignettes/bayesian-dynamic-pupillometry.Rmd",
  "vignettes/pupil-preparation-and-auditing.Rmd",
  "vignettes/gazepoint-pupil-interoperability.Rmd",
  "vignettes/fitting-pupil-timecourse-models.Rmd",
  "vignettes/pupil-trajectories-and-estimands.Rmd",
  "vignettes/pupil-ppc-and-temporal-diagnostics.Rmd",
  "vignettes/pupil-temporal-validation.Rmd",
  "vignettes/pupil-baseline-gaze-luminance-sensitivity.Rmd",
  "vignettes/synthetic-gazepoint-pupillometry-case-study.Rmd"
)
article_slugs <- sub("\\.Rmd$", "", basename(expected_articles))

fail <- function(...) stop(..., call. = FALSE)
git <- function(args) {
  out <- system2("git", args = args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    fail("git ", paste(args, collapse = " "), " failed: ", paste(out, collapse = "\n"))
  }
  out
}
description_version <- function(path = "DESCRIPTION") {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  hit <- grep("^Version:", x, value = TRUE)
  if (length(hit) != 1L) fail("DESCRIPTION must contain exactly one Version field.")
  trimws(sub("^Version:", "", hit))
}
namespace_exports <- function(path = "NAMESPACE") {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  sort(sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", x, value = TRUE)))
}
namespace_s3 <- function(path = "NAMESPACE") {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  z <- grep("^S3method\\(", x, value = TRUE)
  if (!length(z)) return(character())
  inside <- sub("^S3method\\((.*)\\)$", "\\1", z)
  parts <- strsplit(inside, ",", fixed = TRUE)
  sort(vapply(parts, function(v) paste0(trimws(v[1]), ".", trimws(v[2])), character(1L)))
}
read_manifest <- function(path) {
  if (!file.exists(path)) fail("Missing API manifest: ", path)
  utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE)
}
roxygen_block_for <- function(lines, fn) {
  idx <- grep(paste0("^", fn, "\\s*<-\\s*function\\s*\\("), lines)
  if (length(idx) != 1L) return(character())
  j <- idx - 1L
  block <- character()
  while (j >= 1L && startsWith(lines[j], "#'")) {
    block <- c(lines[j], block)
    j <- j - 1L
  }
  block
}

cat("===== gp3bayes Bayesian Dynamic Pupillometry structural audit =====\n")

branch <- git(c("rev-parse", "--abbrev-ref", "HEAD"))
if (!identical(branch, expected_branch)) fail("Expected branch ", expected_branch, "; found ", branch, ".")
if (!identical(description_version(), expected_version)) fail("Expected version ", expected_version, ".")

tag_target <- git(c("rev-list", "-n", "1", "v0.2.0"))
if (!identical(tag_target, expected_tag_target)) fail("Frozen v0.2.0 tag target changed.")

staged <- git(c("diff", "--cached", "--name-only"))
if (length(staged)) fail("Staging area must remain empty. Staged: ", paste(staged, collapse = ", "))

missing_sources <- expected_sources[!file.exists(expected_sources)]
missing_tests <- expected_tests[!file.exists(expected_tests)]
missing_articles <- expected_articles[!file.exists(expected_articles)]
if (length(missing_sources)) fail("Missing pupil source files: ", paste(missing_sources, collapse = ", "))
if (length(missing_tests)) fail("Missing pupil tests: ", paste(missing_tests, collapse = ", "))
if (length(missing_articles)) fail("Missing pupil articles: ", paste(missing_articles, collapse = ", "))
if (!file.exists("tools/pupillometry-development-evidence.tsv")) fail("Missing development evidence table.")

exports <- namespace_exports()
if (length(exports) != expected_total_exports) {
  fail("Expected ", expected_total_exports, " regular exports after documentation; found ", length(exports), ".")
}
old_manifest <- read_manifest("inst/api/public-api-0.3.0.9000.tsv")
if (nrow(old_manifest) != expected_old_exports) fail("Frozen 0.3.0.9000 API manifest no longer has 324 rows.")
new_manifest <- read_manifest("inst/api/public-api-0.4.0.9000.tsv")
if (nrow(new_manifest) != expected_total_exports) fail("0.4.0.9000 API manifest must contain 370 rows.")
old_names <- sort(old_manifest$function_name)
new_names <- sort(new_manifest$function_name)
if (!all(old_names %in% new_names)) fail("0.4 API manifest lost an existing public export.")
manifest_delta <- setdiff(new_names, old_names)
if (!identical(sort(manifest_delta), sort(expected_new_exports))) {
  fail("Unexpected public API delta. Expected exactly the 46 approved pupil exports.")
}
if (!identical(sort(exports), new_names)) fail("NAMESPACE and 0.4 API manifest disagree.")

registered_s3 <- namespace_s3()
missing_s3 <- setdiff(expected_s3_methods, registered_s3)
if (length(missing_s3)) fail("Missing S3 registrations: ", paste(missing_s3, collapse = ", "))

source_text <- setNames(lapply(expected_sources, readLines, warn = FALSE, encoding = "UTF-8"), expected_sources)
source_blob <- paste(unlist(source_text, use.names = FALSE), collapse = "\n")
prohibited <- c(
  "setwd\\s*\\(", "\\.GlobalEnv", "save\\.image\\s*\\(", "\\bq\\s*\\(",
  "\\bshell\\s*\\(", "\\bsystem\\s*\\(", "\\bunlink\\s*\\(",
  "\\bfile\\.remove\\s*\\(", "\\baes_string\\s*\\("
)
bad <- prohibited[vapply(prohibited, function(p) grepl(p, source_blob, perl = TRUE), logical(1L))]
if (length(bad)) fail("Prohibited/unsafe source pattern(s) found: ", paste(bad, collapse = ", "))
if (grepl("cores\\s*=\\s*[3-9][0-9]*", source_blob, perl = TRUE)) fail("Package-controlled core default above two detected.")
if (grepl("\\\\dontrun\\s*\\{", source_blob, perl = TRUE)) fail("New pupil source contains \\dontrun examples.")
if (grepl("TODO|FIXME|PLACEHOLDER", source_blob, ignore.case = TRUE)) fail("Placeholder marker found in pupil source.")

# Closed modelling surface: none of the approved fitting/specification functions
# may expose raw formula/family/program or variadic backend escape hatches.
closed_functions <- c(
  "specify_pupil_timecourse_model", "translate_pupil_model_to_brms",
  "fit_pupil_model_backend", "fit_pupil_model", "fit_pupil_model_cmdstanr"
)
ns <- asNamespace("gp3bayes")
for (fn in closed_functions) {
  f <- get(fn, envir = ns, inherits = FALSE)
  nms <- names(formals(f))
  forbidden_formals <- intersect(nms, c("formula", "family", "stan_program", "algorithm", "..."))
  if (length(forbidden_formals)) fail(fn, " exposes forbidden modelling formals: ", paste(forbidden_formals, collapse = ", "))
}
spec_formals <- names(formals(get("specify_pupil_timecourse_model", envir = ns)))
if (!all(c("temporal_structure", "smooth_basis_dimension", "autocorrelation") %in% spec_formals)) {
  fail("Pupil specification does not expose the expected governed choices.")
}

# Interpretation/preprocessing boundary checks are positive invariants rather than
# naive word bans, because documentation intentionally states what is NOT inferred.
contract_text <- paste(source_text[["R/pupil-contract.R"]], collapse = "\n")
required_boundary_phrases <- c(
  "does not infer cognitive load", "does not automatically correct",
  "Interpretation remains the researcher's responsibility"
)
for (phrase in required_boundary_phrases) {
  if (!grepl(phrase, contract_text, fixed = TRUE)) fail("Missing governance boundary phrase: ", phrase)
}
if (grepl("correct_pfe\\s*<-\\s*function|interpolate_pupil\\s*<-\\s*function|automatic.*model selection",
          source_blob, ignore.case = TRUE, perl = TRUE)) {
  fail("Automatic correction/model-selection surface detected.")
}

# Every new regular export must have a directly attached roxygen examples block.
for (fn in expected_new_exports) {
  found <- FALSE
  for (path in expected_sources) {
    block <- roxygen_block_for(source_text[[path]], fn)
    if (length(block)) {
      found <- TRUE
      if (!any(grepl("@examples", block, fixed = TRUE))) fail("Missing @examples for ", fn, ".")
      break
    }
  }
  if (!found) fail("Cannot locate exported function definition for ", fn, ".")
}

# Rd alias coverage for the 46 new regular exports.
rd_files <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
rd_blob <- paste(unlist(lapply(rd_files, readLines, warn = FALSE, encoding = "UTF-8"), use.names = FALSE), collapse = "\n")
missing_alias <- expected_new_exports[!vapply(expected_new_exports, function(fn) {
  grepl(paste0("\\\\alias\\{", fn, "\\}"), rd_blob, perl = TRUE)
}, logical(1L))]
if (length(missing_alias)) fail("Missing Rd aliases: ", paste(missing_alias, collapse = ", "))

pkgdown <- paste(readLines("_pkgdown.yml", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
if (!grepl("Development documentation for gp3bayes 0.4.0.9000", pkgdown, fixed = TRUE)) {
  fail("pkgdown development tooltip not updated.")
}
missing_slugs <- article_slugs[!vapply(article_slugs, function(slug) grepl(slug, pkgdown, fixed = TRUE), logical(1L))]
if (length(missing_slugs)) fail("Articles missing from pkgdown configuration: ", paste(missing_slugs, collapse = ", "))

# Parse every new R source and focused test file in this R runtime.
for (path in c(expected_sources, expected_tests, "tools/audit-bayesian-pupillometry-foundation.R")) {
  parse(file = path, keep.source = FALSE)
}

cat("Version/branch/tag guards: PASS\n")
cat("Pupil source/test/article inventory: PASS\n")
cat("Public API: 324 existing + 46 pupil = 370 exports: PASS\n")
cat("S3 registrations: ", length(expected_s3_methods), ": PASS\n", sep = "")
cat("Restricted formula/family/backend surface: PASS\n")
cat("Safety, examples, documentation and governance scan: PASS\n")
cat("pkgdown article discoverability: PASS\n")
cat("Frozen v0.2.0 and protected staging boundary: PASS\n")
cat("===== structural audit complete =====\n")
