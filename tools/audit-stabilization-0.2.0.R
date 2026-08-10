# gp3bayes 0.2.0 stabilization static/release audit

stopifnot(file.exists("DESCRIPTION"), dir.exists("R"), dir.exists("tests"))

read_utf8 <- function(path) readLines(path, warn = FALSE, encoding = "UTF-8")
read_all <- function(paths) {
  paste(unlist(lapply(paths, read_utf8), use.names = FALSE), collapse = "\n")
}

r_files <- list.files("R", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)
rd_files <- list.files("man", pattern = "\\.Rd$", recursive = TRUE, full.names = TRUE)
vignette_files <- list.files(
  "vignettes", pattern = "\\.(Rmd|Rnw|qmd)$", recursive = TRUE,
  full.names = TRUE, ignore.case = TRUE
)
test_files <- list.files("tests", pattern = "\\.[Rr]$", recursive = TRUE, full.names = TRUE)

r_text <- read_all(r_files)
example_text <- read_all(c(r_files, rd_files))
execution_text <- read_all(c(r_files, rd_files, vignette_files, test_files))

desc <- read.dcf("DESCRIPTION")
stopifnot(
  unname(desc[1L, "Version"]) %in% c("0.2.0.9001", "0.2.0"),
  grepl("\\bwithr\\b", unname(desc[1L, "Imports"]), perl = TRUE)
)

# CRAN feedback forward-port.
stopifnot(
  !grepl("\\dontrun{", example_text, fixed = TRUE),
  grepl("\\donttest{", example_text, fixed = TRUE),
  !grepl(".GlobalEnv", r_text, fixed = TRUE),
  !grepl("globalenv(", r_text, fixed = TRUE),
  !grepl("<<-", r_text, fixed = TRUE),
  !grepl("cores = 3", execution_text, fixed = TRUE),
  !grepl("cores = 4", execution_text, fixed = TRUE),
  !grepl('file = "gp3bayes-binary-model-report.md"', execution_text, fixed = TRUE),
  !grepl('file = "gp3bayes-duration-model-report.md"', execution_text, fixed = TRUE),
  !grepl('file = "binary-model-report.md"', execution_text, fixed = TRUE),
  !grepl('file = "duration-model-report.md"', execution_text, fixed = TRUE),
  grepl("withr::with_seed", r_text, fixed = TRUE)
)

expected_exports <- c(
  "validate_gp3bayes_object",
  "diagnose_model_fit",
  "summarise_model_posterior",
  "check_model_ppc",
  "estimate_model_estimands",
  "model_workflow_status",
  "create_analysis_manifest",
  "validate_analysis_manifest",
  "freeze_analysis_manifest",
  "read_analysis_manifest",
  "compare_analysis_manifests",
  "analysis_manifest_table",
  "write_reproducibility_report",
  "audit_missingness_structure",
  "audit_fixed_effect_design",
  "audit_random_effects_support",
  "audit_design_support",
  "preflight_model_specification",
  "create_sensitivity_suite_plan",
  "run_sensitivity_suite",
  "summarise_sensitivity_suite",
  "collect_model_evidence",
  "create_model_evidence_report",
  "backend_capabilities",
  "validate_backend_environment",
  "audit_backend_parity",
  "capture_gp3bayes_schema",
  "compare_gp3bayes_schemas",
  "validate_gp3bayes_schema",
  "freeze_gp3bayes_schema",
  "read_gp3bayes_schema"
)

namespace <- read_utf8("NAMESPACE")
exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", namespace, value = TRUE))
missing_exports <- setdiff(expected_exports, exports)
if (length(missing_exports)) {
  stop("Missing expected exports: ", paste(missing_exports, collapse = ", "), call. = FALSE)
}

expected_articles <- c(
  "stable-unified-workflow.Rmd",
  "reproducible-analysis-manifests.Rmd",
  "pre-fit-design-diagnostics.Rmd",
  "sensitivity-evidence-workflow.Rmd",
  "backend-reliability.Rmd",
  "release-case-study.Rmd"
)
stopifnot(all(file.exists(file.path("vignettes", expected_articles))))

new_test_files <- c(
  "test-unified-workflow-api.R",
  "test-analysis-manifest.R",
  "test-design-support-diagnostics.R",
  "test-sensitivity-evidence-suite.R",
  "test-backend-reliability.R"
)
stopifnot(all(file.exists(file.path("tests", "testthat", new_test_files))))

# Runtime defaults and stable signatures.
devtools::load_all(quiet = TRUE)

has_required_argument <- function(fun, argument) {
  fmls <- formals(fun)
  argument %in% names(fmls) && identical(fmls[[argument]], quote(expr = ))
}

stopifnot(
  identical(.gp3b_default_cores(1L), 1L),
  identical(.gp3b_default_cores(2L), 2L),
  identical(.gp3b_default_cores(4L), 2L),
  has_required_argument(create_binary_model_report, "file"),
  has_required_argument(create_duration_model_report, "file")
)

# Lightweight smoke coverage for new public objects.
contract <- create_model_contract(
  "binary", "selected", "participant_id", condition_col = "condition"
)
expectations <- list(
  capture_gp3bayes_schema(contract),
  backend_capabilities()
)
stopifnot(
  inherits(expectations[[1L]], "gp3bayes_object_schema"),
  inherits(expectations[[2L]], "gp3bayes_backend_capabilities_v2")
)

cat("\n============================================================\n")
cat("gp3bayes 0.2.0 STABILIZATION AUDIT PASSED\n")
cat("============================================================\n")
cat("New public exports: ", length(expected_exports), "\n", sep = "")
cat("New stabilization articles: ", length(expected_articles), "\n", sep = "")
cat("CRAN compliance forward-port: PASS\n")
cat("Runtime default checks: PASS\n")
