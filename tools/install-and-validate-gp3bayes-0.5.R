# Local 0.5 documentation/test/install workflow. No Git mutation.

validate_and_install_gp3bayes_0_5 <- function(
    project_root = ".",
    freeze_api = FALSE,
    full_check = FALSE,
    build_vignettes = FALSE,
    install = TRUE) {
  project_root <- normalizePath(project_root, mustWork = TRUE)
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(project_root)
  if (!requireNamespace("devtools", quietly = TRUE)) stop("Install devtools first.", call. = FALSE)
  d <- read.dcf("DESCRIPTION")
  if (!identical(unname(d[1L, "Version"]), "0.5.0.9000")) stop("Expected Version 0.5.0.9000.", call. = FALSE)

  cat("\n[1/6] roxygen documentation\n")
  devtools::document(roclets = c("rd", "namespace", "collate"), quiet = FALSE)

  if (freeze_api) {
    cat("\n[2/6] freeze 0.5 public API\n")
    source("tools/freeze-public-api-0.5.R", local = TRUE)
    freeze_gp3bayes_0_5_public_api(".", overwrite = FALSE)
  } else {
    cat("\n[2/6] API freeze skipped (development mode)\n")
  }

  cat("\n[3/6] structural/backend-free audit\n")
  source("tools/audit-advanced-pupillometry-0.5.R", local = new.env(parent = globalenv()))

  cat("\n[4/6] full testthat suite\n")
  devtools::test(stop_on_failure = TRUE)

  cat("\n[5/6] package check\n")
  if (full_check) {
    devtools::check(document = FALSE, args = "--as-cran", error_on = "warning")
  } else {
    devtools::check(document = FALSE, error_on = "warning", build_args = if (build_vignettes) NULL else "--no-build-vignettes")
  }

  if (install) {
    cat("\n[6/6] manual local install\n")
    devtools::install(upgrade = "never", dependencies = FALSE, build_vignettes = build_vignettes, quiet = FALSE)
  } else {
    cat("\n[6/6] install skipped\n")
  }

  cat("\n0.5 local validation workflow complete. No Git operation was performed.\n")
  invisible(TRUE)
}
