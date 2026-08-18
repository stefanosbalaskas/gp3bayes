# gp3bayes 0.5.0.9000 advanced pupillometry structural/backend-free audit.
# Run from the gp3bayes package root after devtools::document().
# No Stan sampling and no Git mutation are performed.

expected_version <- "0.5.0.9000"
expected_base_04_exports <- 370L
expected_new_exports <- 88L
expected_total_exports <- expected_base_04_exports + expected_new_exports
expected_base_04_s3 <- 169L
expected_new_s3 <- 61L
expected_total_s3 <- expected_base_04_s3 + expected_new_s3
expected_merge_parent <- "33136233203a2d7eb259df282cfb42957e34cb83"
expected_tag <- "59d29668967916c1993255a51905514080955fad"

fail <- function(...) stop(..., call. = FALSE)
git <- function(args) {
  out <- system2("git", args = args, stdout = TRUE, stderr = TRUE)
  st <- attr(out, "status")
  if (!is.null(st) && st != 0L) fail("git query failed: ", paste(args, collapse = " "), "\n", paste(out, collapse = "\n"))
  out
}
strip_export <- function(x) {
  y <- sub("^export\\((.*)\\)$", "\\1", x)
  gsub("^[\"']|[\"']$", "", y)
}
s3_key <- function(x) {
  inside <- sub("^S3method\\((.*)\\)$", "\\1", x)
  parts <- strsplit(inside, ",", fixed = TRUE)[[1L]]
  parts <- trimws(gsub("^[\"']|[\"']$", "", parts))
  if (length(parts) != 2L) return(NA_character_)
  paste0(parts[[1L]], ".", parts[[2L]])
}
is_protected <- function(path) {
  grepl("^paper(/|$)", path) || identical(path, "gp3bayes-paper-bundle.zip") || identical(path, "gp3bayes.Rmd") || identical(path, "refs.bib")
}

cat("\n============================================================\nADVANCED PUPILLOMETRY 0.5 AUDIT\n============================================================\n")

if (!file.exists("DESCRIPTION") || !file.exists("NAMESPACE")) fail("Run from the gp3bayes package root after devtools::document().")
desc <- read.dcf("DESCRIPTION")
stopifnot(identical(unname(desc[1L, "Package"]), "gp3bayes"), identical(unname(desc[1L, "Version"]), expected_version))
cat("DESCRIPTION package/version: PASS\n")

branch <- git(c("rev-parse", "--abbrev-ref", "HEAD"))
if (identical(branch, "master")) fail("0.5 development audit refuses to run on master; use the dedicated feature branch.")
if (length(git(c("diff", "--cached", "--name-only")))) fail("Staging must remain empty during development audit.")
stopifnot(identical(git(c("rev-list", "-n", "1", "v0.2.0")), expected_tag))
ancestor <- system2("git", args = c("merge-base", "--is-ancestor", expected_merge_parent, "HEAD"), stdout = TRUE, stderr = TRUE)
ancestor_status <- attr(ancestor, "status"); if (is.null(ancestor_status)) ancestor_status <- 0L
if (ancestor_status != 0L) fail("The 0.5 branch does not descend from the frozen 0.4 merge commit.")
cat("Git ancestry/tag/staging guards: PASS\n")

untracked <- sort(git(c("ls-files", "--others", "--exclude-standard")))
protected <- untracked[vapply(untracked, is_protected, logical(1L))]
if (length(protected) != 48L) fail("Expected 48 protected research files; observed ", length(protected), ".")
protected_md5 <- tools::md5sum(protected)
cat("Protected research workspace: 48 files — PASS\n")

# Parse every source/test/example script with R itself.
parse_files <- c(
  list.files("R", pattern = "\\.R$", full.names = TRUE),
  list.files("tests/testthat", pattern = "\\.R$", full.names = TRUE),
  list.files("inst/examples", pattern = "\\.R$", full.names = TRUE),
  list.files("tools", pattern = "\\.R$", full.names = TRUE)
)
for (f in parse_files) parse(file = f, keep.source = FALSE)
cat("R parser: ", length(parse_files), " source/test/example/tool files — PASS\n", sep = "")

ns <- readLines("NAMESPACE", warn = FALSE)
exports <- sort(unique(vapply(grep("^export\\(", ns, value = TRUE), strip_export, character(1L))))
s3_lines <- grep("^S3method\\(", ns, value = TRUE)
s3 <- sort(unique(vapply(s3_lines, s3_key, character(1L))))

expected_new <- read.delim("tools/public-api-0.5-new-exports.tsv", stringsAsFactors = FALSE, check.names = FALSE)$name
expected_new <- sort(unique(expected_new))
expected_s3_new <- read.delim("tools/public-api-0.5-new-s3.tsv", stringsAsFactors = FALSE, check.names = FALSE)$method
expected_s3_new <- sort(unique(expected_s3_new))
stopifnot(length(expected_new) == expected_new_exports, length(expected_s3_new) == expected_new_s3)
if (!all(expected_new %in% exports)) fail("Missing expected new 0.5 regular export(s): ", paste(setdiff(expected_new, exports), collapse = ", "))
if (!all(expected_s3_new %in% s3)) fail("Missing expected new 0.5 S3 registration(s): ", paste(setdiff(expected_s3_new, s3), collapse = ", "))
if (length(exports) != expected_total_exports) fail("Expected ", expected_total_exports, " regular exports after documentation; observed ", length(exports), ".")
if (length(s3) != expected_total_s3) fail("Expected ", expected_total_s3, " S3 registrations after documentation; observed ", length(s3), ".")

old_api <- read.delim("inst/api/public-api-0.4.0.9000.tsv", stringsAsFactors = FALSE, check.names = FALSE)
name_col <- intersect(c("name", "export", "function", "symbol"), names(old_api))
if (!length(name_col)) name_col <- names(old_api)[[1L]] else name_col <- name_col[[1L]]
old_names <- sort(unique(as.character(old_api[[name_col]])))
if (length(old_names) != expected_base_04_exports) fail("Frozen 0.4 API manifest does not contain 370 names.")
if (!all(old_names %in% exports)) fail("0.5 removed frozen 0.4 export(s): ", paste(setdiff(old_names, exports), collapse = ", "))
if (!identical(sort(setdiff(exports, old_names)), expected_new)) {
  fail("Current exports differ from frozen 0.4 + exact expected 0.5 additions.\nUnexpected: ", paste(setdiff(setdiff(exports, old_names), expected_new), collapse = ", "), "\nMissing: ", paste(setdiff(expected_new, setdiff(exports, old_names)), collapse = ", "))
}
cat("Public API: 370 frozen + 88 exact additions = 458 — PASS\n")
cat("S3 API: 169 frozen + 61 exact additions = 230 — PASS\n")

if (!requireNamespace("pkgload", quietly = TRUE)) fail("Install pkgload/devtools before running this audit.")
pkgload::load_all(".", quiet = TRUE, export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
cat("pkgload::load_all(): PASS\n")

# Freeze the 0.4 call signatures as well as names.
#
# Historical 0.4 manifests predate signature storage. When the manifest
# contains formals, use them directly. Otherwise reconstruct the exact
# validated 0.4 package from its Git commit and inspect it in an isolated
# R subprocess. This preserves a strict compatibility gate without
# changing the historical manifest.
missing_arg <- quote(expr = )

signature_current <- function(nm) {
  nsenv <- asNamespace("gp3bayes")
  fn <- get(nm, envir = nsenv, inherits = FALSE)

  if (!is.function(fn)) {
    fail(
      "Frozen 0.4 export is not a function in 0.5: ",
      nm
    )
  }

  paste(
    deparse(
      formals(fn),
      width.cutoff = 500L
    ),
    collapse = " "
  )
}

current_old_formals <- vapply(
  old_names,
  signature_current,
  character(1L)
)

if ("formals" %in% names(old_api)) {

  frozen_old_formals <- as.character(
    old_api$formals[
      match(old_names, old_api[[name_col]])
    ]
  )

  formal_source <- "historical 0.4 API manifest"

} else {

  if (!requireNamespace("callr", quietly = TRUE)) {
    fail(
      "Package `callr` is required to reconstruct the frozen 0.4 ",
      "function signatures."
    )
  }

  archive_file <- tempfile(
    pattern = "gp3bayes-0.4-base-",
    fileext = ".zip"
  )

  base_dir <- tempfile(
    pattern = "gp3bayes-0.4-base-"
  )

  dir.create(
    base_dir,
    recursive = TRUE
  )

  archive_file_cmd <- gsub(
    "\\\\",
    "/",
    archive_file
  )

  archive_out <- system2(
    "git",
    args = c(
      "archive",
      "--format=zip",
      paste0(
        "--output=",
        shQuote(archive_file_cmd, type = "cmd")
      ),
      expected_merge_parent
    ),
    stdout = TRUE,
    stderr = TRUE
  )

  archive_status <- attr(
    archive_out,
    "status"
  )

  if (!is.null(archive_status) && archive_status != 0L) {
    fail(
      "Could not reconstruct frozen 0.4 source from Git commit:\n",
      paste(archive_out, collapse = "\n")
    )
  }

  if (!file.exists(archive_file)) {
    fail("git archive did not create the frozen 0.4 archive.")
  }

  utils::unzip(
    archive_file,
    exdir = base_dir
  )

  if (!file.exists(file.path(base_dir, "DESCRIPTION"))) {
    fail("Reconstructed frozen 0.4 source lacks DESCRIPTION.")
  }

  frozen_old_formals <- callr::r(
    function(pkg_path, exported_names) {

      if (!requireNamespace("pkgload", quietly = TRUE)) {
        stop("pkgload is required in the isolated audit process.")
      }

      pkgload::load_all(
        pkg_path,
        quiet = TRUE,
        export_all = FALSE,
        helpers = FALSE,
        attach_testthat = FALSE
      )

      nsenv <- asNamespace("gp3bayes")
      missing_arg <- quote(expr = )

      signature_base <- function(nm) {
        fn <- get(nm, envir = nsenv, inherits = FALSE)

        if (!is.function(fn)) {
          stop(
            "Frozen 0.4 export is not a function: ",
            nm
          )
        }

        paste(
          deparse(
            formals(fn),
            width.cutoff = 500L
          ),
          collapse = " "
        )
      }

      vapply(
        exported_names,
        signature_base,
        character(1L)
      )
    },
    args = list(
      pkg_path = base_dir,
      exported_names = old_names
    ),
    show = FALSE
  )

  unlink(
    archive_file,
    force = TRUE
  )

  unlink(
    base_dir,
    recursive = TRUE,
    force = TRUE
  )

  formal_source <- paste0(
    "exact frozen Git commit ",
    expected_merge_parent
  )
}

if (length(frozen_old_formals) != length(old_names)) {
  fail(
    "Frozen 0.4 signature reconstruction returned ",
    length(frozen_old_formals),
    " signatures; expected ",
    length(old_names),
    "."
  )
}

mismatch <- old_names[
  unname(current_old_formals) !=
    unname(frozen_old_formals)
]

if (length(mismatch)) {
  cat("\nFORMAL SIGNATURE MISMATCHES:\n")
  for (nm in mismatch) {
    i <- match(nm, old_names)
    cat(
      "\n", nm,
      "\n  frozen:  ", frozen_old_formals[[i]],
      "\n  current: ", current_old_formals[[i]],
      "\n",
      sep = ""
    )
  }
  fail(
    "0.5 changed frozen 0.4 function formals: ",
    paste(mismatch, collapse = ", ")
  )
}

cat(
  "Frozen 0.4 function formals: 370/370 exact — PASS [",
  formal_source,
  "]\n",
  sep = ""
)

# Backend-free scientific smoke.
sim <- simulate_advanced_pupil_timecourse(
  n_participants = 12, trials_per_participant = 3,
  time_points = 15, time_range = c(-200, 1200),
  family = "student", residual_scale = 0.08,
  missing_fraction = 0.05, seed = 2050
)
stopifnot(inherits(sim, "gp3bayes_pupil_advanced_simulation"), is.data.frame(sim$data), is.list(sim$truth))

dist <- specify_pupil_distribution("student", "condition_time")
spec <- specify_advanced_pupil_timecourse_model(
  sim$data,
  temporal_structure = "gaussian_process",
  distribution = dist,
  gp_spec = create_pupil_gp_spec("matern32", basis = "approximate", k = 20),
  autocorrelation = "none",
  predictive_target = "future_segment"
)
stopifnot(inherits(spec, "gp3bayes_pupil_advanced_specification"))
stopifnot(inherits(audit_pupil_computational_budget(spec), "gp3bayes_pupil_complexity_audit"))
stopifnot(inherits(audit_advanced_pupil_identifiability(spec), "gp3bayes_pupil_identifiability_audit"))
stopifnot(inherits(pupil_model_card(spec), "gp3bayes_pupil_model_card"))
stopifnot(nrow(create_pupil_advanced_sensitivity_suite(spec)$scenarios) >= 2L)

acspec <- specify_advanced_pupil_timecourse_model(sim$data, family = "gaussian", autocorrelation = "ar2")
stopifnot(inherits(audit_pupil_temporal_dependence(acspec), "gp3bayes_pupil_temporal_dependence_audit"))

bin <- simulate_binocular_pupil_timecourse(n_participants = 8, trials_per_participant = 2, time_points = 9, time_range = c(0, 800), seed = 2051)
bprep <- prepare_binocular_pupil_timecourse(bin$data)
stopifnot(inherits(audit_binocular_pupil_readiness(bprep), "gp3bayes_binocular_pupil_audit"))
bspec <- specify_binocular_pupil_model(bprep)
stopifnot(inherits(bspec, "gp3bayes_binocular_pupil_specification"))

shape_sim <- simulate_pupil_response_shape(n_participants = 8, trials_per_participant = 2, time_points = 13, seed = 2052)
shape_spec <- specify_pupil_response_shape_model(shape_sim$data)
stopifnot(inherits(shape_spec, "gp3bayes_pupil_response_shape_specification"), isTRUE(shape_spec$experimental))

# Functional estimands without Stan by constructing a controlled posterior object.
grid <- expand.grid(time = seq(0, 1000, length.out = 21), condition = factor(c("A", "B")), KEEP.OUT.ATTRS = FALSE)
grid <- grid[order(grid$condition, grid$time), , drop = FALSE]
set.seed(2053)
mu <- ifelse(grid$condition == "A", sin(grid$time / 300), 0.6 * sin(grid$time / 300))
draws <- matrix(rnorm(300 * nrow(grid), rep(mu, each = 300), 0.05), nrow = 300)
fake_spec <- list(mapping = list(time = "time", condition = "condition"))
pred <- structure(list(grid = grid, draws = draws, specification = fake_spec), class = c("gp3bayes_pupil_advanced_trajectory", "gp3bayes_pupil_trajectory"))
der <- estimate_pupil_trajectory_derivative(pred)
con <- estimate_pupil_dynamic_contrast(pred, c("A", "B"), threshold = 0.05)
dur <- estimate_pupil_threshold_duration(con, direction = "absolute", threshold = 0.05)
stopifnot(inherits(der, "gp3bayes_pupil_trajectory_derivative"), inherits(con, "gp3bayes_pupil_dynamic_contrast"), inherits(dur, "gp3bayes_pupil_threshold_duration"))

score <- score_pupil_predictions(rnorm(12), matrix(rnorm(400 * 12), 400, 12))
stopifnot(inherits(score, "gp3bayes_pupil_predictive_score"))

# Plot smoke: no file is retained after the audit.
pdf_file <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf_file)
plot_advanced_pupil_simulation(sim)
plot_pupil_model_complexity(spec$complexity_audit)
plot_pupil_identifiability_audit(audit_advanced_pupil_identifiability(spec))
plot_pupil_trajectory_derivative(der)
plot_pupil_dynamic_contrast(con)
plot_pupil_predictive_calibration(score)
bgrid <- expand.grid(time = seq(0, 800, by = 100), condition = factor(c("control", "experimental")), KEEP.OUT.ATTRS = FALSE)
bleft <- matrix(rnorm(300 * nrow(bgrid), rep(sin(bgrid$time / 300), each = 300), 0.1), nrow = 300)
bright <- matrix(rnorm(300 * nrow(bgrid), rep(sin(bgrid$time / 300) + 0.02, each = 300), 0.1), nrow = 300)
bfake <- structure(
  list(grid = bgrid, left_draws = bleft, right_draws = bright, mapping = list(time = "time", condition = "condition"), probability = 0.95),
  class = "gp3bayes_binocular_pupil_trajectory"
)
plot_binocular_pupil_trajectory(bfake)
grDevices::dev.off()
unlink(pdf_file)
cat("Backend-free simulation/specification/audit/graphics smoke: PASS\n")

# Translation-only brms smoke; no Stan fitting.
if (requireNamespace("brms", quietly = TRUE)) {
  gspec <- specify_advanced_pupil_timecourse_model(sim$data, family = "gaussian", residual_scale = "time", autocorrelation = "ar1")
  gt <- translate_advanced_pupil_model_to_brms(gspec)
  stopifnot(inherits(gt, "gp3bayes_pupil_advanced_brms_specification"))

  gpspec <- specify_advanced_pupil_timecourse_model(sim$data, temporal_structure = "gaussian_process", family = "gaussian", gp_spec = create_pupil_gp_spec("matern52", "approximate", 15), autocorrelation = "none")
  gpt <- translate_advanced_pupil_model_to_brms(gpspec)
  stopifnot(inherits(gpt, "gp3bayes_pupil_advanced_brms_specification"))

  bt <- translate_binocular_pupil_model_to_brms(bspec)
  stopifnot(is.list(bt), !is.null(bt$formula))

  st <- translate_pupil_response_shape_to_brms(shape_spec)
  stopifnot(is.list(st), !is.null(st$formula))
  cat("brms translation-only smoke: PASS\n")
} else {
  cat("brms translation-only smoke: SKIPPED (brms not installed)\n")
}

# Protected workspace must be bitwise unchanged during audit.
protected_after <- sort(git(c("ls-files", "--others", "--exclude-standard")))
protected_after <- protected_after[vapply(protected_after, is_protected, logical(1L))]
stopifnot(identical(sort(protected_after), sort(protected)), identical(unname(tools::md5sum(protected_after)), unname(protected_md5)))
stopifnot(identical(git(c("rev-list", "-n", "1", "v0.2.0")), expected_tag))

cat("\n============================================================\nADVANCED PUPILLOMETRY 0.5 AUDIT: PASS\n============================================================\n")
cat("Regular exports: 458 exact (370 frozen + 88 new)\n")
cat("S3 registrations: 230 exact (169 frozen + 61 new)\n")
cat("Protected research workspace: 48 files preserved\n")
cat("No Stan sampling and no Git mutation performed.\n")
