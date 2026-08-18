# gp3bayes 0.5.0.9000 development overlay installer.
#
# This script mutates package source files only when the user explicitly calls
# apply_gp3bayes_0_5_overlay(). It never creates/switches Git branches, stages,
# commits, pushes, merges, tags, releases, deploys, stashes, resets, or cleans.

apply_gp3bayes_0_5_overlay <- function(
    project_root,
    overlay_root,
    metadata_root = file.path(dirname(normalizePath(overlay_root, mustWork = TRUE)), "metadata"),
    expected_base = "33136233203a2d7eb259df282cfb42957e34cb83",
    expected_version = "0.4.0.9000",
    target_version = "0.5.0.9000",
    expected_protected_files = 48L,
    patch_description = TRUE,
    patch_news = TRUE,
    patch_readme = FALSE) {

  fail <- function(...) stop(..., call. = FALSE)
  git <- function(args) {
    out <- system2("git", args = args, stdout = TRUE, stderr = TRUE)
    st <- attr(out, "status")
    if (!is.null(st) && st != 0L) fail("git ", paste(args, collapse = " "), " failed:\n", paste(out, collapse = "\n"))
    out
  }
  is_protected <- function(path) {
    grepl("^paper(/|$)", path) ||
      identical(path, "gp3bayes-paper-bundle.zip") ||
      identical(path, "gp3bayes.Rmd") ||
      identical(path, "refs.bib")
  }

  project_root <- normalizePath(project_root, mustWork = TRUE)
  overlay_root <- normalizePath(overlay_root, mustWork = TRUE)
  metadata_root <- normalizePath(metadata_root, mustWork = TRUE)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(project_root)

  if (!file.exists("DESCRIPTION")) fail("No DESCRIPTION found in project root.")
  desc <- read.dcf("DESCRIPTION")
  if (!identical(unname(desc[1L, "Package"]), "gp3bayes")) fail("Target project is not gp3bayes.")
  if (!identical(unname(desc[1L, "Version"]), expected_version)) {
    fail("Expected gp3bayes version ", expected_version, " before overlay, found ", unname(desc[1L, "Version"]), ".")
  }

  # 0.5 deliberately reuses the Bayesian dependency family already established
  # by gp3bayes. Refuse to proceed if a local checkout unexpectedly lacks the
  # packages required by the advanced translation/comparison layer instead of
  # silently rewriting DESCRIPTION dependency fields.
  dep_fields <- intersect(c("Depends", "Imports", "Suggests", "Enhances"), colnames(desc))
  dep_text <- paste(unname(desc[1L, dep_fields, drop = TRUE]), collapse = ",")
  declared_packages <- trimws(gsub("\\s*\\([^)]*\\)", "", unlist(strsplit(dep_text, ",", fixed = TRUE))))
  declared_packages <- declared_packages[nzchar(declared_packages)]
  required_declared <- c("brms", "loo", "posterior")
  optional_backend_declared <- c("rstan", "cmdstanr")
  missing_required <- setdiff(required_declared, declared_packages)
  missing_backends <- setdiff(optional_backend_declared, declared_packages)
  if (length(missing_required)) {
    fail(
      "Local DESCRIPTION is missing required 0.5 dependency declaration(s): ",
      paste(missing_required, collapse = ", "),
      ". Add them deliberately before applying the overlay."
    )
  }
  if (length(missing_backends) == length(optional_backend_declared)) {
    fail("Local DESCRIPTION declares neither `rstan` nor `cmdstanr`; at least one supported Stan backend must remain available.")
  }

  branch <- git(c("rev-parse", "--abbrev-ref", "HEAD"))
  head <- git(c("rev-parse", "HEAD"))
  if (identical(branch, "master")) {
    fail("Refusing to install the 0.5 overlay directly on `master`. Create a dedicated feature branch first.")
  }
  if (!identical(head, expected_base)) {
    fail("Expected the new feature branch to start exactly at merged 0.4 master ", expected_base, "; current HEAD is ", head, ".")
  }
  if (length(git(c("diff", "--cached", "--name-only")))) fail("Staging must be empty before applying the overlay.")
  if (length(git(c("diff", "--name-only")))) fail("Tracked working tree must be clean before applying the overlay.")

  untracked <- sort(git(c("ls-files", "--others", "--exclude-standard")))
  protected <- untracked[vapply(untracked, is_protected, logical(1L))]
  other <- setdiff(untracked, protected)
  if (length(protected) != expected_protected_files) {
    fail("Expected exactly ", expected_protected_files, " protected research files; observed ", length(protected), ".")
  }
  if (length(other)) {
    fail("Unexpected untracked files exist before overlay:\n", paste(other, collapse = "\n"))
  }
  protected_md5 <- tools::md5sum(protected)

  source_files <- list.files(overlay_root, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  source_files <- source_files[file.info(source_files)$isdir %in% FALSE]
  rel <- substring(source_files, nchar(overlay_root) + 2L)
  target_files <- file.path(project_root, rel)
  existing <- target_files[file.exists(target_files)]
  if (length(existing)) {
    rel_existing <- rel[file.exists(target_files)]
    fail(
      "The overlay is additive and refuses to overwrite existing files. Existing target(s):\n",
      paste(rel_existing, collapse = "\n")
    )
  }

  dirs <- unique(dirname(target_files))
  invisible(vapply(dirs, dir.create, logical(1L), recursive = TRUE, showWarnings = FALSE))
  copied <- file.copy(source_files, target_files, overwrite = FALSE, copy.mode = TRUE, copy.date = TRUE)
  if (!all(copied)) fail("At least one overlay file could not be copied.")

  if (patch_description) {
    x <- readLines("DESCRIPTION", warn = FALSE, encoding = "UTF-8")
    old <- paste0("Version: ", expected_version)
    new <- paste0("Version: ", target_version)
    hit <- which(x == old)
    if (length(hit) != 1L) fail("Could not patch DESCRIPTION Version exactly once.")
    x[hit] <- new
    marker <- "Version 0.5 adds governed robust and distributional dynamic pupillometry"
    if (!any(grepl(marker, x, fixed = TRUE))) {
      dstart <- grep("^Description:", x)
      if (length(dstart) != 1L) fail("Could not locate DESCRIPTION Description field.")
      j <- dstart + 1L
      while (j <= length(x) && grepl("^[[:space:]]", x[[j]])) j <- j + 1L
      sentence <- trimws(paste(readLines(file.path(metadata_root, "DESCRIPTION-0.5-sentence.txt"), warn = FALSE), collapse = " "))
      x <- append(x, paste0("    ", sentence), after = j - 1L)
    }
    writeLines(x, "DESCRIPTION", useBytes = TRUE)
  }

  if (patch_news) {
    frag <- readLines(file.path(metadata_root, "NEWS-0.5.0.9000.md"), warn = FALSE, encoding = "UTF-8")
    news <- if (file.exists("NEWS.md")) readLines("NEWS.md", warn = FALSE, encoding = "UTF-8") else character()
    if (!any(grepl("gp3bayes 0.5.0.9000", news, fixed = TRUE))) {
      writeLines(c(frag, "", news), "NEWS.md", useBytes = TRUE)
    }
  }

  if (patch_readme) {
    frag <- readLines(file.path(metadata_root, "README-0.5-fragment.md"), warn = FALSE, encoding = "UTF-8")
    for (f in c("README.Rmd", "README.md")) {
      if (!file.exists(f)) next
      x <- readLines(f, warn = FALSE, encoding = "UTF-8")
      if (!any(grepl("Development version 0.5.0.9000", x, fixed = TRUE))) {
        writeLines(c(x, "", frag), f, useBytes = TRUE)
      }
    }
  }

  protected_after <- sort(git(c("ls-files", "--others", "--exclude-standard")))
  protected_after <- protected_after[vapply(protected_after, is_protected, logical(1L))]
  if (!identical(sort(protected_after), sort(protected))) fail("Protected workspace file set changed during overlay application.")
  if (!identical(unname(tools::md5sum(protected_after)), unname(protected_md5))) fail("Protected workspace content changed during overlay application.")

  version_after <- unname(read.dcf("DESCRIPTION")[1L, "Version"])
  if (!identical(version_after, target_version)) fail("DESCRIPTION version patch did not persist.")

  cat(
    "\n============================================================\n",
    "gp3bayes 0.5 DEVELOPMENT OVERLAY APPLIED\n",
    "============================================================\n",
    "Branch: ", branch, "\n",
    "Base commit: ", expected_base, "\n",
    "Version: ", version_after, "\n",
    "Overlay files copied: ", length(source_files), "\n",
    "Protected research workspace: ", length(protected_after), " files PRESERVED\n",
    "Git mutation: NONE (no stage/commit/push/merge/tag/release/deploy)\n",
    "\nNext: devtools::document(), then source('tools/audit-advanced-pupillometry-0.5.R').\n",
    sep = ""
  )

  invisible(list(
    branch = branch,
    base = head,
    version = version_after,
    copied = rel,
    protected_md5 = protected_md5,
    pkgdown_fragment = file.path(metadata_root, "pkgdown-0.5-fragment.yml"),
    readme_fragment = file.path(metadata_root, "README-0.5-fragment.md")
  ))
}
