# Freeze gp3bayes 0.5 public API after roxygen documentation.
# This writes inst/api/public-api-0.5.0.9000.tsv only; it does not mutate Git.

freeze_gp3bayes_0_5_public_api <- function(project_root = ".", overwrite = FALSE) {
  project_root <- normalizePath(project_root, mustWork = TRUE)
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(project_root)
  if (!requireNamespace("pkgload", quietly = TRUE)) stop("Install pkgload/devtools first.", call. = FALSE)
  d <- read.dcf("DESCRIPTION")
  stopifnot(identical(unname(d[1L, "Package"]), "gp3bayes"), identical(unname(d[1L, "Version"]), "0.5.0.9000"))
  if (!file.exists("NAMESPACE")) stop("Run devtools::document() before freezing the API.", call. = FALSE)

  ns <- readLines("NAMESPACE", warn = FALSE)
  lines <- grep("^export\\(", ns, value = TRUE)
  exports <- sub("^export\\((.*)\\)$", "\\1", lines)
  exports <- gsub("^[\"']|[\"']$", "", exports)
  exports <- sort(unique(exports))
  if (length(exports) != 458L) stop("Expected exactly 458 regular exports before freezing 0.5; found ", length(exports), ".", call. = FALSE)

  old_api <- read.delim("inst/api/public-api-0.4.0.9000.tsv", stringsAsFactors = FALSE, check.names = FALSE)
  ncol_name <- intersect(c("name", "export", "function", "symbol"), names(old_api))
  name_col <- if (length(ncol_name)) ncol_name[[1L]] else names(old_api)[[1L]]
  old_names <- sort(unique(as.character(old_api[[name_col]])))
  if (length(old_names) != 370L || !all(old_names %in% exports)) stop("Frozen 0.4 API is not an exact 370-name subset of 0.5.", call. = FALSE)

  new_expected <- sort(read.delim("tools/public-api-0.5-new-exports.tsv", stringsAsFactors = FALSE)$name)
  if (!identical(sort(setdiff(exports, old_names)), new_expected)) stop("0.5 export difference does not equal the reviewed new-export list.", call. = FALSE)

  pkgload::load_all(".", quiet = TRUE, export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
  nsenv <- asNamespace("gp3bayes")
  formal_is_missing <- function(f, a) {
    one <- f[a]
    template <- alist(.formal = )
    names(template) <- a
    identical(one, template)
  }
  signature <- function(nm) {
    fn <- get(nm, envir = nsenv, inherits = FALSE)
    if (!is.function(fn)) return("<non-function>")
    f <- formals(fn)
    if (!length(f)) return("")
    paste(
      vapply(
        names(f),
        function(a) {
          rhs <- if (formal_is_missing(f, a)) {
            "<MISSING>"
          } else {
            paste(
              deparse(
                f[[a]],
                width.cutoff = 500L
              ),
              collapse = " "
            )
          }
          paste0(a, "=", rhs)
        },
        character(1L)
      ),
      collapse = "; "
    )
  }
  tab <- data.frame(
    name = exports,
    formals = vapply(exports, signature, character(1L)),
    inherited_from_0_4 = exports %in% old_names,
    stringsAsFactors = FALSE
  )
  path <- "inst/api/public-api-0.5.0.9000.tsv"
  if (file.exists(path) && !overwrite) stop(path, " already exists; use overwrite=TRUE only after deliberate API review.", call. = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write.table(tab, path, sep = "\t", quote = TRUE, qmethod = "double", row.names = FALSE, na = "")
  cat("Frozen 0.5 API manifest: ", path, "\n", sep = "")
  cat("Regular exports: 458 = 370 inherited + 88 new\n")
  invisible(tab)
}
