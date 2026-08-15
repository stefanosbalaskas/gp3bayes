# API-CONTRACT-0.4: historical 0.3 compatibility + current 0.4 exactness

.gp3bayes_manifest_path <- function(filename) {
  source_path <- testthat::test_path(
    "..", "..", "inst", "api", filename
  )
  installed_path <- system.file(
    "api", filename, package = "gp3bayes"
  )
  candidates <- c(source_path, installed_path)
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) {
    stop("Could not locate API manifest: ", filename, call. = FALSE)
  }
  candidates[[1L]]
}

testthat::test_that(
  "historical 0.3 API remains compatible and current 0.4 API is exact",
  {
    old <- utils::read.delim(
      .gp3bayes_manifest_path("public-api-0.3.0.9000.tsv"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    cur <- utils::read.delim(
      .gp3bayes_manifest_path("public-api-0.4.0.9000.tsv"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    old <- old[order(old$function_name), , drop = FALSE]
    cur <- cur[order(cur$function_name), , drop = FALSE]

    testthat::expect_equal(nrow(old), 324L)
    testthat::expect_equal(nrow(cur), 370L)

    idx <- match(old$function_name, cur$function_name)
    testthat::expect_false(anyNA(idx))
    testthat::expect_identical(
      old$formal_names, cur$formal_names[idx]
    )

    ns <- asNamespace("gp3bayes")
    exports <- sort(getNamespaceExports(ns))
    testthat::expect_identical(exports, cur$function_name)

    actual_formals <- vapply(
      exports,
      function(name) {
        f <- get(name, envir = ns, inherits = FALSE)
        testthat::expect_true(is.function(f), info = name)
        paste(names(formals(f)), collapse = "|")
      },
      character(1L)
    )
    testthat::expect_identical(
      unname(actual_formals), cur$formal_names
    )
  }
)
