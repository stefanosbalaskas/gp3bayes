# API-DOC-CONTRACT-0.4: aliases follow the current development manifest

.gp3bayes_doc_manifest_path <- function(filename) {
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

.gp3bayes_rd_aliases <- function() {
  man_dir <- testthat::test_path("..", "..", "man")
  rd_files <- if (dir.exists(man_dir)) {
    list.files(man_dir, pattern = "\\.Rd$", full.names = TRUE)
  } else {
    character()
  }

  if (length(rd_files)) {
    alias_lines <- unlist(
      lapply(rd_files, function(path) {
        x <- readLines(path, warn = FALSE, encoding = "UTF-8")
        grep("^\\\\alias\\{.*\\}$", x, value = TRUE)
      }),
      use.names = FALSE
    )
    return(unique(sub(
      "^\\\\alias\\{(.*)\\}$", "\\1", alias_lines
    )))
  }

  anindex <- system.file("help", "AnIndex", package = "gp3bayes")
  if (!nzchar(anindex) || !file.exists(anindex)) {
    stop(
      "Could not locate source Rd files or installed help/AnIndex.",
      call. = FALSE
    )
  }
  x <- readLines(anindex, warn = FALSE, encoding = "UTF-8")
  x <- x[nzchar(trimws(x))]
  unique(sub("[[:space:]].*$", "", x))
}

testthat::test_that(
  "every current public export has an Rd alias",
  {
    cur <- utils::read.delim(
      .gp3bayes_doc_manifest_path("public-api-0.4.0.9000.tsv"),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    cur <- cur[order(cur$function_name), , drop = FALSE]

    exports <- sort(getNamespaceExports(asNamespace("gp3bayes")))
    testthat::expect_equal(length(exports), 370L)
    testthat::expect_identical(exports, cur$function_name)

    aliases <- .gp3bayes_rd_aliases()
    testthat::expect_gt(length(aliases), 0L)
    missing_aliases <- setdiff(exports, aliases)

    if (length(missing_aliases)) {
      testthat::fail(paste0(
        "Public exports missing Rd aliases: ",
        paste(missing_aliases, collapse = ", ")
      ))
    } else {
      testthat::pass()
    }
  }
)
