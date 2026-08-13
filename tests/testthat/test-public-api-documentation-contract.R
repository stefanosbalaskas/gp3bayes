test_that("every public export has an installed or source Rd alias", {

  exports <- sort(getNamespaceExports("gp3bayes"))

  expect_equal(
    length(exports),
    324L
  )

  # Locate a source package root when tests are running directly
  # from the repository. R CMD check may instead execute against
  # the installed package, where source man/*.Rd files are absent.
  candidates <- c(
    ".",
    "..",
    "../..",
    "../../..",
    "../../../.."
  )

  candidates <- unique(
    normalizePath(
      candidates,
      winslash = "/",
      mustWork = FALSE
    )
  )

  is_source_root <- vapply(
    candidates,
    function(path) {
      file.exists(file.path(path, "NAMESPACE")) &&
        dir.exists(file.path(path, "man"))
    },
    logical(1L)
  )

  if (any(is_source_root)) {

    source_root <- candidates[which(is_source_root)[1L]]

    rd_files <- list.files(
      file.path(source_root, "man"),
      pattern = "\\.Rd$",
      full.names = TRUE
    )

    expect_gt(
      length(rd_files),
      0L
    )

    aliases <- unique(
      unlist(
        lapply(
          rd_files,
          function(file) {

            x <- readLines(
              file,
              warn = FALSE,
              encoding = "UTF-8"
            )

            a <- grep(
              "^\\\\alias\\{",
              x,
              value = TRUE
            )

            sub(
              "^\\\\alias\\{(.*)\\}$",
              "\\1",
              a
            )
          }
        ),
        use.names = FALSE
      )
    )

  } else {

    # Installed packages store the help alias index here.
    alias_path <- system.file(
      "help",
      "aliases.rds",
      package = "gp3bayes"
    )

    expect_true(
      nzchar(alias_path)
    )

    expect_true(
      file.exists(alias_path)
    )

    alias_index <- readRDS(
      alias_path
    )

    aliases <- names(
      alias_index
    )
  }

  aliases <- unique(
    aliases[
      !is.na(aliases) &
        nzchar(aliases)
    ]
  )

  expect_gt(
    length(aliases),
    0L
  )

  missing_aliases <- setdiff(
    exports,
    aliases
  )

  expect_identical(
    missing_aliases,
    character()
  )
})
