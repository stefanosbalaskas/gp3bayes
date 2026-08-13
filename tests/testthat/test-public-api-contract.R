test_that("frozen public API names and formal arguments remain stable", {
  manifest_path <- system.file(
    "api",
    "public-api-0.3.0.9000.tsv",
    package = "gp3bayes"
  )
  expect_true(nzchar(manifest_path))
  manifest <- utils::read.delim(
    manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  actual_exports <- sort(getNamespaceExports("gp3bayes"))
  expect_identical(actual_exports, manifest$function_name)

  actual_formals <- vapply(
    actual_exports,
    function(name) {
      fn <- getExportedValue("gp3bayes", name)
      expect_true(is.function(fn))
      paste(names(formals(fn)), collapse = "|")
    },
    character(1L)
  )

  expect_identical(
    unname(actual_formals),
    manifest$formal_names
  )
})

test_that("frozen API contains no duplicate public names", {
  manifest_path <- system.file(
    "api",
    "public-api-0.3.0.9000.tsv",
    package = "gp3bayes"
  )
  manifest <- utils::read.delim(
    manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_equal(nrow(manifest), 324L)
  expect_false(anyDuplicated(manifest$function_name) > 0L)
})
