test_that("0.5 frozen public API preserves 0.4 and exact advanced additions", {
  manifest <- system.file("api", "public-api-0.5.0.9000.tsv", package = "gp3bayes")
  if (!nzchar(manifest)) {
    skip("Freeze the 0.5 API with tools/freeze-public-api-0.5.R before publication freeze.")
  }
  tab <- read.delim(manifest, stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(nrow(tab), 458L)
  expect_equal(sum(tab$inherited_from_0_4), 370L)
  expect_equal(sum(!tab$inherited_from_0_4), 88L)

  formal_is_missing <- function(f, a) {
    one <- f[a]
    template <- alist(.formal = )
    names(template) <- a
    identical(one, template)
  }

  signature <- function(nm) {
    fn <- getExportedValue("gp3bayes", nm)
    f <- formals(fn)
    if (!length(f)) return("")
    paste(vapply(names(f), function(a) {
      rhs <- if (formal_is_missing(f, a)) {
        "<MISSING>"
      } else {
        paste(deparse(f[[a]], width.cutoff = 500L), collapse = " ")
      }
      paste0(a, "=", rhs)
    }, character(1L)), collapse = "; ")
  }

  current <- vapply(tab$name, signature, character(1L))
  expect_identical(unname(current), unname(tab$formals))
})
