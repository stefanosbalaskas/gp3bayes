setwd(
  "C:/Users/Stefanos-PC/Documents/Rstudio/gp3bayes"
)

required_packages <- c(
  "pkgdown",
  "yaml"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing development packages: ",
    paste(
      missing_packages,
      collapse = ", "
    ),
    ".",
    call. = FALSE
  )
}

namespace <- readLines(
  "NAMESPACE",
  warn = FALSE,
  encoding = "UTF-8"
)

exports <- sub(
  "^export\\((.*)\\)$",
  "\\1",
  namespace[
    grepl(
      "^export\\(",
      namespace
    )
  ]
)

rd_files <- list.files(
  "man",
  pattern = "\\.Rd$",
  full.names = TRUE
)

aliases <- unique(
  unlist(
    lapply(
      rd_files,
      function(path) {
        lines <- readLines(
          path,
          warn = FALSE,
          encoding = "UTF-8"
        )
        sub(
          "^\\\\alias\\{(.*)\\}$",
          "\\1",
          lines[
            grepl(
              "^\\\\alias\\{",
              lines
            )
          ]
        )
      }
    ),
    use.names = FALSE
  )
)

config <- yaml::read_yaml(
  "_pkgdown.yml"
)

reference_topics <- unique(
  unlist(
    lapply(
      config$reference,
      function(group) {
        as.character(
          group$contents
        )
      }
    ),
    use.names = FALSE
  )
)

article_topics <- unique(
  unlist(
    lapply(
      config$articles,
      function(group) {
        as.character(
          group$contents
        )
      }
    ),
    use.names = FALSE
  )
)

expected_articles <- c(
  "binary-end-to-end",
  "duration-end-to-end",
  "posterior-diagnostics",
  "sensitivity-and-recovery",
  "optional-backend-installation"
)

vignette_files <- file.path(
  "vignettes",
  paste0(
    expected_articles,
    ".Rmd"
  )
)

description <- read.dcf(
  "DESCRIPTION"
)

suggests <- if (
  "Suggests" %in% colnames(description)
) {
  trimws(
    sub(
      "\\s*\\(.*$",
      "",
      unlist(
        strsplit(
          description[
            1L,
            "Suggests"
          ],
          ","
        )
      )
    )
  )
} else {
  character()
}

required_optional <- c(
  "bayesplot",
  "brms",
  "knitr",
  "posterior",
  "rmarkdown",
  "rstan",
  "testthat"
)

rows <- list(
  data.frame(
    check =
      "exports_have_rd_aliases",
    status =
      if (
        all(
          exports %in% aliases
        )
      ) {
        "pass"
      } else {
        "fail"
      },
    detail =
      paste(
        setdiff(
          exports,
          aliases
        ),
        collapse = ", "
      ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    check =
      "exports_in_pkgdown_reference",
    status =
      if (
        all(
          exports %in%
            reference_topics
        )
      ) {
        "pass"
      } else {
        "fail"
      },
    detail =
      paste(
        setdiff(
          exports,
          reference_topics
        ),
        collapse = ", "
      ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    check =
      "expected_vignettes_present",
    status =
      if (
        all(
          file.exists(
            vignette_files
          )
        )
      ) {
        "pass"
      } else {
        "fail"
      },
    detail =
      paste(
        vignette_files[
          !file.exists(
            vignette_files
          )
        ],
        collapse = ", "
      ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    check =
      "expected_articles_indexed",
    status =
      if (
        all(
          expected_articles %in%
            article_topics
        )
      ) {
        "pass"
      } else {
        "fail"
      },
    detail =
      paste(
        setdiff(
          expected_articles,
          article_topics
        ),
        collapse = ", "
      ),
    stringsAsFactors = FALSE
  ),
  data.frame(
    check =
      "optional_dependencies_declared",
    status =
      if (
        all(
          required_optional %in%
            suggests
        )
      ) {
        "pass"
      } else {
        "fail"
      },
    detail =
      paste(
        setdiff(
          required_optional,
          suggests
        ),
        collapse = ", "
      ),
    stringsAsFactors = FALSE
  )
)

pkgdown_result <- tryCatch(
  {
    pkgdown::check_pkgdown()
    NULL
  },
  error = function(error) {
    conditionMessage(error)
  }
)

rows[[length(rows) + 1L]] <-
  data.frame(
    check = "pkgdown_configuration",
    status =
      if (is.null(pkgdown_result)) {
        "pass"
      } else {
        "fail"
      },
    detail =
      if (is.null(pkgdown_result)) {
        ""
      } else {
        pkgdown_result
      },
    stringsAsFactors = FALSE
  )

if (dir.exists("docs")) {
  expected_reference_pages <- file.path(
    "docs",
    "reference",
    paste0(
      exports,
      ".html"
    )
  )
  expected_article_pages <- file.path(
    "docs",
    "articles",
    paste0(
      expected_articles,
      ".html"
    )
  )

  rows[[length(rows) + 1L]] <-
    data.frame(
      check =
        "built_reference_pages",
      status =
        if (
          all(
            file.exists(
              expected_reference_pages
            )
          )
        ) {
          "pass"
        } else {
          "fail"
        },
      detail =
        paste(
          expected_reference_pages[
            !file.exists(
              expected_reference_pages
            )
          ],
          collapse = ", "
        ),
      stringsAsFactors = FALSE
    )

  rows[[length(rows) + 1L]] <-
    data.frame(
      check =
        "built_article_pages",
      status =
        if (
          all(
            file.exists(
              expected_article_pages
            )
          )
        ) {
          "pass"
        } else {
          "fail"
        },
      detail =
        paste(
          expected_article_pages[
            !file.exists(
              expected_article_pages
            )
          ],
          collapse = ", "
        ),
      stringsAsFactors = FALSE
    )
}

audit <- do.call(
  rbind,
  rows
)
rownames(audit) <- NULL

print(
  audit,
  row.names = FALSE
)

output <- file.path(
  tempdir(),
  "gp3bayes-website-audit.csv"
)

utils::write.csv(
  audit,
  output,
  row.names = FALSE,
  na = ""
)

cat(
  "Website audit written outside the repository:\n",
  normalizePath(
    output,
    winslash = "/",
    mustWork = TRUE
  ),
  "\n",
  sep = ""
)

if (any(
  audit$status == "fail"
)) {
  stop(
    "The package website audit contains one or more failures.",
    call. = FALSE
  )
}

cat(
  "Package website reference and article audit passed.\n"
)
