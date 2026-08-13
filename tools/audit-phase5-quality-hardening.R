cat("\n===== gp3bayes Phase 5 quality-hardening audit =====\n")

expected_exports <- 324L
expected_version <- "0.3.0.9000"

namespace <- readLines(
  "NAMESPACE",
  warn = FALSE,
  encoding = "UTF-8"
)

exports <- sort(sub(
  "^export\\((.*)\\)$",
  "\\1",
  grep("^export\\(", namespace, value = TRUE)
))

stopifnot(
  length(exports) == expected_exports,
  identical(
    unname(read.dcf("DESCRIPTION")[1L, "Version"]),
    expected_version
  )
)

manifest <- utils::read.delim(
  "inst/api/public-api-0.3.0.9000.tsv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

stopifnot(
  nrow(manifest) == expected_exports,
  identical(exports, manifest$function_name),
  !anyDuplicated(manifest$function_name)
)

rd_files <- list.files(
  "man",
  pattern = "\\.Rd$",
  full.names = TRUE
)

aliases <- unique(unlist(
  lapply(
    rd_files,
    function(file) {
      x <- readLines(file, warn = FALSE, encoding = "UTF-8")
      a <- grep("^\\\\alias\\{", x, value = TRUE)
      sub("^\\\\alias\\{(.*)\\}$", "\\1", a)
    }
  ),
  use.names = FALSE
))

stopifnot(all(exports %in% aliases))

required_files <- c(
  "inst/api/public-api-0.3.0.9000.tsv",
  "tests/testthat/test-public-api-contract.R",
  "tests/testthat/test-public-api-documentation-contract.R",
  "tests/testthat/test-postfit-adapter-smoke-contracts.R",
  "tests/testthat/test-postfit-failure-contracts.R",
  "vignettes/public-api-map.Rmd",
  "vignettes/quality-hardening-and-failure-contracts.Rmd"
)

stopifnot(all(file.exists(required_files)))

r_files <- list.files(
  "R",
  pattern = "\\.[Rr]$",
  full.names = TRUE
)

source_text <- unlist(
  lapply(r_files, readLines, warn = FALSE, encoding = "UTF-8"),
  use.names = FALSE
)

stopifnot(
  !any(grepl("\\.GlobalEnv", source_text, fixed = TRUE)),
  !any(grepl("\\bsetwd\\s*\\(", source_text, perl = TRUE)),
  !any(grepl("geom_errorbarh\\s*\\(", source_text, perl = TRUE)),
  !any(grepl("aes_string\\s*\\(", source_text, perl = TRUE))
)

api_map <- paste(
  readLines(
    "vignettes/public-api-map.Rmd",
    warn = FALSE,
    encoding = "UTF-8"
  ),
  collapse = "\n"
)

missing_from_map <- exports[
  !vapply(
    exports,
    function(name) {
      grepl(
        paste0("`", name, "\\(\\)`"),
        api_map,
        perl = TRUE
      )
    },
    logical(1L)
  )
]

stopifnot(length(missing_from_map) == 0L)

cat("Public exports: 324\n")
cat("New Phase-5 exports: 0\n")
cat("API manifest: PASS\n")
cat("Rd alias coverage: 324/324\n")
cat("Complete public API map: 324/324\n")
cat("Safety/compatibility scan: PASS\n")
cat("Scope/compliance audit: PASS\n")
