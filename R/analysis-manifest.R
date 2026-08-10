# Analysis manifest and reproducibility layer for gp3bayes 0.2.0

.gp3m_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.gp3m_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .gp3m_stop("`", name, "` must be TRUE or FALSE.")
  }
  x
}

.gp3m_string <- function(x, name, optional = FALSE) {
  if (optional && is.null(x)) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .gp3m_stop("`", name, "` must be one non-empty character value.")
  }
  x
}

.gp3m_character <- function(x, name) {
  if (!is.character(x) || anyNA(x)) {
    .gp3m_stop("`", name, "` must be a character vector without missing values.")
  }
  x
}

.gp3m_hash_object <- function(x) {
  path <- tempfile(pattern = "gp3bayes-hash-", fileext = ".rds")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  saveRDS(x, path, version = 3, compress = FALSE)
  unname(tools::md5sum(path))
}

.gp3m_safe_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(package))
}

.gp3m_package_versions <- function() {
  packages <- c(
    "gp3bayes", "brms", "rstan", "cmdstanr", "posterior", "loo",
    "priorsense", "detectseparation", "SBC", "withr"
  )
  values <- vapply(packages, .gp3m_safe_version, character(1L))
  values[!is.na(values)]
}

.gp3m_data_fingerprint <- function(data) {
  if (is.null(data)) {
    return(list(
      available = FALSE,
      hash = NA_character_,
      n_rows = NA_integer_,
      n_columns = NA_integer_,
      columns = character(),
      classes = character(),
      complete_rows = NA_integer_,
      hash_method = "MD5-of-RDS-v3",
      row_order_sensitive = TRUE
    ))
  }
  if (!is.data.frame(data)) {
    .gp3m_stop("`data` must be a data frame when supplied.")
  }
  canonical <- data
  rownames(canonical) <- NULL
  list(
    available = TRUE,
    hash = .gp3m_hash_object(canonical),
    n_rows = nrow(canonical),
    n_columns = ncol(canonical),
    columns = names(canonical),
    classes = vapply(
      canonical,
      function(column) paste(class(column), collapse = "/"),
      character(1L)
    ),
    complete_rows = sum(stats::complete.cases(canonical)),
    hash_method = "MD5-of-RDS-v3",
    row_order_sensitive = TRUE
  )
}

.gp3m_contract_signature <- function(contract) {
  if (is.null(contract)) return(NULL)
  if (!inherits(contract, "gp3bayes_model_contract")) {
    .gp3m_stop("The manifest contract must inherit from `gp3bayes_model_contract`.")
  }
  list(
    contract_version = contract$contract_version,
    family = contract$family,
    model_family = contract$model_family,
    mappings = contract$mappings,
    predictors = contract$predictors,
    interaction = contract$interaction,
    random_slope = contract$random_slope,
    outcome_unit = contract$outcome_unit,
    likelihood = contract$likelihood,
    link = contract$link,
    hash = .gp3m_hash_object(list(
      contract$family,
      contract$model_family,
      contract$mappings,
      contract$predictors,
      contract$interaction,
      contract$random_slope,
      contract$outcome_unit,
      contract$likelihood,
      contract$link
    ))
  )
}

.gp3m_specification_signature <- function(specification) {
  if (is.null(specification)) return(NULL)
  if (!inherits(specification, "gp3bayes_model_specification")) {
    .gp3m_stop("`specification` must inherit from `gp3bayes_model_specification`.")
  }
  priors <- specification$priors
  prior_signature <- if (is.data.frame(priors)) {
    priors
  } else if (is.list(priors)) {
    priors
  } else {
    as.character(priors)
  }
  signature <- list(
    family = specification$family,
    model_family = specification$model_family,
    formula_text = specification$formula_text %||%
      paste(deparse(specification$formula), collapse = " "),
    priors = prior_signature,
    advanced_priors = specification$advanced_priors %||% NULL,
    outcome_unit = specification$outcome_unit %||% NULL
  )
  signature$hash <- .gp3m_hash_object(signature)
  signature
}

.gp3m_transformation_signature <- function(prepared) {
  if (is.null(prepared)) return(NULL)
  transformations <- prepared$transformations %||% NULL
  list(
    preparation_version = prepared$preparation_version %||% NA_character_,
    transformations = transformations,
    decision_log = prepared$decision_log %||% NULL,
    hash = .gp3m_hash_object(list(transformations, prepared$decision_log %||% NULL))
  )
}

.gp3m_sampling_signature <- function(fit) {
  if (is.null(fit)) return(NULL)
  list(
    backend_interface = fit$backend_interface %||% NA_character_,
    sampling_backend = fit$sampling_backend %||% NA_character_,
    algorithm = fit$algorithm %||% NA_character_,
    sampling = fit$sampling %||% NULL,
    package_versions = fit$package_versions %||% NULL
  )
}

#' Create an Analysis Manifest
#'
#' Records the declared analysis contract, transformations, estimands,
#' sensitivity plan, random seed, data fingerprint, package versions, and
#' optional sampling specification in one backend-independent provenance
#' object. The manifest stores a data fingerprint rather than a duplicate copy
#' of the analysis data.
#'
#' @param specification Optional approved gp3bayes model specification.
#' @param fit Optional gp3bayes fit. When supplied, the specification and
#'   prepared data are derived from the fit unless explicitly supplied.
#' @param data Optional analysis data frame. When omitted it is derived from
#'   the specification or fit where possible.
#' @param estimands Character vector or structured list describing the
#'   prespecified estimands.
#' @param sensitivity_plan Optional sensitivity-plan object or list.
#' @param seed Optional non-negative integer seed. When omitted and `fit` is
#'   supplied, the recorded fitting seed is used.
#' @param label Optional human-readable analysis label.
#' @param notes Optional character notes.
#'
#' @return A `gp3bayes_analysis_manifest`.
#' @examples
#' simulation <- simulate_hierarchical_binary_data(
#'   n_participants = 8,
#'   trials_per_participant = 6,
#'   n_items = 4,
#'   random_slope_sd = 0,
#'   seed = 2026
#' )
#' contract <- create_model_contract(
#'   family = "binary",
#'   outcome_col = "selected",
#'   participant_col = "participant_id",
#'   item_col = "item_id",
#'   trial_col = "trial_id",
#'   condition_col = "condition"
#' )
#' prepared <- prepare_hierarchical_binary_data(
#'   simulation$data,
#'   contract,
#'   condition_levels = c("control", "treatment")
#' )
#' specification <- specify_binary_model(prepared, baseline = 0.35)
#' manifest <- create_analysis_manifest(
#'   specification = specification,
#'   estimands = "standardized_probability_contrast",
#'   seed = 2026
#' )
#' manifest
#' @export
create_analysis_manifest <- function(
  specification = NULL,
  fit = NULL,
  data = NULL,
  estimands = character(),
  sensitivity_plan = NULL,
  seed = NULL,
  label = NULL,
  notes = character()
) {
  if (!is.null(fit)) {
    if (!inherits(fit, "gp3bayes_fit")) {
      .gp3m_stop("`fit` must inherit from `gp3bayes_fit`.")
    }
    if (is.null(specification)) specification <- fit$specification
  }
  if (!is.null(specification) &&
      !inherits(specification, "gp3bayes_model_specification")) {
    .gp3m_stop("`specification` must inherit from `gp3bayes_model_specification`.")
  }

  prepared <- if (!is.null(specification)) specification$prepared else NULL
  contract <- if (!is.null(specification)) specification$contract else NULL
  if (is.null(data) && !is.null(prepared)) data <- prepared$data

  if (is.character(estimands)) {
    estimands <- .gp3m_character(estimands, "estimands")
  } else if (!is.list(estimands)) {
    .gp3m_stop("`estimands` must be a character vector or list.")
  }
  notes <- .gp3m_character(notes, "notes")
  label <- .gp3m_string(label, "label", optional = TRUE)

  if (is.null(seed) && !is.null(fit) && !is.null(fit$sampling$seed)) {
    seed <- fit$sampling$seed
  }
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
        !is.finite(seed) || seed < 0 || seed != floor(seed)) {
      .gp3m_stop("`seed` must be NULL or one non-negative integer.")
    }
    seed <- as.integer(seed)
  }

  data_fingerprint <- .gp3m_data_fingerprint(data)
  contract_signature <- .gp3m_contract_signature(contract)
  specification_signature <- .gp3m_specification_signature(specification)
  transformation_signature <- .gp3m_transformation_signature(prepared)

  family <- if (!is.null(specification)) specification$family else NA_character_
  result <- list(
    manifest_version = "0.2",
    fingerprint_method = "MD5-of-RDS-v3",
    fingerprint_security = "change-detection only; not a cryptographic authenticity proof",
    created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
    label = label,
    family = family,
    model_family = specification$model_family %||% NA_character_,
    contract = contract_signature,
    specification = specification_signature,
    transformations = transformation_signature,
    data = data_fingerprint,
    estimands = estimands,
    sensitivity_plan = sensitivity_plan,
    seed = seed,
    sampling = .gp3m_sampling_signature(fit),
    package_versions = .gp3m_package_versions(),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    notes = notes,
    frozen = FALSE,
    manifest_hash = NA_character_,
    adequacy_established = FALSE,
    causal_identification_established = FALSE
  )
  structure(result, class = "gp3bayes_analysis_manifest")
}

#' Validate an Analysis Manifest
#'
#' @param manifest A `gp3bayes_analysis_manifest`.
#' @param strict Whether failures should raise an error.
#' @return A `gp3bayes_manifest_validation`.
#' @export
validate_analysis_manifest <- function(manifest, strict = FALSE) {
  strict <- .gp3m_flag(strict, "strict")
  required <- c(
    "manifest_version", "family", "specification", "data", "estimands",
    "package_versions", "frozen", "manifest_hash"
  )
  missing <- if (is.list(manifest)) setdiff(required, names(manifest)) else required
  rows <- list(
    data.frame(
      check = "manifest_class",
      status = if (inherits(manifest, "gp3bayes_analysis_manifest")) "pass" else "fail",
      detail = paste(class(manifest), collapse = ", "),
      stringsAsFactors = FALSE
    ),
    data.frame(
      check = "required_fields",
      status = if (length(missing)) "fail" else "pass",
      detail = if (length(missing)) paste(missing, collapse = ", ") else "complete",
      stringsAsFactors = FALSE
    )
  )

  if (is.list(manifest) && !is.null(manifest$data)) {
    hash_ok <- !isTRUE(manifest$data$available) ||
      (is.character(manifest$data$hash) && length(manifest$data$hash) == 1L &&
         !is.na(manifest$data$hash) && nzchar(manifest$data$hash))
    rows[[length(rows) + 1L]] <- data.frame(
      check = "data_fingerprint",
      status = if (hash_ok) "pass" else "fail",
      detail = manifest$data$hash %||% "missing",
      stringsAsFactors = FALSE
    )
  }

  if (is.list(manifest) && !is.na(manifest$family %||% NA_character_)) {
    family_ok <- manifest$family %in% c("binary", "duration")
    rows[[length(rows) + 1L]] <- data.frame(
      check = "approved_family",
      status = if (family_ok) "pass" else "fail",
      detail = manifest$family,
      stringsAsFactors = FALSE
    )
  }

  table <- do.call(rbind, rows)
  status <- if (any(table$status == "fail")) "fail" else "pass"
  result <- structure(
    list(status = status, checks = table),
    class = "gp3bayes_manifest_validation"
  )
  if (strict && identical(status, "fail")) {
    .gp3m_stop(
      "Manifest validation failed: ",
      paste(table$check[table$status == "fail"], collapse = ", "),
      "."
    )
  }
  result
}

#' Freeze an Analysis Manifest
#'
#' Computes a deterministic hash over analysis-defining fields. When `file` is
#' supplied the frozen manifest is written explicitly to that path. No file is
#' written when `file = NULL`.
#'
#' @param manifest A valid analysis manifest.
#' @param file Optional explicit `.rds` output path.
#' @param overwrite Whether an existing explicit output file may be replaced.
#' @return A frozen `gp3bayes_analysis_manifest`.
#' @export
freeze_analysis_manifest <- function(manifest, file = NULL, overwrite = FALSE) {
  validate_analysis_manifest(manifest, strict = TRUE)
  overwrite <- .gp3m_flag(overwrite, "overwrite")
  canonical <- manifest[c(
    "manifest_version", "fingerprint_method", "label", "family", "model_family", "contract",
    "specification", "transformations", "data", "estimands",
    "sensitivity_plan", "seed", "sampling", "package_versions", "r_version",
    "platform", "notes"
  )]
  manifest$manifest_hash <- .gp3m_hash_object(canonical)
  manifest$frozen <- TRUE
  manifest$frozen_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  if (!is.null(file)) {
    file <- .gp3m_string(file, "file")
    if (file.exists(file) && !overwrite) {
      .gp3m_stop("`file` already exists. Set `overwrite = TRUE` to replace it.")
    }
    parent <- dirname(file)
    if (!dir.exists(parent)) {
      .gp3m_stop("The parent directory for `file` does not exist.")
    }
    saveRDS(manifest, file = file, version = 3)
  }
  manifest
}

#' Read a Frozen Analysis Manifest
#'
#' @param file Explicit `.rds` manifest path.
#' @return A validated `gp3bayes_analysis_manifest`.
#' @export
read_analysis_manifest <- function(file) {
  file <- .gp3m_string(file, "file")
  if (!file.exists(file)) .gp3m_stop("Manifest file does not exist.")
  manifest <- readRDS(file)
  validate_analysis_manifest(manifest, strict = TRUE)
  manifest
}

.gp3m_compare_value <- function(component, left, right) {
  identical_value <- identical(left, right)
  data.frame(
    component = component,
    identical = identical_value,
    left = paste(utils::capture.output(utils::str(left, give.attr = FALSE)), collapse = " "),
    right = paste(utils::capture.output(utils::str(right, give.attr = FALSE)), collapse = " "),
    stringsAsFactors = FALSE
  )
}

#' Compare Analysis Manifests
#'
#' Compares analysis-defining fields without interpreting any difference as
#' automatically problematic.
#'
#' @param x,y Analysis manifests.
#' @return A `gp3bayes_manifest_comparison`.
#' @export
compare_analysis_manifests <- function(x, y) {
  validate_analysis_manifest(x, strict = TRUE)
  validate_analysis_manifest(y, strict = TRUE)
  components <- list(
    family = list(x$family, y$family),
    contract_hash = list(x$contract$hash %||% NA_character_, y$contract$hash %||% NA_character_),
    specification_hash = list(
      x$specification$hash %||% NA_character_,
      y$specification$hash %||% NA_character_
    ),
    transformation_hash = list(
      x$transformations$hash %||% NA_character_,
      y$transformations$hash %||% NA_character_
    ),
    data_hash = list(x$data$hash %||% NA_character_, y$data$hash %||% NA_character_),
    estimands = list(x$estimands, y$estimands),
    sensitivity_plan = list(x$sensitivity_plan, y$sensitivity_plan),
    seed = list(x$seed, y$seed),
    sampling = list(x$sampling, y$sampling),
    package_versions = list(x$package_versions, y$package_versions)
  )
  table <- do.call(
    rbind,
    lapply(names(components), function(name) {
      .gp3m_compare_value(name, components[[name]][[1L]], components[[name]][[2L]])
    })
  )
  changed <- table$component[!table$identical]
  structure(
    list(
      comparison_version = "0.2",
      identical = length(changed) == 0L,
      changed_components = changed,
      table = table,
      left = x,
      right = y,
      interpretation = paste(
        "Differences are provenance differences only.",
        "They are not automatic evidence that either analysis is invalid."
      )
    ),
    class = "gp3bayes_manifest_comparison"
  )
}

#' Summarise Manifest Components
#'
#' @param manifest An analysis manifest.
#' @return A data frame of key provenance components.
#' @export
analysis_manifest_table <- function(manifest) {
  validate_analysis_manifest(manifest, strict = TRUE)
  data.frame(
    component = c(
      "family", "model_family", "data_hash", "contract_hash",
      "specification_hash", "transformation_hash", "seed", "backend",
      "frozen", "manifest_hash"
    ),
    value = c(
      manifest$family,
      manifest$model_family,
      manifest$data$hash %||% NA_character_,
      manifest$contract$hash %||% NA_character_,
      manifest$specification$hash %||% NA_character_,
      manifest$transformations$hash %||% NA_character_,
      as.character(manifest$seed %||% NA_integer_),
      manifest$sampling$sampling_backend %||% NA_character_,
      as.character(isTRUE(manifest$frozen)),
      manifest$manifest_hash %||% NA_character_
    ),
    stringsAsFactors = FALSE
  )
}

#' Write a Reproducibility Report
#'
#' Writes a conservative Markdown provenance report to an explicit path.
#'
#' @param manifest An analysis manifest.
#' @param file Explicit Markdown output path.
#' @param overwrite Whether an existing file may be replaced.
#' @return The normalized output path, invisibly.
#' @export
write_reproducibility_report <- function(manifest, file, overwrite = FALSE) {
  validate_analysis_manifest(manifest, strict = TRUE)
  file <- .gp3m_string(file, "file")
  overwrite <- .gp3m_flag(overwrite, "overwrite")
  if (file.exists(file) && !overwrite) {
    .gp3m_stop("`file` already exists. Set `overwrite = TRUE` to replace it.")
  }
  parent <- dirname(file)
  if (!dir.exists(parent)) .gp3m_stop("The report parent directory does not exist.")

  versions <- if (length(manifest$package_versions)) {
    paste0(
      "- `", names(manifest$package_versions), "`: ",
      unname(manifest$package_versions)
    )
  } else {
    "- No optional package versions recorded."
  }
  estimands <- if (length(manifest$estimands)) {
    paste0("- ", vapply(manifest$estimands, function(x) paste(x, collapse = ", "), character(1L)))
  } else {
    "- None recorded."
  }
  lines <- c(
    "# gp3bayes reproducibility report",
    "",
    paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    "",
    "## Analysis identity",
    "",
    paste0("- Label: ", manifest$label %||% "not specified"),
    paste0("- Family: ", manifest$family),
    paste0("- Model family: ", manifest$model_family),
    paste0("- Seed: ", manifest$seed %||% "not recorded"),
    paste0("- Frozen: ", isTRUE(manifest$frozen)),
    paste0("- Manifest hash: ", manifest$manifest_hash %||% "not frozen"),
    "",
    "## Data fingerprint",
    "",
    paste0("- Rows: ", manifest$data$n_rows),
    paste0("- Columns: ", manifest$data$n_columns),
    paste0("- Complete rows: ", manifest$data$complete_rows),
    paste0("- Data hash: `", manifest$data$hash, "`"),
    paste0("- Fingerprint method: ", manifest$data$hash_method %||% manifest$fingerprint_method),
    paste0("- Row-order sensitive: ", isTRUE(manifest$data$row_order_sensitive)),
    "- Fingerprints are change-detection identifiers, not cryptographic authenticity proofs.",
    "",
    "## Specification",
    "",
    paste0("- Formula: `", manifest$specification$formula_text %||% "not recorded", "`"),
    paste0("- Contract hash: `", manifest$contract$hash %||% "not recorded", "`"),
    paste0("- Specification hash: `", manifest$specification$hash %||% "not recorded", "`"),
    paste0("- Transformation hash: `", manifest$transformations$hash %||% "not recorded", "`"),
    "",
    "## Prespecified estimands",
    "",
    estimands,
    "",
    "## Software versions",
    "",
    versions,
    "",
    "## Interpretation boundary",
    "",
    paste(
      "This report records computational provenance. It does not establish",
      "model adequacy, robustness, causal identification, or substantive validity."
    )
  )
  writeLines(lines, con = file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

#' @export
print.gp3bayes_analysis_manifest <- function(x, ...) {
  cat("<gp3bayes_analysis_manifest>\n")
  cat("  Version: ", x$manifest_version, "\n", sep = "")
  if (!is.null(x$label)) cat("  Label: ", x$label, "\n", sep = "")
  cat("  Family: ", x$family, "\n", sep = "")
  cat("  Data: ", x$data$n_rows, " x ", x$data$n_columns, "\n", sep = "")
  cat("  Data hash: ", x$data$hash, "\n", sep = "")
  cat("  Frozen: ", isTRUE(x$frozen), "\n", sep = "")
  if (isTRUE(x$frozen)) cat("  Manifest hash: ", x$manifest_hash, "\n", sep = "")
  invisible(x)
}

#' @export
plot.gp3bayes_analysis_manifest <- function(x, ...) {
  table <- analysis_manifest_table(x)
  present <- !is.na(table$value) & nzchar(table$value) &
    !table$value %in% c("FALSE", "not recorded")
  graphics::barplot(
    as.integer(present),
    names.arg = table$component,
    las = 2,
    ylim = c(0, 1),
    ylab = "Recorded (0/1)",
    main = "Analysis manifest coverage",
    ...
  )
  invisible(x)
}

#' @export
print.gp3bayes_manifest_validation <- function(x, ...) {
  cat("<gp3bayes_manifest_validation>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  print.data.frame(x$checks, row.names = FALSE)
  invisible(x)
}

#' @export
print.gp3bayes_manifest_comparison <- function(x, ...) {
  cat("<gp3bayes_manifest_comparison>\n")
  cat("  Identical: ", x$identical, "\n", sep = "")
  if (length(x$changed_components)) {
    cat("  Changed: ", paste(x$changed_components, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' @export
plot.gp3bayes_manifest_comparison <- function(x, ...) {
  values <- ifelse(x$table$identical, 0, 1)
  graphics::barplot(
    values,
    names.arg = x$table$component,
    las = 2,
    ylim = c(0, 1),
    ylab = "Changed (0/1)",
    main = "Analysis manifest differences",
    ...
  )
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_analysis_manifest <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(
    analysis_manifest_table(x),
    row.names = row.names,
    optional = optional,
    ...
  )
}

#' @export
as.data.frame.gp3bayes_manifest_comparison <- function(
  x, row.names = NULL, optional = FALSE, ...
) {
  as.data.frame(x$table, row.names = row.names, optional = optional, ...)
}
