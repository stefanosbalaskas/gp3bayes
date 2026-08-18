# Governance, distribution declarations, sensitivity scenarios, and model cards.

#' Declare an advanced pupil observation distribution
#'
#' @param family Gaussian or Student-t.
#' @param residual_scale Constant, condition-, time-, or condition-by-time scale.
#' @return A `gp3bayes_pupil_distribution_spec` object.
#' @export
specify_pupil_distribution <- function(
    family = c("gaussian", "student"),
    residual_scale = c("constant", "condition", "time", "condition_time")) {
  family <- match.arg(family)
  residual_scale <- match.arg(residual_scale)
  structure(
    list(
      family = family,
      residual_scale = residual_scale,
      interpretation = if (family == "student") {
        "Student-t provides a robust observation model; it does not identify individual observations as invalid outliers."
      } else {
        "Gaussian observation model with explicitly declared residual-scale structure."
      }
    ),
    class = "gp3bayes_pupil_distribution_spec"
  )
}

#' Tabulate a pupil distribution declaration or advanced specification
#'
#' @param x A distribution declaration, advanced specification, or advanced fit.
#' @return A one-row data frame.
#' @export
pupil_distribution_table <- function(x) {
  if (inherits(x, "gp3bayes_pupil_advanced_fit")) x <- x$specification
  if (inherits(x, "gp3bayes_pupil_advanced_specification")) {
    return(data.frame(
      family = x$family,
      residual_scale = x$residual_scale,
      robust = x$family == "student",
      distributional = x$residual_scale != "constant",
      stringsAsFactors = FALSE
    ))
  }
  if (!inherits(x, "gp3bayes_pupil_distribution_spec")) stop("Expected a pupil distribution declaration or advanced specification.", call. = FALSE)
  data.frame(
    family = x$family,
    residual_scale = x$residual_scale,
    robust = x$family == "student",
    distributional = x$residual_scale != "constant",
    stringsAsFactors = FALSE
  )
}

#' Report governed compatibility rules for 0.5 advanced models
#'
#' @return A data frame documenting combinations that are supported, reviewed,
#'   or deliberately blocked in the 0.5 governed interface.
#' @export
pupil_advanced_compatibility_table <- function() {
  data.frame(
    feature_a = c(
      "Student-t", "Student-t", "Gaussian", "Gaussian", "GP", "GP",
      "missing response model", "measurement-error predictors", "binocular", "response-shape"
    ),
    feature_b = c(
      "ARMA", "distributional sigma", "AR2/ARMA11", "distributional sigma",
      "approximate basis", "exact basis > 500 unique time locations",
      "ARMA", "missing predictors", "residual correlation", "ARMA"
    ),
    status = c(
      "blocked", "supported", "supported", "supported", "supported", "explicit opt-in",
      "blocked", "supported", "supported", "not implemented"
    ),
    rationale = c(
      "Robust likelihood and residual ARMA are compared as separate governed candidates in 0.5.",
      "brms distributional regression supports sigma predictors for Student-t models.",
      "Bounded ARMA orders are exposed only through the governed interface.",
      "Gaussian distributional models are a primary 0.5 target.",
      "Hilbert-space approximate GP is the default scalable GP route.",
      "Exact GP cubic scaling can become prohibitive and requires explicit computational review.",
      "Missing time points alter the residual-series contract; 0.5 does not silently bridge them.",
      "Joint mi()-based latent predictor submodels can carry both missingness and known uncertainty.",
      "Multivariate Gaussian/Student models can estimate residual eye correlation.",
      "The experimental nonlinear family remains deliberately narrow in 0.5."
    ),
    stringsAsFactors = FALSE
  )
}

#' Create a pre-fit advanced pupillometry sensitivity suite
#'
#' The suite materializes scientifically interpretable alternative model
#' specifications without fitting, ranking, or choosing among them.
#'
#' @param specification Baseline advanced specification.
#' @param include Character subset of `"likelihood"`, `"residual_scale"`,
#'   `"autocorrelation"`, `"temporal"`, and `"gp_kernel"`.
#' @return A `gp3bayes_pupil_advanced_sensitivity_suite` object.
#' @export
create_pupil_advanced_sensitivity_suite <- function(
    specification,
    include = c("likelihood", "residual_scale", "autocorrelation", "temporal", "gp_kernel")) {

  if (!inherits(specification, "gp3bayes_pupil_advanced_specification")) stop("Expected an advanced specification.", call. = FALSE)
  allowed <- c("likelihood", "residual_scale", "autocorrelation", "temporal", "gp_kernel")
  if (!is.character(include) || any(!include %in% allowed)) stop("Unknown sensitivity dimension.", call. = FALSE)
  scenarios <- data.frame(
    scenario = "baseline",
    dimension = "baseline",
    value = "declared",
    stringsAsFactors = FALSE
  )
  add <- function(sc, dim, value) {
    scenarios <<- rbind(scenarios, data.frame(scenario = sc, dimension = dim, value = value, stringsAsFactors = FALSE))
  }

  if ("likelihood" %in% include && is.null(specification$autocorrelation)) {
    alt <- if (specification$family == "gaussian") "student" else "gaussian"
    add(paste0("likelihood_", alt), "family", alt)
  }
  if ("residual_scale" %in% include) {
    for (v in setdiff(c("constant", "condition", "time", "condition_time"), specification$residual_scale)) {
      if (grepl("condition", v) && is.null(specification$mapping$condition)) next
      add(paste0("sigma_", v), "residual_scale", v)
    }
  }
  if ("autocorrelation" %in% include && specification$family == "gaussian" && (is.null(specification$missingness_model) || specification$missingness_model$response != "model")) {
    current <- if (is.null(specification$autocorrelation)) {
      "none"
    } else if (specification$autocorrelation$p == 1L && specification$autocorrelation$q == 0L) {
      "ar1"
    } else if (specification$autocorrelation$p == 2L && specification$autocorrelation$q == 0L) {
      "ar2"
    } else if (specification$autocorrelation$p == 1L && specification$autocorrelation$q == 1L) {
      "arma11"
    } else {
      paste0("arma", specification$autocorrelation$p, specification$autocorrelation$q)
    }
    for (v in c("none", "ar1", "ar2", "arma11")) {
      if (v != current) add(paste0("ac_", v), "autocorrelation", v)
    }
  }
  if ("temporal" %in% include) {
    for (v in setdiff(c("linear", "smooth", "gaussian_process"), specification$temporal_structure)) {
      add(paste0("temporal_", v), "temporal_structure", v)
    }
  }
  if ("gp_kernel" %in% include && specification$temporal_structure == "gaussian_process") {
    for (v in setdiff(c("matern32", "matern52", "exp_quad"), specification$gp_spec$kernel)) {
      add(paste0("gp_", v), "gp_kernel", v)
    }
  }

  structure(
    list(
      baseline = specification,
      scenarios = scenarios,
      interpretation = "Sensitivity scenarios are pre-declared alternatives; the suite does not fit, rank, or choose a preferred scenario."
    ),
    class = "gp3bayes_pupil_advanced_sensitivity_suite"
  )
}

#' Materialize one advanced sensitivity scenario
#'
#' @param suite A sensitivity suite.
#' @param scenario Scenario name from `suite$scenarios`.
#' @return An advanced pupil specification.
#' @export
materialize_pupil_advanced_sensitivity_scenario <- function(suite, scenario) {
  if (!inherits(suite, "gp3bayes_pupil_advanced_sensitivity_suite")) stop("Expected an advanced sensitivity suite.", call. = FALSE)
  if (!is.character(scenario) || length(scenario) != 1L || !scenario %in% suite$scenarios$scenario) stop("Unknown sensitivity `scenario`.", call. = FALSE)
  if (scenario == "baseline") return(suite$baseline)
  row <- suite$scenarios[suite$scenarios$scenario == scenario, , drop = FALSE]
  b <- suite$baseline

  temporal <- b$temporal_structure
  family <- b$family
  residual <- b$residual_scale
  ac <- if (is.null(b$autocorrelation)) "none" else b$autocorrelation
  gp <- b$gp_spec

  if (row$dimension == "family") family <- row$value
  if (row$dimension == "residual_scale") residual <- row$value
  if (row$dimension == "autocorrelation") ac <- row$value
  if (row$dimension == "temporal_structure") temporal <- row$value
  if (row$dimension == "gp_kernel") {
    gp <- create_pupil_gp_spec(kernel = row$value, basis = b$gp_spec$basis, k = if (b$gp_spec$basis == "approximate") b$gp_spec$k else 30L, scale = b$gp_spec$scale)
  }
  if (temporal == "gaussian_process" && is.null(gp)) gp <- create_pupil_gp_spec()

  specify_advanced_pupil_timecourse_model(
    prepared = b$prepared,
    temporal_structure = temporal,
    family = family,
    residual_scale = residual,
    smooth_basis_dimension = b$smooth_basis_dimension,
    gp_spec = if (is.null(gp)) create_pupil_gp_spec() else gp,
    condition_trajectory = b$condition_trajectory,
    autocorrelation = ac,
    participant_trajectory = b$participant_trajectory,
    item_effects = b$item_effects,
    covariates = b$covariates,
    measurement_model = b$measurement_model,
    missingness_model = b$missingness_model,
    prior_scales = b$prior_scales,
    predictive_target = b$predictive_target,
    allow_high_complexity = b$allow_high_complexity
  )
}

#' Build an auditable advanced pupil model card
#'
#' @param x An advanced specification or fit.
#' @return A `gp3bayes_pupil_model_card` object containing structured metadata
#'   and governance statements suitable for methods supplements.
#' @export
pupil_model_card <- function(x) {
  fit <- inherits(x, "gp3bayes_pupil_advanced_fit")
  spec <- if (fit) x$specification else x
  if (!inherits(spec, "gp3bayes_pupil_advanced_specification")) stop("Expected an advanced pupil specification or fit.", call. = FALSE)
  d <- spec$data
  m <- spec$mapping
  ac <- spec$autocorrelation
  tab <- data.frame(
    field = c(
      "gp3bayes_version", "fit_performed", "backend", "rows", "participants", "conditions",
      "family", "temporal_structure", "residual_scale", "autocorrelation", "participant_trajectory",
      "measurement_model", "missingness_model", "predictive_target", "complexity_status"
    ),
    value = c(
      spec$version,
      if (fit) "TRUE" else "FALSE",
      if (fit) x$backend else "none",
      nrow(d),
      length(unique(d[[m$participant]][!is.na(d[[m$participant]])])),
      if (is.null(m$condition)) 1 else length(unique(d[[m$condition]][!is.na(d[[m$condition]])])),
      spec$family,
      spec$temporal_structure,
      spec$residual_scale,
      if (is.null(ac)) "none" else paste0("ARMA(", ac$p, ",", ac$q, ")"),
      spec$participant_trajectory,
      !is.null(spec$measurement_model),
      !is.null(spec$missingness_model),
      spec$predictive_target,
      spec$complexity_audit$overall_status
    ),
    stringsAsFactors = FALSE
  )
  structure(
    list(
      table = tab,
      governance = spec$governance,
      interpretation = paste(
        "The model card records declared analysis decisions and computational structure.",
        "It is not an automatic certificate of validity, robustness, or causal identification."
      )
    ),
    class = "gp3bayes_pupil_model_card"
  )
}

#' Tabulate a pupil model card
#' @param x A model-card object, specification, or fit.
#' @export
pupil_model_card_table <- function(x) {
  if (!inherits(x, "gp3bayes_pupil_model_card")) x <- pupil_model_card(x)
  x$table
}

#' @export
print.gp3bayes_pupil_distribution_spec <- function(x, ...) {
  cat("<gp3bayes_pupil_distribution_spec>\n")
  cat("  Family:", x$family, "\n")
  cat("  Residual scale:", x$residual_scale, "\n")
  invisible(x)
}

#' @export
print.gp3bayes_pupil_advanced_sensitivity_suite <- function(x, ...) {
  cat("<gp3bayes_pupil_advanced_sensitivity_suite>\n")
  cat("  Scenarios:", nrow(x$scenarios), "\n")
  print(x$scenarios, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_advanced_sensitivity_suite <- function(x, row.names = NULL, optional = FALSE, ...) x$scenarios

#' @export
print.gp3bayes_pupil_model_card <- function(x, ...) {
  cat("<gp3bayes_pupil_model_card>\n")
  print(x$table, row.names = FALSE)
  cat("Governance:\n")
  cat(paste0("  - ", x$governance, collapse = "\n"), "\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_model_card <- function(x, row.names = NULL, optional = FALSE, ...) x$table

#' @export
print.gp3bayes_pupil_measurement_audit_05 <- function(x, ...) {
  cat("<gp3bayes_pupil_measurement_audit_05>\n")
  cat("  Status:", x$status, "\n")
  print(x$table, row.names = FALSE)
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_pupil_measurement_audit_05 <- function(x, row.names = NULL, optional = FALSE, ...) x$table
