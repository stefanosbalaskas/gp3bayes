.gp3p_gazepoint_schema <- function() {
  data.frame(
    field = c(
      "TIME", "TIME_TICK",
      "LPOGX", "LPOGY", "LPOGV", "RPOGX", "RPOGY", "RPOGV",
      "BPOGX", "BPOGY", "BPOGV",
      "LPCX", "LPCY", "LPD", "LPS", "LPV",
      "RPCX", "RPCY", "RPD", "RPS", "RPV",
      "LEYEX", "LEYEY", "LEYEZ", "LPUPILD", "LPUPILV",
      "REYEX", "REYEY", "REYEZ", "RPUPILD", "RPUPILV"
    ),
    role = c(
      "time", "time_tick",
      "left_gaze_x", "left_gaze_y", "left_gaze_valid",
      "right_gaze_x", "right_gaze_y", "right_gaze_valid",
      "best_gaze_x", "best_gaze_y", "best_gaze_valid",
      "left_pupil_camera_x", "left_pupil_camera_y", "left_pupil_diameter_pixels",
      "left_pupil_scale", "left_pupil_valid",
      "right_pupil_camera_x", "right_pupil_camera_y", "right_pupil_diameter_pixels",
      "right_pupil_scale", "right_pupil_valid",
      "left_eye_x", "left_eye_y", "left_eye_z", "left_pupil_diameter_metres",
      "left_pupil_3d_valid",
      "right_eye_x", "right_eye_y", "right_eye_z", "right_pupil_diameter_metres",
      "right_pupil_3d_valid"
    ),
    unit = c(
      "seconds", "ticks",
      rep("normalized_screen", 9),
      "camera_pixels", "camera_pixels", "pixels", "scale", "indicator",
      "camera_pixels", "camera_pixels", "pixels", "scale", "indicator",
      "metres", "metres", "metres", "metres", "indicator",
      "metres", "metres", "metres", "metres", "indicator"
    ),
    eye = c(
      "none", "none",
      "left", "left", "left", "right", "right", "right",
      "combined", "combined", "combined",
      "left", "left", "left", "left", "left",
      "right", "right", "right", "right", "right",
      "left", "left", "left", "left", "left",
      "right", "right", "right", "right", "right"
    ),
    source_specification = "Gazepoint Open Gaze API v2-era field specification",
    stringsAsFactors = FALSE
  )
}

#' Inspect verified Gazepoint pupil and gaze fields
#'
#' Compares column names with the documented Open Gaze API field identifiers.
#' The inspector reports candidates and ambiguity but never chooses a pupil
#' channel automatically. Export variants that use other names remain
#' unrecognized rather than being guessed.
#'
#' @param data A data frame containing a Gazepoint export or API record table.
#' @return A `gp3bayes_gazepoint_pupil_schema` with detected fields,
#'   pupil-channel candidates, and an audit table.
#' @details `LPD` and `RPD` are documented pixel diameters. `LPUPILD` and
#'   `RPUPILD` are documented in metres. These scales are intentionally kept
#'   distinct.
#' @section Governance boundary:
#' Recognition is schema evidence only. The function does not establish
#' device validity, select an eye, convert units, correct PFE, or infer
#' preprocessing history.
#' @examples
#' x <- data.frame(TIME = 0:2 / 60, LPD = c(32, 33, 31), LPV = 1)
#' inspect_gazepoint_pupil_schema(x)
#' @export
inspect_gazepoint_pupil_schema <- function(data) {
  if (!is.data.frame(data)) .gp3p_stop("`data` must be a data frame.")
  schema <- .gp3p_gazepoint_schema()
  schema$present <- schema$field %in% names(data)
  detected <- schema[schema$present, , drop = FALSE]

  pupil_roles <- c(
    "left_pupil_diameter_pixels", "right_pupil_diameter_pixels",
    "left_pupil_diameter_metres", "right_pupil_diameter_metres"
  )
  candidates <- detected[detected$role %in% pupil_roles, , drop = FALSE]
  time_candidates <- detected[detected$role %in% c("time", "time_tick"), , drop = FALSE]
  gaze_candidates <- detected[grepl("gaze_[xy]$", detected$role), , drop = FALSE]
  validity_candidates <- detected[grepl("valid$", detected$role), , drop = FALSE]

  status <- if (!nrow(candidates)) {
    "missing_pupil_channel"
  } else if (nrow(candidates) > 1L) {
    "ambiguous_pupil_channel"
  } else {
    "single_pupil_candidate"
  }

  audit <- data.frame(
    check = c(
      "documented_fields_detected", "pupil_channels_detected",
      "time_fields_detected", "gaze_fields_detected", "validity_fields_detected",
      "channel_selection"
    ),
    value = c(
      nrow(detected), nrow(candidates), nrow(time_candidates),
      nrow(gaze_candidates), nrow(validity_candidates), status
    ),
    status = c(
      if (nrow(detected)) "pass" else "review",
      if (nrow(candidates)) "pass" else "fail",
      if (nrow(time_candidates)) "pass" else "review",
      if (nrow(gaze_candidates)) "pass" else "review",
      if (nrow(validity_candidates)) "pass" else "review",
      if (identical(status, "single_pupil_candidate")) "pass" else "review"
    ),
    detail = c(
      "Only exact documented field identifiers are recognized.",
      "LPD/RPD are pixels; LPUPILD/RPUPILD are metres.",
      "TIME is elapsed seconds; TIME_TICK is a processor tick count.",
      "POG coordinates are reported separately from pupil-camera coordinates.",
      "Validity fields are channel/measurement specific.",
      "Explicit pupil-column selection is required downstream."
    ),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      schema_version = "open-gaze-v2-era",
      detected = detected,
      missing_documented = schema[!schema$present, , drop = FALSE],
      pupil_candidates = candidates,
      time_candidates = time_candidates,
      gaze_candidates = gaze_candidates,
      validity_candidates = validity_candidates,
      status = status,
      audit = audit,
      automatic_channel_selection = FALSE
    ),
    class = "gp3bayes_gazepoint_pupil_schema"
  )
}

#' Return the Gazepoint pupil mapping audit table
#' @param x A result from `inspect_gazepoint_pupil_schema()` or a compatible
#'   data frame to inspect.
#' @return A data frame with documented fields, roles, units, eye, and presence.
#' @examples
#' # See vignette("bayesian-dynamic-pupillometry", package = "gp3bayes") for a complete workflow.
#' @export
gazepoint_pupil_mapping_table <- function(x) {
  if (is.data.frame(x)) x <- inspect_gazepoint_pupil_schema(x)
  if (!inherits(x, "gp3bayes_gazepoint_pupil_schema")) {
    .gp3p_stop("`x` must be a Gazepoint pupil schema inspection or data frame.")
  }
  schema <- .gp3p_gazepoint_schema()
  present <- x$detected$field
  schema$present <- schema$field %in% present
  schema
}

#' @export
print.gp3bayes_gazepoint_pupil_schema <- function(x, ...) {
  cat("<gp3bayes_gazepoint_pupil_schema>\n")
  cat("  Status: ", x$status, "\n", sep = "")
  cat("  Documented fields detected: ", nrow(x$detected), "\n", sep = "")
  cat("  Pupil candidates: ", nrow(x$pupil_candidates), "\n", sep = "")
  if (nrow(x$pupil_candidates)) {
    cat("  Candidates: ", paste(x$pupil_candidates$field, collapse = ", "), "\n", sep = "")
  }
  cat("  Automatic channel selection: FALSE\n")
  invisible(x)
}

#' @export
as.data.frame.gp3bayes_gazepoint_pupil_schema <- function(x, ...) {
  gazepoint_pupil_mapping_table(x)
}
