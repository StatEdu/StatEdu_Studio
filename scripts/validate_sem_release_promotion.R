promotion_read_text <- function(path) {
  if (!file.exists(path)) stop("Required promotion file was not found: ", path, call. = FALSE)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

promotion_require <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

promotion_sha256 <- function(path) {
  digest::digest(file = path, algo = "sha256", serialize = FALSE)
}

promotion_require_sha256 <- function(path, recorded, label) {
  actual <- toupper(promotion_sha256(path))
  expected <- toupper(trimws(as.character(recorded %||% "")))
  promotion_require(nzchar(expected) && identical(actual, expected), paste0(label, " SHA-256 does not match the recorded value."))
  invisible(actual)
}

promotion_read_fit_csv <- function(path, label) {
  value <- utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  required <- c("Model", "Fit", "srmr", "d_G", "d_ULS")
  promotion_require(all(required %in% names(value)), paste0(label, " fit CSV has an invalid schema."))
  promotion_require(nrow(value) == 2L, paste0(label, " fit CSV must contain exactly PLS and PLSc saturated rows."))
  promotion_require(setequal(tolower(trimws(value$Model)), c("pls", "plsc")), paste0(label, " fit CSV must cover PLS and PLSc."))
  promotion_require(all(tolower(trimws(value$Fit)) == "saturated"), paste0(label, " fit CSV must contain saturated results only."))
  for (metric in c("srmr", "d_G", "d_ULS")) {
    numeric_values <- suppressWarnings(as.numeric(value[[metric]]))
    promotion_require(all(is.finite(numeric_values)), paste0(label, " fit CSV contains a non-finite ", metric, " value."))
  }
  invisible(value)
}

promotion_validate_public_manifest <- function(manifest_path) {
  promotion_require(file.exists(manifest_path), paste0(
    "Public 1.2.3 promotion is blocked: create ", manifest_path,
    " from docs/RELEASE_1_2_3_PROMOTION_MANIFEST.template.json after all evidence is complete."
  ))
  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = TRUE)
  promotion_require(identical(as.character(manifest$target_version), "1.2.3"), "Promotion manifest target_version must be 1.2.3.")
  required_gates <- c(
    "external_numeric_evidence", "version_metadata_aligned", "public_notes_finalized",
    "final_package_validated", "manual_packaged_qa", "publication_approved"
  )
  missing_gates <- setdiff(required_gates, names(manifest$gates))
  promotion_require(!length(missing_gates), paste0("Promotion manifest is missing gate(s): ", paste(missing_gates, collapse = ", "), "."))
  incomplete <- required_gates[!vapply(manifest$gates[required_gates], isTRUE, logical(1))]
  promotion_require(!length(incomplete), paste0("Public 1.2.3 promotion gate(s) remain incomplete: ", paste(incomplete, collapse = ", "), "."))

  required_evidence <- c("statedu_fit", "external_fit", "comparison", "external_run_record", "manual_qa_record", "public_notes", "installer", "blockmap")
  missing_evidence_keys <- setdiff(required_evidence, names(manifest$evidence))
  promotion_require(!length(missing_evidence_keys), paste0("Promotion manifest is missing evidence path(s): ", paste(missing_evidence_keys, collapse = ", "), "."))
  evidence_paths <- unlist(manifest$evidence[required_evidence], use.names = TRUE)
  absent <- names(evidence_paths)[!file.exists(evidence_paths)]
  promotion_require(!length(absent), paste0("Promotion evidence file(s) were not found: ", paste(absent, collapse = ", "), "."))

  comparison <- utils::read.csv(evidence_paths[["comparison"]], check.names = FALSE, stringsAsFactors = FALSE)
  promotion_require(all(c("Model", "Fit", "Metric", "StatEdu", "External", "Pass") %in% names(comparison)), "External comparison CSV has an invalid schema.")
  pass_values <- tolower(trimws(as.character(comparison$Pass)))
  promotion_require(nrow(comparison) == 6L && all(pass_values %in% c("true", "t", "1")), "External comparison must contain six passing PLS/PLSc saturated metric rows.")
  promotion_require(setequal(tolower(comparison$Model), c("pls", "plsc")) && all(tolower(comparison$Fit) == "saturated"), "External comparison must cover PLS and PLSc saturated rows only.")

  external_run <- jsonlite::fromJSON(evidence_paths[["external_run_record"]], simplifyVector = TRUE)
  promotion_require(nzchar(trimws(as.character(external_run$software %||% ""))), "External run software is missing.")
  promotion_require(nzchar(trimws(as.character(external_run$version %||% ""))), "External run software version is missing.")
  run_date <- as.character(external_run$run_date %||% "")
  parsed_run_date <- suppressWarnings(try(as.Date(run_date, format = "%Y-%m-%d"), silent = TRUE))
  promotion_require(grepl("^\\d{4}-\\d{2}-\\d{2}$", run_date) && !inherits(parsed_run_date, "try-error") && !is.na(parsed_run_date) && identical(format(parsed_run_date, "%Y-%m-%d"), run_date), "External run date must be a valid calendar date using YYYY-MM-DD.")
  promotion_require(identical(tolower(as.character(external_run$fit_target %||% "")), "saturated"), "External run fit_target must be saturated.")
  promotion_require(identical(tolower(as.character(external_run$weighting_scheme %||% "")), "path"), "External run weighting_scheme must be path.")
  promotion_require(isTRUE(external_run$standardized_results), "External run must record standardized_results=true.")
  promotion_require(isTRUE(external_run$converged_before_300), "External run must confirm convergence before 300 iterations.")
  reported_decimal_places <- suppressWarnings(as.integer(external_run$reported_decimal_places %||% NA_integer_))
  promotion_require(length(reported_decimal_places) == 1L && !is.na(reported_decimal_places) && reported_decimal_places >= 3L && reported_decimal_places <= 15L, "External run reported_decimal_places must be an integer from 3 to 15.")

  data_path <- as.character(external_run$data_file %||% "")
  model_path <- as.character(external_run$model_file %||% "")
  promotion_require(nzchar(data_path) && file.exists(data_path), "External run fixed data file was not found.")
  promotion_require(nzchar(model_path) && file.exists(model_path), "External run fixed model file was not found.")
  promotion_require_sha256(data_path, external_run$data_sha256, "External run data")
  promotion_require_sha256(model_path, external_run$model_sha256, "External run model")
  canonical_data <- file.path("sample", "HolzingerSwineford1939.csv")
  canonical_model <- file.path("sample", "pls_external_benchmark.stmodel")
  promotion_require(file.exists(canonical_data) && file.exists(canonical_model), "Canonical external benchmark fixtures were not found.")
  promotion_require(identical(toupper(promotion_sha256(data_path)), toupper(promotion_sha256(canonical_data))), "External run data are not the fixed canonical benchmark data.")
  promotion_require(identical(toupper(promotion_sha256(model_path)), toupper(promotion_sha256(canonical_model))), "External run model is not the fixed canonical benchmark model.")
  promotion_require_sha256(evidence_paths[["statedu_fit"]], external_run$statedu_fit_sha256, "StatEdu fit evidence")
  promotion_require_sha256(evidence_paths[["external_fit"]], external_run$external_fit_sha256, "External fit evidence")
  promotion_require_sha256(evidence_paths[["comparison"]], external_run$comparison_sha256, "External comparison evidence")

  promotion_read_fit_csv(evidence_paths[["statedu_fit"]], "StatEdu")
  promotion_read_fit_csv(evidence_paths[["external_fit"]], "External")
  absolute_tolerance <- suppressWarnings(as.numeric(external_run$absolute_tolerance %||% NA_real_))
  relative_tolerance <- suppressWarnings(as.numeric(external_run$relative_tolerance %||% NA_real_))
  promotion_require(length(absolute_tolerance) == 1L && is.finite(absolute_tolerance) && absolute_tolerance >= 0, "External run absolute_tolerance must be a finite non-negative number.")
  promotion_require(length(relative_tolerance) == 1L && is.finite(relative_tolerance) && relative_tolerance >= 0, "External run relative_tolerance must be a finite non-negative number.")
  rounding_tolerance <- 0.5 * 10^(-reported_decimal_places)
  promotion_require(absolute_tolerance + .Machine$double.eps >= rounding_tolerance, "External run absolute_tolerance is smaller than the recorded output precision permits.")
  comparison_environment <- new.env(parent = baseenv())
  sys.source("scripts/compare_pls_fit_external.R", envir = comparison_environment)
  recomputed <- comparison_environment$pls_fit_compare_external(
    evidence_paths[["statedu_fit"]], evidence_paths[["external_fit"]],
    absolute_tolerance = absolute_tolerance, relative_tolerance = relative_tolerance
  )
  promotion_require(nrow(recomputed) == 6L && all(recomputed$Pass), "Recomputed external PLS/PLSc comparison did not pass the recorded tolerances.")
  comparison_keys <- paste(tolower(comparison$Model), tolower(comparison$Fit), comparison$Metric, sep = "::")
  recomputed_keys <- paste(tolower(recomputed$Model), tolower(recomputed$Fit), recomputed$Metric, sep = "::")
  promotion_require(identical(comparison_keys, recomputed_keys), "Recorded external comparison rows do not match the recomputed comparison.")
  for (column in c("StatEdu", "External", "Absolute error", "Relative error")) {
    promotion_require(column %in% names(comparison), paste0("External comparison CSV is missing ", column, "."))
    recorded <- suppressWarnings(as.numeric(comparison[[column]]))
    promotion_require(all(is.finite(recorded)) && isTRUE(all.equal(recorded, recomputed[[column]], tolerance = 1e-12, check.attributes = FALSE)), paste0("Recorded external comparison ", column, " values do not match recomputation."))
  }

  promotion_require_sha256(evidence_paths[["installer"]], manifest$checksums$installer_sha256, "Installer")
  promotion_require_sha256(evidence_paths[["blockmap"]], manifest$checksums$blockmap_sha256, "Blockmap")
  promotion_require(nzchar(trimws(as.character(manifest$approval$approved_by %||% ""))), "Promotion approver is missing.")
  promotion_require(nzchar(trimws(as.character(manifest$approval$approved_at %||% ""))), "Promotion approval time is missing.")
  invisible(TRUE)
}

`%||%` <- function(value, fallback) if (is.null(value) || !length(value)) fallback else value

version <- trimws(readLines("VERSION", warn = FALSE, n = 1L))
decision_log <- promotion_read_text("docs/RELEASE_1_2_3_DECISION_LOG.md")
checklist <- promotion_read_text("docs/RELEASE_1_2_3_PROMOTION_CHECKLIST.md")
public_notes <- promotion_read_text("docs/RELEASE_1_2_3_PUBLIC_NOTES_DRAFT.md")
template <- jsonlite::fromJSON("docs/RELEASE_1_2_3_PROMOTION_MANIFEST.template.json", simplifyVector = TRUE)
external_run_template <- jsonlite::fromJSON("docs/RELEASE_1_2_3_EXTERNAL_PLS_RUN.template.json", simplifyVector = TRUE)

promotion_require(grepl("does not modify or supersede the already published 1.2.0", checklist, fixed = TRUE), "1.2.3 checklist must preserve the historical 1.2.0 release boundary.")
promotion_require(identical(as.character(template$target_version), "1.2.3"), "Promotion-manifest template target version must be 1.2.3.")
promotion_require(all(!unlist(template$gates, use.names = FALSE)), "Promotion-manifest template gates must default to false.")
promotion_require(identical(as.character(external_run_template$fit_target), "saturated"), "External-run template must default to saturated fit.")
promotion_require(!isTRUE(external_run_template$standardized_results) && !isTRUE(external_run_template$converged_before_300), "External-run template confirmations must default to false.")
promotion_require(identical(external_run_template$reported_decimal_places, 0L), "External-run template decimal places must remain unset.")

if (identical(version, "1.2.3")) {
  promotion_require(grepl("Public promotion: approved", decision_log, fixed = TRUE), "Final 1.2.3 decision log must record explicit public-promotion approval.")
  promotion_require(!grepl("pending", public_notes, ignore.case = TRUE), "Final 1.2.3 public notes must not contain pending placeholders.")
  promotion_validate_public_manifest("docs/evidence/release_1_2_3/promotion_manifest.json")
  cat("SEM 1.2.3 public-promotion gate passed.\n")
} else if (identical(version, "1.2.3-dev")) {
  promotion_require(grepl("Public promotion: blocked", decision_log, fixed = TRUE), "Development decision log must retain the blocked promotion status.")
  promotion_require(grepl("Status: draft; not approved for publication", public_notes, fixed = TRUE), "Development public notes must remain an unapproved draft.")
  missing_manifest_error <- try(promotion_validate_public_manifest(tempfile("missing-promotion-manifest-", fileext = ".json")), silent = TRUE)
  promotion_require(inherits(missing_manifest_error, "try-error") && grepl("Public 1.2.3 promotion is blocked", as.character(missing_manifest_error), fixed = TRUE), "Public promotion validator must fail closed when the manifest is absent.")
  cat("SEM 1.2.3 public-promotion gate is armed; development version remains blocked from public promotion.\n")
} else {
  cat("SEM 1.2.3 public-promotion gate not applicable to version ", version, ".\n", sep = "")
}
