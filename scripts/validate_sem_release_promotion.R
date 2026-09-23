promotion_read_text <- function(path) {
  if (!file.exists(path)) stop("Required promotion file was not found: ", path, call. = FALSE)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

promotion_record_field <- function(text, label) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  prefix <- paste0(label, ":")
  matches <- lines[startsWith(trimws(lines), prefix)]
  if (!length(matches)) return("")
  trimws(sub(paste0("^", prefix), "", trimws(matches[[length(matches)]])))
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

promotion_validate_approval_record <- function(text, manifest, packaged_commit, comparison_sha256) {
  promotion_require(identical(promotion_record_field(text, "Target version"), "1.2.3"), "Approval record target version must be 1.2.3.")
  promotion_require(identical(tolower(promotion_record_field(text, "Publication approval status")), "approved"), "Publication approval record status must be Approved.")
  promotion_require(!grepl("| Pending |", text, fixed = TRUE), "Publication approval record still contains Pending rows.")
  promotion_require(!grepl("| Fail |", text, fixed = TRUE), "Publication approval record still contains Fail rows.")
  approved_by <- promotion_record_field(text, "Approved by")
  approver_role <- promotion_record_field(text, "Approver role")
  approval_time <- promotion_record_field(text, "Approval time")
  promotion_require(nzchar(approved_by), "Publication approver is missing from the approval record.")
  promotion_require(nzchar(approver_role), "Publication approver role is missing from the approval record.")
  promotion_require(grepl("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(Z|[+-]\\d{2}:\\d{2})$", approval_time), "Publication approval time must use ISO 8601 with a timezone.")
  promotion_require(identical(approved_by, trimws(as.character(manifest$approval$approved_by %||% ""))), "Approval-record approver does not match the promotion manifest.")
  promotion_require(identical(approval_time, trimws(as.character(manifest$approval$approved_at %||% ""))), "Approval-record time does not match the promotion manifest.")
  promotion_require(identical(promotion_record_field(text, "Approved release commit"), packaged_commit), "Approved release commit does not match packaged validation.")
  promotion_require(identical(toupper(promotion_record_field(text, "Approved installer SHA-256")), toupper(as.character(manifest$checksums$installer_sha256))), "Approved installer SHA-256 does not match the promotion manifest.")
  promotion_require(identical(toupper(promotion_record_field(text, "Approved blockmap SHA-256")), toupper(as.character(manifest$checksums$blockmap_sha256))), "Approved blockmap SHA-256 does not match the promotion manifest.")
  promotion_require(identical(toupper(promotion_record_field(text, "PLS comparison SHA-256")), toupper(as.character(comparison_sha256))), "Approved PLS comparison SHA-256 does not match the external evidence.")
  promotion_require(identical(promotion_record_field(text, "Release tag"), "v1.2.3"), "Approval record release tag must be v1.2.3.")
  invisible(TRUE)
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

  required_evidence <- c("statedu_fit", "external_fit", "comparison", "external_run_record", "packaged_validation_record", "manual_qa_record", "approval_record", "public_notes", "installer", "blockmap")
  missing_evidence_keys <- setdiff(required_evidence, names(manifest$evidence))
  promotion_require(!length(missing_evidence_keys), paste0("Promotion manifest is missing evidence path(s): ", paste(missing_evidence_keys, collapse = ", "), "."))
  evidence_paths <- unlist(manifest$evidence[required_evidence], use.names = TRUE)
  absent <- names(evidence_paths)[!file.exists(evidence_paths)]
  promotion_require(!length(absent), paste0("Promotion evidence file(s) were not found: ", paste(absent, collapse = ", "), "."))

  manual_qa <- promotion_read_text(evidence_paths[["manual_qa_record"]])
  promotion_require(identical(promotion_record_field(manual_qa, "Release candidate"), "1.2.3"), "Manual QA record must identify release candidate 1.2.3.")
  promotion_require(identical(tolower(promotion_record_field(manual_qa, "Manual QA status")), "pass"), "Manual QA final status must be Pass.")
  promotion_require(!grepl("| Pending |", manual_qa, fixed = TRUE), "Manual QA record still contains Pending rows.")
  promotion_require(!grepl("| Fail |", manual_qa, fixed = TRUE), "Manual QA record still contains Fail rows.")
  promotion_require(nzchar(promotion_record_field(manual_qa, "Tester sign-off")), "Manual QA tester sign-off is missing.")
  promotion_require(nzchar(promotion_record_field(manual_qa, "Sign-off time")), "Manual QA sign-off time is missing.")

  packaged_validation <- promotion_read_text(evidence_paths[["packaged_validation_record"]])
  promotion_require(identical(promotion_record_field(packaged_validation, "Release candidate"), "1.2.3"), "Packaged validation record must identify release candidate 1.2.3.")
  promotion_require(identical(tolower(promotion_record_field(packaged_validation, "Packaged validation status")), "pass"), "Packaged validation final status must be Pass.")
  promotion_require(!grepl("| Pending |", packaged_validation, fixed = TRUE), "Packaged validation record still contains Pending rows.")
  promotion_require(!grepl("| Fail |", packaged_validation, fixed = TRUE), "Packaged validation record still contains Fail rows.")
  promotion_require(nzchar(promotion_record_field(packaged_validation, "Git commit")), "Packaged validation Git commit is missing.")
  promotion_require(nzchar(promotion_record_field(packaged_validation, "Validation date")), "Packaged validation date is missing.")
  promotion_require(identical(promotion_record_field(packaged_validation, "R runtime"), "R-4.5.3"), "Packaged validation must record bundled R-4.5.3.")
  promotion_require(identical(promotion_record_field(packaged_validation, "Residual packaged processes"), "0"), "Packaged validation must record zero residual packaged processes.")
  installer_bytes <- suppressWarnings(as.numeric(promotion_record_field(packaged_validation, "Installer bytes")))
  blockmap_bytes <- suppressWarnings(as.numeric(promotion_record_field(packaged_validation, "Blockmap bytes")))
  promotion_require(length(installer_bytes) == 1L && is.finite(installer_bytes) && installer_bytes > 0, "Packaged validation installer byte size must be positive.")
  promotion_require(length(blockmap_bytes) == 1L && is.finite(blockmap_bytes) && blockmap_bytes > 0, "Packaged validation blockmap byte size must be positive.")

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
  promotion_require(identical(toupper(promotion_record_field(manual_qa, "Installer SHA-256")), toupper(as.character(manifest$checksums$installer_sha256))), "Manual QA installer SHA-256 does not match the promotion manifest.")
  promotion_require(identical(toupper(promotion_record_field(manual_qa, "Blockmap SHA-256")), toupper(as.character(manifest$checksums$blockmap_sha256))), "Manual QA blockmap SHA-256 does not match the promotion manifest.")
  promotion_require(identical(toupper(promotion_record_field(packaged_validation, "Installer SHA-256")), toupper(as.character(manifest$checksums$installer_sha256))), "Packaged validation installer SHA-256 does not match the promotion manifest.")
  promotion_require(identical(toupper(promotion_record_field(packaged_validation, "Blockmap SHA-256")), toupper(as.character(manifest$checksums$blockmap_sha256))), "Packaged validation blockmap SHA-256 does not match the promotion manifest.")
  promotion_require(nzchar(trimws(as.character(manifest$approval$approved_by %||% ""))), "Promotion approver is missing.")
  promotion_require(nzchar(trimws(as.character(manifest$approval$approved_at %||% ""))), "Promotion approval time is missing.")
  approval_record <- promotion_read_text(evidence_paths[["approval_record"]])
  promotion_validate_approval_record(approval_record, manifest, promotion_record_field(packaged_validation, "Git commit"), external_run$comparison_sha256)
  invisible(TRUE)
}

`%||%` <- function(value, fallback) if (is.null(value) || !length(value)) fallback else value

version <- trimws(readLines("VERSION", warn = FALSE, n = 1L))
decision_log <- promotion_read_text("docs/RELEASE_1_2_3_DECISION_LOG.md")
checklist <- promotion_read_text("docs/RELEASE_1_2_3_PROMOTION_CHECKLIST.md")
public_notes <- promotion_read_text("docs/RELEASE_1_2_3_PUBLIC_NOTES_DRAFT.md")
manual_qa_template <- promotion_read_text("docs/RELEASE_1_2_3_MANUAL_QA_RECORD.md")
packaged_validation_template <- promotion_read_text("docs/RELEASE_1_2_3_PACKAGED_VALIDATION_NOTES.md")
approval_template <- promotion_read_text("docs/RELEASE_1_2_3_APPROVAL_RECORD.md")
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
  promotion_require(grepl("Overall status: blocked / pending final public package", manual_qa_template, fixed = TRUE), "Development manual QA record must remain blocked pending the final public package.")
  promotion_require(!grepl("| Pass |", manual_qa_template, fixed = TRUE), "Development manual QA template must not contain final-package Pass rows.")
  promotion_require(grepl("Overall status: blocked / pending final public package", packaged_validation_template, fixed = TRUE), "Development packaged validation record must remain blocked pending the final public package.")
  promotion_require(!grepl("| Pass |", packaged_validation_template, fixed = TRUE), "Development packaged validation template must not contain final-package Pass rows.")
  promotion_require(grepl("Overall status: blocked / pending explicit publication approval", approval_template, fixed = TRUE), "Development publication approval record must remain blocked pending explicit approval.")
  promotion_require(!grepl("| Pass |", approval_template, fixed = TRUE), "Development publication approval record must not contain Pass rows.")
  missing_manifest_error <- try(promotion_validate_public_manifest(tempfile("missing-promotion-manifest-", fileext = ".json")), silent = TRUE)
  promotion_require(inherits(missing_manifest_error, "try-error") && grepl("Public 1.2.3 promotion is blocked", as.character(missing_manifest_error), fixed = TRUE), "Public promotion validator must fail closed when the manifest is absent.")
  dummy_evidence <- tempfile("promotion-dummy-evidence-", fileext = ".txt")
  manual_gate_manifest <- tempfile("promotion-manual-gate-", fileext = ".json")
  writeLines("placeholder", dummy_evidence, useBytes = TRUE)
  jsonlite::write_json(list(
    target_version = "1.2.3",
    gates = as.list(stats::setNames(rep(TRUE, 6L), c(
      "external_numeric_evidence", "version_metadata_aligned", "public_notes_finalized",
      "final_package_validated", "manual_packaged_qa", "publication_approved"
    ))),
    evidence = list(
      statedu_fit = dummy_evidence, external_fit = dummy_evidence, comparison = dummy_evidence,
      external_run_record = dummy_evidence, packaged_validation_record = "docs/RELEASE_1_2_3_PACKAGED_VALIDATION_NOTES.md",
      manual_qa_record = "docs/RELEASE_1_2_3_MANUAL_QA_RECORD.md",
      approval_record = "docs/RELEASE_1_2_3_APPROVAL_RECORD.md",
      public_notes = dummy_evidence, installer = dummy_evidence, blockmap = dummy_evidence
    ),
    checksums = list(installer_sha256 = "", blockmap_sha256 = ""),
    approval = list(approved_by = "test", approved_at = "test")
  ), manual_gate_manifest, auto_unbox = TRUE, pretty = TRUE)
  manual_gate_error <- try(promotion_validate_public_manifest(manual_gate_manifest), silent = TRUE)
  promotion_require(inherits(manual_gate_error, "try-error") && grepl("Manual QA final status must be Pass", as.character(manual_gate_error), fixed = TRUE), "Public promotion validator must reject a Pending manual QA record.")
  dummy_manual_qa <- tempfile("promotion-passing-manual-qa-", fileext = ".md")
  writeLines(c(
    "Release candidate: 1.2.3", "Installer SHA-256:", "Blockmap SHA-256:",
    "Manual QA status: Pass", "Tester sign-off: test", "Sign-off time: test"
  ), dummy_manual_qa, useBytes = TRUE)
  pending_package_manifest <- tempfile("promotion-package-gate-", fileext = ".json")
  pending_manifest <- jsonlite::fromJSON(manual_gate_manifest, simplifyVector = FALSE)
  pending_manifest$evidence$manual_qa_record <- dummy_manual_qa
  jsonlite::write_json(pending_manifest, pending_package_manifest, auto_unbox = TRUE, pretty = TRUE)
  package_gate_error <- try(promotion_validate_public_manifest(pending_package_manifest), silent = TRUE)
  unlink(c(dummy_evidence, manual_gate_manifest, dummy_manual_qa, pending_package_manifest), force = TRUE)
  promotion_require(inherits(package_gate_error, "try-error") && grepl("Packaged validation final status must be Pass", as.character(package_gate_error), fixed = TRUE), "Public promotion validator must reject a Pending packaged validation record.")
  synthetic_manifest <- list(
    checksums = list(installer_sha256 = paste(rep("A", 64L), collapse = ""), blockmap_sha256 = paste(rep("B", 64L), collapse = "")),
    approval = list(approved_by = "Test Approver", approved_at = "2026-08-19T22:30:00+09:00")
  )
  synthetic_comparison_sha256 <- paste(rep("C", 64L), collapse = "")
  pending_approval_error <- try(promotion_validate_approval_record(approval_template, synthetic_manifest, "abc123", synthetic_comparison_sha256), silent = TRUE)
  promotion_require(inherits(pending_approval_error, "try-error") && grepl("Publication approval record status must be Approved", as.character(pending_approval_error), fixed = TRUE), "Public promotion validator must reject a Pending approval record.")
  synthetic_approval <- paste(c(
    "Target version: 1.2.3", "Publication approval status: Approved",
    "Approved by: Test Approver", "Approver role: Release owner", "Approval time: 2026-08-19T22:30:00+09:00",
    "Approved release commit: abc123",
    paste0("Approved installer SHA-256: ", synthetic_manifest$checksums$installer_sha256),
    paste0("Approved blockmap SHA-256: ", synthetic_manifest$checksums$blockmap_sha256),
    paste0("PLS comparison SHA-256: ", synthetic_comparison_sha256),
    "Release tag: v1.2.3"
  ), collapse = "\n")
  promotion_validate_approval_record(synthetic_approval, synthetic_manifest, "abc123", synthetic_comparison_sha256)
  cat("SEM 1.2.3 public-promotion gate is armed; development version remains blocked from public promotion.\n")
} else {
  cat("SEM 1.2.3 public-promotion gate not applicable to version ", version, ".\n", sep = "")
}
