if (.Platform$OS.type == "windows") {
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE)))
}

source(file.path("scripts", "finalize_pls_external_evidence.R"), encoding = "UTF-8")

validation_mode <- tolower(trimws(Sys.getenv("STATEDU_SMARTPLS_EVIDENCE_MODE", "required")))
if (!validation_mode %in% c("required", "optional")) {
  stop("STATEDU_SMARTPLS_EVIDENCE_MODE must be required or optional.", call. = FALSE)
}
private_root_value <- trimws(Sys.getenv("STATEDU_SMARTPLS_EVIDENCE_ROOT", ""))
if (!nzchar(private_root_value)) {
  if (identical(validation_mode, "optional")) {
    message("SmartPLS private evidence validation SKIP (explicit optional mode): STATEDU_SMARTPLS_EVIDENCE_ROOT is not set.")
    quit(save = "no", status = 0L)
  }
  stop("RELEASE BLOCKER: STATEDU_SMARTPLS_EVIDENCE_ROOT is required for SmartPLS private evidence validation.", call. = FALSE)
}
if (!grepl("^(?:[A-Za-z]:[\\\\/]|/|[\\\\]{2})", private_root_value, perl = TRUE)) {
  stop("STATEDU_SMARTPLS_EVIDENCE_ROOT must be an absolute path.", call. = FALSE)
}
private_root <- normalizePath(private_root_value, winslash = "/", mustWork = TRUE)

public_root <- file.path("docs", "evidence", "release_1_2_4", "pls")
hs_public <- file.path(public_root, "smartpls_4_1_1_8_hs_first100")
tam_public <- file.path(public_root, "smartpls_4_1_1_8_tam100_supplement")
smartpls_citation <- "Ringle, C. M., Wende, S., and Becker, J.-M. (2024). SmartPLS 4. Bönningstedt: SmartPLS GmbH. https://www.smartpls.com ."
citation_public_files <- c(
  file.path("docs", "PLS_EXTERNAL_FIT_BENCHMARK_KO.md"),
  file.path(hs_public, "README.md"),
  file.path(tam_public, "README.md"),
  file.path(hs_public, "external_run.json"),
  file.path(tam_public, "evidence_manifest.json")
)
stopifnot(length(citation_public_files) == 5L, all(file.exists(citation_public_files)))
for (path in citation_public_files) {
  text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  if (!grepl(smartpls_citation, text, fixed = TRUE)) {
    stop("The complete SmartPLS Terms section 8.1 citation is missing from: ", path, call. = FALSE)
  }
  if (!grepl("SmartPLS Terms §3.4", text, fixed = TRUE)) {
    stop("The SmartPLS Terms section 3.4 private-artifact basis is missing from: ", path, call. = FALSE)
  }
}
hs_private <- pls_evidence_resolve_private_file(private_root, basename(hs_public), "SmartPLS HS private bundle")
tam_private <- pls_evidence_resolve_private_file(private_root, basename(tam_public), "SmartPLS TAM private bundle")

tracked_evidence <- system2("git", c("ls-files", "--", gsub("\\\\", "/", public_root)), stdout = TRUE, stderr = TRUE)
forbidden_public <- grepl("\\.(png|splsm)$|\\.settings\\.json$|/smartpls_imported_data\\.txt$", tracked_evidence, ignore.case = TRUE)
if (any(forbidden_public)) {
  stop("SmartPLS vendor UI/project/settings artifacts must not be tracked in the public evidence tree.", call. = FALSE)
}

hash_tree <- function(path) {
  root <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- sort(list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE))
  files <- files[file.info(files)$isdir %in% FALSE]
  normalized_files <- normalizePath(files, winslash = "/", mustWork = TRUE)
  stats::setNames(vapply(files, pls_evidence_sha256, character(1)), substring(normalized_files, nchar(root) + 2L))
}

public_before <- hash_tree(public_root)
run_record <- jsonlite::fromJSON(file.path(hs_public, "external_run.json"), simplifyVector = TRUE)
benchmark_manifest <- jsonlite::fromJSON(file.path(hs_public, "benchmark_manifest.json"), simplifyVector = TRUE)
stopifnot(
  identical(run_record$finalization_status, "finalized"),
  identical(run_record$profile, "holzinger-swineford-first100-smartpls-student"),
  identical(run_record$license_edition, "Student"),
  identical(run_record$license_description, "Student license (free limited, non-Professional)"),
  identical(run_record$software_citation, smartpls_citation),
  identical(run_record$citation_basis, "SmartPLS Terms §8.1"),
  grepl("SmartPLS Terms §3.4", run_record$private_artifact_publication_basis, fixed = TRUE),
  identical(benchmark_manifest$profile$id, run_record$profile),
  identical(benchmark_manifest$public_text_hash_normalization, run_record$public_text_hash_normalization),
  identical(tolower(benchmark_manifest$data$sha256), tolower(run_record$data_sha256)),
  identical(tolower(benchmark_manifest$data$source_sha256), tolower(run_record$source_data_sha256)),
  identical(tolower(benchmark_manifest$model$sha256), tolower(run_record$model_sha256))
)

validated <- finalize_pls_external_evidence(
  hs_public, run_record$software, run_record$version, run_record$run_date,
  TRUE, as.integer(run_record$reported_decimal_places),
  as.numeric(run_record$absolute_tolerance), as.numeric(run_record$relative_tolerance),
  private_evidence_dir = hs_private
)
half_unit <- 0.5 * 10^(-as.integer(run_record$reported_decimal_places)) + .Machine$double.eps
stopifnot(
  nrow(validated$comparison) == 6L,
  all(validated$comparison$Pass),
  all(validated$comparison$`Absolute error` <= half_unit)
)

regenerated_dir <- tempfile(pattern = "statedu-pls-canonical-")
dir.create(regenerated_dir, recursive = TRUE)
on.exit(unlink(regenerated_dir, recursive = TRUE, force = TRUE), add = TRUE)
regenerated <- generate_pls_external_benchmark(
  regenerated_dir, "holzinger-swineford-first100-smartpls-student"
)
exact <- pls_fit_compare_external(regenerated$statedu_path, file.path(hs_public, "statedu_fit.csv"), 0, 0)
stopifnot(nrow(exact) == 6L, all(exact$Pass))

tam_manifest <- jsonlite::fromJSON(file.path(tam_public, "evidence_manifest.json"), simplifyVector = TRUE)
tam_evidence <- tam_manifest$retained_evidence
hs_private_names <- c(
  unlist(run_record$execution_artifacts[grepl("_file$", names(run_record$execution_artifacts))], use.names = FALSE),
  unlist(lapply(run_record$runs, function(run) run[grepl("_file$", names(run))]), use.names = FALSE)
)
tam_private_names <- unname(c(tam_evidence$pls_model_fit_file, tam_evidence$plsc_model_fit_file))
private_names <- as.character(c(hs_private_names, tam_private_names))
stopifnot(
  length(private_names) == 16L,
  length(unique(hs_private_names)) == 14L,
  length(unique(tam_private_names)) == 2L,
  identical(basename(private_names), private_names),
  length(unique(c(
    file.path(basename(hs_public), hs_private_names),
    file.path(basename(tam_public), tam_private_names)
  ))) == 16L
)
pls_evidence_require_recorded_text_hash(
  file.path(tam_public, tam_evidence$reference_file), tam_evidence$reference_sha256, "SmartPLS TAM public reference"
)
for (prefix in c("pls_model_fit", "plsc_model_fit")) {
  path <- pls_evidence_resolve_private_file(tam_private, tam_evidence[[paste0(prefix, "_file")]], paste("SmartPLS TAM", prefix))
  pls_evidence_require_recorded_artifact(
    path, tam_evidence[[paste0(prefix, "_sha256")]], tam_evidence[[paste0(prefix, "_bytes")]], paste("SmartPLS TAM", prefix)
  )
}
stopifnot(
  identical(tam_manifest$output_provenance, "manual displayed transcription; source/path evidence not retained; optional check"),
  !isTRUE(tam_manifest$source_artifacts_included),
  identical(tam_manifest$software_citation, smartpls_citation),
  identical(tam_manifest$citation_basis, "SmartPLS Terms §8.1"),
  grepl("SmartPLS Terms §3.4", tam_manifest$private_artifact_publication_basis, fixed = TRUE)
)

public_after <- hash_tree(public_root)
stopifnot(identical(public_before, public_after))
message("SmartPLS private/public evidence validation passed: 16 private artifacts, exact StatEdu regeneration, and 6/6 displayed comparisons.")
