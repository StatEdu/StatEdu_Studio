# Integration contract for fail-closed latent-mixture candidate selection.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args[grepl("^--file=", args)])
script_path <- if (length(file_arg) > 0L) normalizePath(file_arg[1], winslash = "/", mustWork = TRUE) else NA_character_
repo_root <- if (!is.na(script_path)) dirname(dirname(script_path)) else normalizePath(".", winslash = "/", mustWork = TRUE)

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

read_repo <- function(path) {
  full <- file.path(repo_root, path)
  assert_true(file.exists(full), paste0("Required file is missing: ", path))
  paste(readLines(full, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

core_path <- file.path(repo_root, "modules", "latent_mplus", "app", "R", "common", "15_mixture_selection_core.R")
source(core_path, local = TRUE)

loader <- read_repo("modules/latent_mplus/app/R/cross_sectional_mixture/00_run_pipeline.R")
run_step <- read_repo("modules/latent_mplus/app/R/cross_sectional_mixture/03b_estimation_run_mplus.R")
collect_step <- read_repo("modules/latent_mplus/app/R/cross_sectional_mixture/03c_estimation_collect.R")
select_step <- read_repo("modules/latent_mplus/app/R/cross_sectional_mixture/04a_select_best_k.R")
classify_step <- read_repo("modules/latent_mplus/app/R/cross_sectional_mixture/04b_classify.R")
table_step <- read_repo("modules/latent_mplus/app/R/cross_sectional_mixture/05_tables.R")
server_step <- read_repo("modules/latent_mplus/app/R/app_server.R")

assert_true(grepl("15_mixture_selection_core\\.R", loader), "Pipeline does not load the mixture-selection core.")
for (token in c("mixture_parse_run_quality", "file_was_fresh", "mixture_file_signature", "out_md5", "cprob_md5", "exec_ok", "out_fresh", "run_quality_ok")) {
  assert_true(grepl(token, run_step, fixed = TRUE), paste0("03b integration is missing: ", token))
}
assert_true(grepl("mixture_registry_to_row_df", run_step, fixed = TRUE), "03b does not preserve the rich build registry.")
assert_true(!grepl("DIR_OUTPUT_RDS", run_step, fixed = TRUE), "03b still uses a step-specific RDS directory.")
for (artifact in c("estimation_build.rds", "estimation_run.rds")) {
  assert_true(
    grepl(paste0("file.path(DIR_RDS, \"", artifact, "\")"), run_step, fixed = TRUE),
    paste0("03b does not use DIR_RDS for ", artifact, ".")
  )
}
for (token in c("expected_k = k_i", "class_count_complete", "fit_metrics_complete", "out_signature_ok", "output_signature_mismatch", "run_quality_ok")) {
  assert_true(grepl(token, collect_step, fixed = TRUE), paste0("03c integration is missing: ", token))
}
assert_true(
  grepl('status\\s*=\\s*"failed"', collect_step, perl = TRUE),
  "03c does not force failed status when the recorded output is missing."
)
assert_true(
  !grepl("file.path(DIR_MPLUS_OUT, paste0(model_tag", collect_step, fixed = TRUE),
  "03c still falls back to an uncertified conventional output path."
)
for (token in c("mixture_annotate_eligibility", "mixture_select_candidate", "SELECTION_FAILURE_SUMMARY", "eligible")) {
  assert_true(grepl(token, select_step, fixed = TRUE), paste0("04a integration is missing: ", token))
}
assert_true(!grepl("Falling back to raw candidates", select_step, fixed = TRUE), "04a still contains a raw-candidate fallback.")
assert_true(grepl("cprob_ready", classify_step, fixed = TRUE), "04b does not reject stale classification data.")
assert_true(grepl("mixture_detect_generic_cprob_block", classify_step, fixed = TRUE), "04b does not validate generic CPROB column structure.")
assert_true(grepl("expected_prefix_n", classify_step, fixed = TRUE), "04b does not anchor CPROB columns to registered USEVARIABLES.")
assert_true(grepl("CPROB file changed after estimation", classify_step, fixed = TRUE), "04b does not verify the recorded CPROB signature.")
assert_true(
  !grepl("paste0(BEST_MODEL_STRUCTURE, \"_cprob_k\"", classify_step, fixed = TRUE),
  "04b still falls back to an unverified conventional CPROB filename."
)
assert_true(
  grepl("Expected exactly one classification registry row", classify_step, fixed = TRUE),
  "04b does not require an exact selected-model registry match."
)
assert_true(grepl("display_id_ok", classify_step, fixed = TRUE), "04b can enter ID merge without a valid display-data ID.")
for (token in c("MODEL_CANDIDATES_RAW", "Normal termination", "Best LL replicated", "Exclusion reason")) {
  assert_true(grepl(token, table_step, fixed = TRUE), paste0("T1/T2 quality reporting is missing: ", token))
}
assert_true(
  grepl("exclusion\\\\s\\+reason", server_step, perl = TRUE) || grepl("wrap_cols", server_step, fixed = TRUE),
  "Screen renderer does not handle the exclusion-reason column."
)

fixture <- data.frame(
  model_tag = c("demo_model2_k3_lpa", "demo_model2_k4_lpa"),
  k = c(3L, 4L),
  model_structure = c("model2", "model2"),
  status = c("ok", "failed"),
  parse_ok = c(TRUE, TRUE),
  converged = c(TRUE, TRUE),
  loglik_replicated = c(TRUE, FALSE),
  admissible = c(TRUE, TRUE),
  smallest_class_p = c(0.10, 0.08),
  smallest_class_n = c(40, 32),
  entropy = c(0.84, 0.91),
  ll = c(-510, -500),
  npar = c(18, 22),
  aic = c(1056, 1044),
  bic = c(1120, 1100),
  sabic = c(1080, 1060),
  dbic = c(1110, 1090),
  stringsAsFactors = FALSE
)
annotated <- mixture_annotate_eligibility(
  fixture,
  rule = "bic",
  min_class_prop = 0.03,
  min_class_n = 1,
  min_entropy_hard = 0.60
)
selection <- mixture_select_candidate(annotated, mode = "auto", rule = "bic")
assert_true(identical(selection$selected$model_tag[[1L]], "demo_model2_k3_lpa"), "An ineligible lower-BIC model was retained.")
assert_true(grepl("loglik_not_replicated", annotated$failure_reasons[[2L]], fixed = TRUE), "The rejected model lacks an auditable exclusion reason.")

real_dir <- file.path(repo_root, "modules", "latent_mplus", "app", "mplus_tmp", "inp")
good_path <- file.path(real_dir, "kswl_select_cross_sectional_mixture_model2_k5_lpa.out")
bad_path <- file.path(real_dir, "kswl_select_cross_sectional_mixture_model2_k6_lpa.out")
if (file.exists(good_path) && file.exists(bad_path)) {
  good_lines <- readLines(good_path, warn = FALSE)
  good <- mixture_parse_run_quality(good_lines, k = 5L)
  bad <- mixture_parse_run_quality(readLines(bad_path, warn = FALSE), k = 6L)
  assert_true(isTRUE(good$run_quality_ok), "Repository good-output regression did not pass.")
  assert_true(!isTRUE(good$local_maxima_warning), "A normal local-maxima listing was mislabeled as a warning.")
  assert_true(!isTRUE(bad$run_quality_ok) && identical(bad$loglik_replicated, FALSE), "Repository local-maximum regression was not rejected.")
  good_counts <- mixture_parse_class_count_block(good_lines, expected_k = 5L)
  assert_true(good_counts$complete && nrow(good_counts$data) == 5L, "Repository class-count block was not parsed exactly.")
}

savedata_dir <- file.path(repo_root, "modules", "latent_mplus", "app", "mplus_tmp", "savedata")
for (k in 2:6) {
  cprob_path <- file.path(savedata_dir, paste0("model2_cprob_k", k, ".dat"))
  if (!file.exists(cprob_path)) next
  cprob <- utils::read.table(cprob_path, header = FALSE)
  names(cprob) <- paste0("V", seq_len(ncol(cprob)))
  detected <- mixture_detect_generic_cprob_block(cprob, k = k, expected_start = 6L)
  assert_true(
    detected$found && !detected$ambiguous && length(detected$posterior_cols) == k,
    paste0("Repository CPROB structure was not uniquely detected for k=", k, ".")
  )
}

parse_files <- c(
  core_path,
  file.path(repo_root, "modules", "latent_mplus", "app", "R", "cross_sectional_mixture", c(
    "00_run_pipeline.R", "03a_estimation_build_inputs.R", "03b_estimation_run_mplus.R",
    "03c_estimation_collect.R", "04a_select_best_k.R", "04b_classify.R", "05_tables.R"
  )),
  file.path(repo_root, "modules", "latent_mplus", "app", "R", "app_server.R")
)
for (path in parse_files) parse(file = path)

cat("latent mixture selection integration validation PASS\n")
