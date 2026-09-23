#!/usr/bin/env Rscript

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- grep("^--file=", arguments, value = TRUE)
script_path <- if (length(file_argument) > 0L) {
  normalizePath(sub("^--file=", "", file_argument[[1L]]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath("scripts/validate_latent_mixture_selection.R", winslash = "/", mustWork = TRUE)
}
repository_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(
  repository_root,
  "modules", "latent_mplus", "app", "R", "common", "15_mixture_selection_core.R"
), local = FALSE)

assert_true <- function(value, message) {
  if (!isTRUE(value)) stop(message, call. = FALSE)
}

expect_condition <- function(expression, expected_class) {
  captured <- tryCatch(
    {
      force(expression)
      NULL
    },
    error = function(error) error
  )
  assert_true(inherits(captured, expected_class), paste0("Expected condition class: ", expected_class))
  captured
}

has_reason <- function(data, reason) {
  grepl(paste0("(^|;)", reason, "(;|$)"), data$failure_reasons, perl = TRUE)
}

base_candidates <- data.frame(
  model_tag = c(
    "fixture_model2_k2_main", "fixture_model2_k3_main", "fixture_model3_k4_main"
  ),
  k = c(2L, 3L, 4L),
  model_structure = c("model2", "model2", "model3"),
  status = c("ok", "ok", "ok"),
  parse_ok = c(TRUE, TRUE, TRUE),
  terminated_normally = c(TRUE, TRUE, TRUE),
  converged = c(TRUE, TRUE, TRUE),
  loglik_replicated = c(TRUE, TRUE, TRUE),
  admissible = c(TRUE, TRUE, TRUE),
  admissibility_reasons = c("", "", ""),
  ll = c(-520, -480, -450),
  npar = c(12, 18, 24),
  aic = c(1064, 996, 948),
  bic = c(110, 100, 80),
  sabic = c(108, 100, 82),
  dbic = c(112, 102, 84),
  caic = c(115, 105, 87),
  entropy = c(0.75, 0.88, 0.55),
  smallest_class_p = c(0.12, 0.08, 0.07),
  smallest_class_n = c(120, 80, 70),
  stringsAsFactors = FALSE
)

annotate <- function(data, rule = "hybrid", min_prop = 0.03, min_n = 20) {
  mixture_annotate_eligibility(
    data,
    rule = rule,
    min_class_prop = min_prop,
    min_class_n = min_n,
    min_entropy_hard = 0.60
  )
}

# Parser quality is based on explicit termination and replication evidence.
quality_ok <- mixture_parse_run_quality(c(
  "THE MODEL ESTIMATION TERMINATED NORMALLY",
  "THE BEST LOGLIKELIHOOD VALUE WAS REPLICATED.",
  "ENDING TIME: 12:00"
), k = 3L)
assert_true(quality_ok$converged, "Normal termination was not recognized.")
assert_true(quality_ok$run_quality_ok, "A replicated, converged run must pass run quality.")

quality_not_replicated <- mixture_parse_run_quality(c(
  "THE MODEL ESTIMATION TERMINATED NORMALLY",
  "THE BEST LOGLIKELIHOOD VALUE HAS NOT BEEN REPLICATED. SOLUTION MAY NOT BE TRUSTWORTHY DUE TO LOCAL MAXIMA."
), k = 3L)
assert_true(quality_not_replicated$converged, "Replication failure must not erase convergence state.")
assert_true(identical(quality_not_replicated$best_ll_replicated, FALSE), "Non-replication was not recognized.")
assert_true(!quality_not_replicated$run_quality_ok, "A non-replicated mixture solution must fail run quality.")
assert_true(identical(quality_not_replicated$status, "failed"), "Non-replication must produce failed run status.")
assert_true(quality_not_replicated$local_maxima_warning, "Local-maxima risk was not retained for a non-replicated optimum.")

quality_local_maxima_heading <- mixture_parse_run_quality(c(
  "THE MODEL ESTIMATION TERMINATED NORMALLY",
  "THE BEST LOGLIKELIHOOD VALUE WAS REPLICATED.",
  "FINAL STAGE LOGLIKELIHOOD VALUES AT LOCAL MAXIMA"
), k = 3L)
assert_true(
  !quality_local_maxima_heading$local_maxima_warning,
  "The standard local-maxima listing heading was mislabeled as a warning."
)

quality_tech14 <- mixture_parse_run_quality(c(
  "THE MODEL ESTIMATION TERMINATED NORMALLY",
  "THE BEST LOGLIKELIHOOD VALUE WAS REPLICATED.",
  "THE BEST LOGLIKELIHOOD VALUE FOR THE H0 MODEL WAS NOT REPLICATED IN 2 OUT OF 20 BOOTSTRAP DRAWS."
), k = 3L)
assert_true(
  identical(quality_tech14$best_ll_replicated, TRUE),
  "A TECH14 bootstrap warning was mistaken for failure to replicate the main solution."
)

quality_tech14_only <- mixture_parse_run_quality(c(
  "THE MODEL ESTIMATION TERMINATED NORMALLY",
  "THE BEST LOGLIKELIHOOD VALUE FOR THE H0 MODEL WAS NOT REPLICATED IN 2 OUT OF 20 BOOTSTRAP DRAWS."
), k = 3L)
assert_true(
  is.na(quality_tech14_only$best_ll_replicated),
  "TECH14-only replication text must not establish main-solution replication."
)
assert_true(!quality_tech14_only$run_quality_ok, "Missing main-solution replication evidence must fail closed.")

quality_ending_only <- mixture_parse_run_quality("ENDING TIME: 12:00", k = 3L)
assert_true(!quality_ending_only$converged, "ENDING TIME alone must not imply convergence.")

quality_partial_starts <- mixture_parse_run_quality(c(
  "THE MODEL ESTIMATION TERMINATED NORMALLY",
  "THE BEST LOGLIKELIHOOD VALUE WAS REPLICATED.",
  "1 PERTURBED STARTING VALUE RUN(S) DID NOT CONVERGE OR WERE REJECTED IN THE THIRD STAGE."
), k = 3L)
assert_true(
  quality_partial_starts$run_quality_ok,
  "A failed random start was mistaken for failure of the final replicated solution."
)

quality_fatal <- mixture_parse_run_quality(c(
  "*** ERROR in MODEL command",
  "THE MODEL ESTIMATION TERMINATED NORMALLY",
  "THE BEST LOGLIKELIHOOD VALUE WAS REPLICATED."
), k = 3L)
assert_true(!quality_fatal$converged, "Fatal output must fail convergence even if a normal phrase remains.")

quality_k1 <- mixture_parse_run_quality("THE MODEL ESTIMATION TERMINATED NORMALLY", k = 1L)
assert_true(quality_k1$run_quality_ok, "A one-class model must not require random-start replication.")

# Read only the first class-count block; do not absorb later posterior matrices.
class_block <- mixture_parse_class_count_block(c(
  "FINAL CLASS COUNTS AND PROPORTIONS FOR THE LATENT CLASSES",
  "BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP",
  "",
  "1 60.000 0.60000",
  "2 40.000 0.40000",
  "",
  "Average Latent Class Probabilities for Most Likely Latent Class Membership",
  "1 0.900 0.100",
  "2 0.200 0.800"
), expected_k = 2L)
assert_true(class_block$complete, "The exact two-class count block was not recognized.")
assert_true(nrow(class_block$data) == 2L, "Posterior-probability rows leaked into class counts.")
assert_true(isTRUE(all.equal(class_block$smallest_class_n, 40)), "Smallest class count is incorrect.")
assert_true(isTRUE(all.equal(class_block$smallest_class_p, 0.4)), "Smallest class proportion is incorrect.")

incomplete_class_block <- mixture_parse_class_count_block(c(
  "BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP",
  "1 100 1.000"
), expected_k = 2L)
assert_true(!incomplete_class_block$complete, "An incomplete class-count block passed as complete.")

inconsistent_class_block <- mixture_parse_class_count_block(c(
  "BASED ON THEIR MOST LIKELY LATENT CLASS MEMBERSHIP",
  "1 100 0.800",
  "2 100 0.900"
), expected_k = 2L)
assert_true(!inconsistent_class_block$complete, "Inconsistent class counts and proportions passed as complete.")

cprob_fixture <- data.frame(
  V1 = c(0, 1, 1, 0, 1, 0),
  V2 = c(1, 1, 0, 0, 1, 0),
  V3 = c(0.90, 0.80, 0.70, 0.20, 0.10, 0.40),
  V4 = c(0.10, 0.20, 0.30, 0.80, 0.90, 0.60),
  V5 = c(1, 1, 1, 2, 2, 2),
  V6 = c(1, 2, 1, 2, 1, 2)
)
cprob_block <- mixture_detect_generic_cprob_block(cprob_fixture, k = 2L, expected_start = 3L)
assert_true(cprob_block$found && !cprob_block$ambiguous, "The unique CPROB block was not detected.")
assert_true(identical(cprob_block$posterior_cols, c("V3", "V4")), "Binary indicators were mistaken for posterior probabilities.")
assert_true(identical(cprob_block$class_col, "V5"), "A later binary strata column was mistaken for the class column.")

cprob_spoof <- data.frame(
  V1 = c(1, 1, 0, 0),
  V2 = c(0, 0, 1, 1),
  V3 = c(1, 1, 2, 2)
)
cprob_spoof_result <- mixture_detect_generic_cprob_block(cprob_spoof, k = 2L, expected_start = 2L)
assert_true(!cprob_spoof_result$found, "A one-hot observed-variable block was accepted outside the registered CPROB position.")

signature_path <- tempfile(fileext = ".out")
writeLines("first run", signature_path)
signature <- mixture_file_signature(signature_path)
assert_true(
  mixture_file_signature_matches(signature_path, signature$size, signature$mtime, signature$md5),
  "A newly captured file signature did not verify."
)
writeLines("replacement run with different content", signature_path)
assert_true(
  !mixture_file_signature_matches(signature_path, signature$size, signature$mtime, signature$md5),
  "A replaced file retained a stale execution signature."
)
unlink(signature_path)

registry_fixture <- list(
  list(model_tag = "m2", k = 2L, usevariables = c("x1", "x2"), id_var = "id"),
  list(model_tag = "m3", k = 3L, usevariables = c("x1", "x2", "x3"), id_var = "id")
)
registry_rows <- mixture_registry_to_row_df(registry_fixture)
assert_true(nrow(registry_rows) == 2L, "Rich registry rows were expanded by vector-valued fields.")
assert_true(is.list(registry_rows$usevariables), "Registry vector fields were not preserved as list columns.")
assert_true(identical(registry_rows$usevariables[[2L]], c("x1", "x2", "x3")), "Registry vector content was lost.")

admissibility_cases <- list(
  list(
    text = "THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES COULD NOT BE COMPUTED.",
    reason = "standard_errors_not_computed"
  ),
  list(
    text = "THE STANDARD ERRORS OF THE MODEL PARAMETER ESTIMATES MAY NOT BE TRUSTWORTHY DUE TO A NON-POSITIVE DEFINITE FIRST-ORDER DERIVATIVE PRODUCT MATRIX.",
    reason = "standard_errors_untrustworthy"
  ),
  list(
    text = "THE LATENT VARIABLE COVARIANCE MATRIX (PSI) IS NOT POSITIVE DEFINITE.",
    reason = "nonpositive_definite"
  ),
  list(
    text = "A SINGULARITY OF THE INFORMATION MATRIX WAS DETECTED.",
    reason = "information_matrix_singular"
  ),
  list(
    text = "THE INFORMATION MATRIX COULD NOT BE INVERTED.",
    reason = "matrix_not_invertible"
  )
)
for (case in admissibility_cases) {
  quality <- mixture_parse_run_quality(c(
    "THE MODEL ESTIMATION TERMINATED NORMALLY",
    "THE BEST LOGLIKELIHOOD VALUE WAS REPLICATED.",
    case$text
  ), k = 3L)
  assert_true(!quality$admissible, paste0("Hard inadmissibility was not detected: ", case$reason))
  assert_true(!quality$pass_admissibility, "Parser pass_admissibility did not match admissible.")
  assert_true(case$reason %in% quality$failure_reasons, paste0("Missing parser reason: ", case$reason))
  assert_true(!quality$run_quality_ok, "An inadmissible solution passed run quality.")
  assert_true(identical(quality$status, "failed"), "An inadmissible solution retained ok status.")
}

# Each hard requirement fails closed and records an auditable reason.
flag_cases <- list(
  list(column = "status", value = "failed", reason = "status_not_ok"),
  list(column = "status", value = NA_character_, reason = "status_not_ok"),
  list(column = "parse_ok", value = FALSE, reason = "parse_failed"),
  list(column = "parse_ok", value = NA, reason = "parse_failed"),
  list(column = "converged", value = FALSE, reason = "not_converged"),
  list(column = "converged", value = NA, reason = "not_converged"),
  list(column = "loglik_replicated", value = FALSE, reason = "loglik_not_replicated"),
  list(column = "loglik_replicated", value = NA, reason = "loglik_replication_missing"),
  list(column = "admissible", value = FALSE, reason = "inadmissible_model"),
  list(column = "admissible", value = NA, reason = "admissibility_unknown"),
  list(column = "smallest_class_p", value = 0.01, reason = "class_prop_below_min"),
  list(column = "smallest_class_p", value = NA_real_, reason = "class_prop_missing"),
  list(column = "smallest_class_n", value = 10, reason = "class_n_below_min"),
  list(column = "smallest_class_n", value = NA_real_, reason = "class_n_missing")
)
for (case in flag_cases) {
  fixture <- base_candidates[1L, , drop = FALSE]
  fixture[[case$column]] <- case$value
  checked <- annotate(fixture)
  assert_true(!checked$eligible[[1L]], paste0("Candidate unexpectedly eligible after mutating ", case$column))
  assert_true(has_reason(checked, case$reason)[[1L]], paste0("Missing failure reason: ", case$reason))
}

domain_cases <- list(
  list(column = "k", value = 0L, reason = "invalid_k"),
  list(column = "k", value = 1.5, reason = "invalid_k"),
  list(column = "entropy", value = 3, reason = "entropy_out_of_range"),
  list(column = "smallest_class_p", value = 101, reason = "class_prop_out_of_range"),
  list(column = "smallest_class_n", value = -1, reason = "class_n_invalid"),
  list(column = "parse_ok", value = 2, reason = "parse_failed"),
  list(column = "converged", value = -1, reason = "not_converged")
)
for (case in domain_cases) {
  fixture <- base_candidates[1L, , drop = FALSE]
  fixture[[case$column]] <- case$value
  checked <- annotate(fixture)
  assert_true(!checked$eligible[[1L]], paste0("Out-of-domain value passed: ", case$column))
  assert_true(has_reason(checked, case$reason)[[1L]], paste0("Missing domain failure reason: ", case$reason))
}

# Detailed parser diagnostics survive eligibility annotation and remain visible in T2.
detailed_reason_fixture <- base_candidates[1L, , drop = FALSE]
detailed_reason_fixture$admissible <- FALSE
detailed_reason_fixture$failure_reasons <- "nonpositive_definite"
detailed_reason_checked <- annotate(detailed_reason_fixture)
assert_true(
  has_reason(detailed_reason_checked, "nonpositive_definite")[[1L]],
  "Eligibility annotation discarded the parser's detailed failure reason."
)
assert_true(
  has_reason(detailed_reason_checked, "inadmissible_model")[[1L]],
  "Eligibility annotation omitted its admissibility gate reason."
)

# Percentage and proportion class-size inputs normalize to the same scale.
percentage_fixture <- base_candidates[1L, , drop = FALSE]
percentage_fixture$smallest_class_p <- 5
percentage_checked <- annotate(percentage_fixture)
assert_true(isTRUE(all.equal(percentage_checked$min_class_prop[[1L]], 0.05)), "Percent class size did not normalize.")
assert_true(percentage_checked$eligible[[1L]], "A normalized 5% class should pass a 3% cutoff.")

# k=1 explicitly does not require replication; k>1 does.
k1_fixture <- base_candidates[1L, , drop = FALSE]
k1_fixture$k <- 1L
k1_fixture$model_tag <- "fixture_model2_k1_main"
k1_fixture$loglik_replicated <- NA
k1_fixture$entropy <- NA_real_
k1_checked <- annotate(k1_fixture)
assert_true(k1_checked$eligible[[1L]], "k=1 should not require replication or entropy evidence.")
k1_result <- mixture_select_candidate(k1_checked, mode = "auto", rule = "hybrid")
assert_true(k1_result$selected$k[[1L]] == 1L, "Hybrid selection could not retain an eligible k=1 model.")

# Invalid lowest-BIC candidate is excluded rather than silently selected.
bic_checked <- annotate(base_candidates, rule = "bic")
bic_result <- mixture_select_candidate(bic_checked, mode = "auto", rule = "bic")
assert_true(bic_result$selected$k[[1L]] == 3L, "Auto BIC selected an ineligible lower-BIC solution.")

# Hybrid selection uses only complete, eligible solutions.
hybrid_checked <- annotate(base_candidates, rule = "hybrid")
hybrid_result <- mixture_select_candidate(hybrid_checked, mode = "auto", rule = "hybrid")
assert_true(hybrid_result$selected$k[[1L]] == 3L, "Hybrid selection did not choose the intended eligible solution.")
assert_true(identical(hybrid_result$reason, "auto:hybrid"), "Hybrid reason metadata is incorrect.")

# Fixed and UI-manual semantics both honor the requested eligible k.
fixed_result <- mixture_select_candidate(hybrid_checked, mode = "fixed", rule = "hybrid", fixed_k = 2L)
assert_true(fixed_result$selected$k[[1L]] == 2L, "Fixed selection ignored fixed_k.")
manual_result <- mixture_select_candidate(hybrid_checked, mode = "manual", rule = "hybrid", fixed_k = 2L)
assert_true(manual_result$selected$k[[1L]] == 2L, "Manual alias ignored fixed_k.")
assert_true(identical(manual_result$mode, "fixed"), "Manual mode did not normalize to fixed.")

# A fixed request can neither override ineligibility nor select a missing k.
fixed_ineligible <- expect_condition(
  mixture_select_candidate(hybrid_checked, mode = "fixed", rule = "hybrid", fixed_k = 4L),
  "mixture_fixed_candidate_ineligible"
)
assert_true(nrow(fixed_ineligible$diagnostics) == 1L, "Fixed ineligibility diagnostics were not retained.")
invisible(expect_condition(
  mixture_select_candidate(hybrid_checked, mode = "fixed", rule = "hybrid", fixed_k = 9L),
  "mixture_fixed_candidate_not_found"
))

# No eligible candidate is a terminal condition; there is no raw fallback.
all_failed <- base_candidates
all_failed$status <- "failed"
all_failed_checked <- annotate(all_failed)
no_eligible <- expect_condition(
  mixture_select_candidate(all_failed_checked, mode = "auto", rule = "hybrid"),
  "mixture_no_eligible_candidates"
)
assert_true(nrow(no_eligible$diagnostics) == nrow(all_failed), "No-eligible diagnostics lost raw candidates.")

# Missing rule metrics fail before any arbitrary row can be selected.
missing_metric <- base_candidates[1:2, , drop = FALSE]
missing_metric$bic <- NA_real_
missing_metric_checked <- annotate(missing_metric, rule = "bic")
invisible(expect_condition(
  mixture_select_candidate(missing_metric_checked, mode = "auto", rule = "bic"),
  "mixture_no_eligible_candidates"
))

# Fully tied candidates are deterministic by k, structure, and tag.
tied <- base_candidates[rep(1L, 3L), , drop = FALSE]
tied$model_tag <- c("fixture_model3_k3_z", "fixture_model2_k2_b", "fixture_model2_k2_a")
tied$k <- c(3L, 2L, 2L)
tied$model_structure <- c("model3", "model2", "model2")
tied$smallest_class_p <- 0.10
tied$smallest_class_n <- 100
tied_checked <- annotate(tied, rule = "hybrid")
tied_result <- mixture_select_candidate(tied_checked, mode = "auto", rule = "hybrid")
assert_true(
  identical(tied_result$selected$model_tag[[1L]], "fixture_model2_k2_a"),
  "Hybrid ties were not resolved deterministically."
)

cat("latent mixture selection validation PASS\n")
