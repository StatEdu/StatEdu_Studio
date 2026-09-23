# CFA/SEM canvas report and reproducibility export helpers.

structural_canvas_analysis_context <- function(bundle) {
  if (isTRUE(bundle$modified_from_baseline)) {
    return("Exploratory modified model; selected using the analyzed data and requiring independent validation.")
  }
  "Prespecified/original model."
}

structural_canvas_sha256 <- function(value) {
  if (!requireNamespace("digest", quietly = TRUE)) return(NA_character_)
  digest::digest(value, algo = "sha256", serialize = TRUE)
}

structural_canvas_data_fingerprint <- function(data) {
  if (!is.data.frame(data)) return(list(available = FALSE, reason = "Data frame not retained in the analysis bundle."))
  list(
    available = TRUE,
    rows = nrow(data),
    columns = ncol(data),
    column_names = names(data),
    column_classes = vapply(data, function(value) paste(class(value), collapse = "/"), character(1)),
    content_sha256 = structural_canvas_sha256(data),
    hash_scope = "Serialized analysis values, row order, column names, classes, and attributes"
  )
}

structural_canvas_audit_valid_positions <- function(value, inline_limit = 1000L) {
  value <- as.character(value %||% character(0))
  inline_limit <- suppressWarnings(as.integer(inline_limit %||% 1000L))
  if (!is.finite(inline_limit) || inline_limit < 0L) inline_limit <- 1000L
  if (length(value) <= inline_limit) {
    numeric_value <- suppressWarnings(as.integer(value))
    if (length(value) && all(is.finite(numeric_value))) return(numeric_value)
    return(value)
  }
  numeric_value <- suppressWarnings(as.integer(value))
  numeric_available <- length(value) > 0L && all(is.finite(numeric_value))
  list(
    compacted = TRUE,
    count = length(value),
    first = if (length(value)) value[[1L]] else NULL,
    last = if (length(value)) value[[length(value)]] else NULL,
    minimum = if (numeric_available) min(numeric_value) else NULL,
    maximum = if (numeric_available) max(numeric_value) else NULL,
    contiguous_in_recorded_order = if (numeric_available && length(numeric_value) > 1L) {
      all(diff(numeric_value) == 1L)
    } else if (numeric_available) TRUE else NULL,
    sha256 = structural_canvas_sha256(value),
    note = paste0(
      "The accepted-position vector exceeded ", inline_limit,
      " entries and was replaced by a count/range/hash summary to bound audit JSON size."
    )
  )
}

structural_canvas_pls_modmed_audit_summary <- function(
  result = NULL, bootstrap = NULL, error = NULL
) {
  result <- result %||% list()
  bootstrap <- bootstrap %||% list()
  error <- trimws(paste(as.character(error %||% ""), collapse = " "))
  interaction_effects <- result$interaction_effects %||%
    bootstrap$bootstrapped_moderation_effects %||% data.frame()
  simple_slopes <- result$simple_slopes %||%
    bootstrap$bootstrapped_moderation_simple_slopes %||% data.frame()
  moderated_mediation <- result$moderated_mediation %||% data.frame()
  conditional_indirect <- result$conditional_indirect %||% data.frame()
  definitions <- result$definitions %||%
    bootstrap$statedu_moderation_definitions %||% data.frame()
  recorded <- any(vapply(
    list(interaction_effects, simple_slopes, moderated_mediation, conditional_indirect),
    function(value) is.data.frame(value) && nrow(value) > 0L,
    logical(1)
  )) || length(definitions) > 0L || nzchar(error)
  if (!recorded) return(NULL)
  valid_positions <- result$valid_positions %||% bootstrap$valid_positions %||% character(0)
  list(
    type = result$type %||% "pls_latent_moderation",
    estimator = result$estimator %||% NULL,
    definitions = definitions,
    interaction_effects = interaction_effects,
    simple_slopes = simple_slopes,
    moderated_mediation_indices = moderated_mediation,
    conditional_indirect_effects = conditional_indirect,
    validity_gate = result$validity_gate %||% list(
      minimum_valid_ratio = bootstrap$minimum_valid_ratio %||% .80,
      valid = bootstrap$nboot %||% NULL,
      requested = bootstrap$requested_nboot %||% NULL,
      ratio = bootstrap$valid_ratio %||% NULL,
      passed = bootstrap$inference_available %||% NULL,
      status = bootstrap$bootstrap_status %||% NULL
    ),
    accepted_positions = structural_canvas_audit_valid_positions(valid_positions),
    seed = result$seed %||% bootstrap$seed %||% NULL,
    inference_available = result$inference_available %||%
      bootstrap$inference_available %||% NULL,
    metadata = result$metadata %||%
      bootstrap$statedu_moderation_bootstrap_contract %||% NULL,
    error = if (nzchar(error)) error else NULL,
    excluded_payloads = c(
      "draws", "statedu_boot_paths", "statedu_moderation_draws",
      "fit", "fitted model", "raw data"
    )
  )
}

structural_canvas_pls_modmed_mga_audit_summary <- function(result = NULL) {
  result <- result %||% list()
  if (!is.list(result) || !length(result)) return(NULL)
  group_results <- result$group_results %||% list()
  compact_groups <- lapply(group_results, function(value) {
    structural_canvas_pls_modmed_audit_summary(value, bootstrap = list())
  })
  compact_groups <- Filter(Negate(is.null), compact_groups)
  list(
    type = result$type %||% "pls_moderated_mediation_mga",
    group = result$group %||% NULL,
    groups = result$groups %||% names(group_results),
    estimator = result$estimator %||% NULL,
    group_effects = result$group_effects %||% data.frame(),
    group_summaries = compact_groups,
    pairwise_differences = result$pairwise_differences %||% data.frame(),
    pairwise_validity = result$pairwise_validity %||% data.frame(),
    micom_gate = result$micom_gate %||% NULL,
    validity_gate = result$validity_gate %||% NULL,
    inference_available = result$inference_available %||% NULL,
    status = result$status %||% NULL,
    reason = result$reason %||% NULL,
    seed = result$seed %||% NULL,
    omnibus_status = result$omnibus_status %||% "not_provided",
    metadata = result$metadata %||% NULL,
    excluded_payloads = c("group draw registries", "fit", "raw data")
  )
}

structural_canvas_pls_mga_audit_summary <- function(result = NULL) {
  result <- result %||% list()
  if (!is.list(result) || !length(result)) return(NULL)
  modmed <- result$pls_modmed_mga %||% NULL
  list(
    type = result$type %||% "pls_mga_effects",
    group = result$group %||% NULL,
    groups = result$groups %||% NULL,
    estimator = result$estimator %||% NULL,
    bootstrap_replicates = result$bootstrap_reps_requested %||% NULL,
    seed = result$seed %||% NULL,
    group_seeds = result$group_seeds %||% NULL,
    missing_data_policy = result$missing_policy %||% NULL,
    estimand_basis = result$estimand_basis %||% NULL,
    path_scope = result$path_scope %||% "all",
    requested_path_ids = result$requested_path_ids %||% character(0),
    selected_path_registry = result$selected_path_registry %||% data.frame(),
    direct_path_selection_policy = result$direct_path_selection_policy %||% NULL,
    micom_gate = result$micom_gate %||% NULL,
    validity_gate = result$validity_gate %||% NULL,
    group_diagnostics = result$group_diagnostics %||% data.frame(),
    group_effects = result$group_effects %||% data.frame(),
    pairwise_differences = result$pairwise_differences %||% data.frame(),
    inference_available = result$inference_available %||% NULL,
    status = result$status %||% NULL,
    reason = result$reason %||% NULL,
    omnibus_status = result$omnibus_status %||% "not_provided",
    metadata = result$metadata %||% NULL,
    moderated_mediation = structural_canvas_pls_modmed_mga_audit_summary(modmed),
    excluded_payloads = c("family duplicates", "bootstrap draw registries", "fit", "raw data")
  )
}

structural_canvas_pls_micom_audit_summary <- function(result = NULL) {
  result <- result %||% list()
  if (!is.list(result) || !identical(result$type %||% "", "pls_micom")) return(NULL)
  list(
    type = result$type,
    group = result$group %||% NULL,
    groups = result$groups %||% NULL,
    estimator = result$estimator %||% NULL,
    estimand = result$estimand %||% NULL,
    method_scope = result$method_scope %||% NULL,
    path_scope = result$path_scope %||% "all",
    requested_path_ids = result$requested_path_ids %||% character(0),
    selected_path_registry = result$selected_path_registry %||% data.frame(),
    direct_path_selection_policy = result$direct_path_selection_policy %||% NULL,
    configural_invariance = result$configural_invariance %||% NULL,
    configural_audit = result$configural_audit %||% data.frame(),
    configural_invariance_policy = result$configural_invariance_policy %||% NULL,
    group_diagnostics = result$group_diagnostics %||% data.frame(),
    steps_2_3 = result$table %||% data.frame(),
    measurement_gate = result$measurement_gate %||% NULL,
    pairwise_gate = result$pairwise_gate %||% data.frame(),
    direct_path_permutation_sensitivity = result$mga_table %||% data.frame(),
    direct_path_permutation_status = result$mga_status %||% NULL,
    multiple_testing = result$multiple_testing %||% NULL,
    missing_data_policy = result$missing_data_policy %||% NULL,
    stage3_score_source = result$stage3_score_source %||% NULL,
    permutation_design = result$permutation_design %||% NULL,
    observations_used = result$observations_used %||% NULL,
    observations_excluded_missing_group = result$observations_excluded_missing_group %||% NULL,
    permutations_requested = result$permutations_requested %||% NULL,
    permutations_valid = result$permutations_valid %||% NULL,
    permutations_valid_by_pair = result$permutations_valid_by_pair %||% NULL,
    permutation_valid_ratio = result$permutation_valid_ratio %||% NULL,
    permutation_valid_ratio_by_pair = result$permutation_valid_ratio_by_pair %||% NULL,
    minimum_valid_ratio = result$minimum_valid_ratio %||% NULL,
    seed = result$seed %||% NULL,
    excluded_payloads = c("PLS-MGA nested results", "fit", "raw data", "permutation draws")
  )
}

structural_canvas_structural_group_comparison_export <- function(bundle) {
  result <- bundle$invariance_result %||% NULL
  if (!is.list(result) || !identical(result$type %||% "", "structural_path_comparison")) return(NULL)
  if (exists("structural_canvas_enforce_product_factor_joint_gate", mode = "function")) {
    result <- structural_canvas_enforce_product_factor_joint_gate(result)
  }
  measurement <- result$measurement_invariance %||% list()
  gate <- structural_canvas_normalize_metric_invariance_gate(
    result$measurement_gate %||% list(), measurement
  )
  list(
    enabled = TRUE,
    grouping_variable = bundle$invariance_group %||% result$group %||% NULL,
    path_scope = result$path_scope %||% "all",
    requested_path_ids = result$requested_path_ids %||% character(0),
    selected_path_registry = result$selected_path_registry %||% data.frame(),
    resolved_path_keys = result$resolved_path_keys %||% character(0),
    unselected_group_partial = result$unselected_group_partial %||% character(0),
    constraint_df_audit = result$constraint_df_audit %||% NULL,
    measurement_gate = gate,
    measurement_gate_reason_en = structural_canvas_metric_invariance_gate_reason(gate, "en"),
    measurement_model_comparison = measurement$table %||% data.frame(),
    structural_model_comparison = result$table %||% data.frame(),
    group_diagnostics = result$group_diagnostics %||% data.frame(),
    group_path_estimates = result$path_estimates %||% data.frame(),
    formal_path_equality_tests = result$formal_path_tests %||% data.frame(),
    pairwise_path_differences = result$path_differences %||% data.frame(),
    interaction_group_estimates = result$interaction_group_estimates %||% data.frame(),
    interaction_omnibus_tests = result$interaction_omnibus_tests %||% data.frame(),
    interaction_pairwise_differences = result$interaction_pairwise_differences %||% data.frame(),
    moderated_mediation_group_indices = result$moderated_mediation_group_indices %||% data.frame(),
    moderated_mediation_delta_tests = result$moderated_mediation_delta_tests %||% data.frame(),
    moderated_mediation_pairwise_differences = result$moderated_mediation_pairwise_differences %||% data.frame(),
    moderated_mediation_unsupported_paths = result$moderated_mediation_unsupported_paths %||% character(0),
    moderated_mediation_bootstrap_diagnostics = result$moderated_mediation_bootstrap_diagnostics %||% data.frame(),
    bootstrap_execution = list(
      requested = isTRUE(bundle$multigroup_moderation_bootstrap_requested),
      pending = isTRUE(bundle$multigroup_moderation_bootstrap_pending),
      canceled = isTRUE(bundle$multigroup_moderation_bootstrap_canceled),
      error = bundle$multigroup_moderation_bootstrap_error %||% "",
      blocked_reason = bundle$multigroup_moderation_bootstrap_blocked_reason %||% ""
    ),
    product_indicator_policy = result$product_indicator_policy %||% NULL,
    product_indicator_audit = result$product_indicator_audit %||% NULL,
    product_factor_joint_gate = result$product_factor_joint_gate %||% NULL,
    subtype = result$subtype %||% NULL,
    specification_policy = result$specification_policy %||% NULL,
    comparison_policy = result$comparison_policy %||% NULL,
    constraint_audit = result$constraint_audit %||% NULL,
    partial_invariance = result$partial_invariance %||%
      measurement$partial_invariance %||% structural_canvas_partial_invariance_status()
  )
}

structural_canvas_multigroup_latent_moderation_bootstrap_recorded <- function(comparison) {
  structural_canvas_multigroup_latent_moderation_bootstrap_state(comparison)$recorded
}

structural_canvas_multigroup_latent_moderation_bootstrap_state <- function(comparison) {
  component_names <- c(
    "interaction_group_estimates", "interaction_pairwise_differences",
    "moderated_mediation_group_indices", "moderated_mediation_pairwise_differences"
  )
  empty_components <- as.list(stats::setNames(rep(FALSE, length(component_names)), component_names))
  empty <- list(
    recorded = FALSE, usable = FALSE, state = "not_recorded",
    reason = "No stratified-bootstrap execution record was available.",
    diagnostic_status = NULL, finite_inference = FALSE,
    finite_components = empty_components, usable_components = empty_components
  )
  if (!is.list(comparison)) return(empty)
  execution <- comparison$bootstrap_execution %||% list()
  execution_requested <- isTRUE(execution$requested)
  execution_pending <- isTRUE(execution$pending)
  execution_canceled <- isTRUE(execution$canceled)
  execution_error <- trimws(paste(as.character(execution$error %||% ""), collapse = " "))
  execution_blocked <- trimws(paste(as.character(execution$blocked_reason %||% ""), collapse = " "))
  diagnostics <- comparison$moderated_mediation_bootstrap_diagnostics %||% data.frame()
  tables <- list(
    interaction_group_estimates = comparison$interaction_group_estimates %||% data.frame(),
    interaction_pairwise_differences = comparison$interaction_pairwise_differences %||% data.frame(),
    moderated_mediation_group_indices = comparison$moderated_mediation_group_indices %||% data.frame(),
    moderated_mediation_pairwise_differences = comparison$moderated_mediation_pairwise_differences %||% data.frame()
  )
  bootstrap_record_columns <- c(
    "Bootstrap SE", "Bootstrap CI lower", "Bootstrap CI upper",
    "Bootstrap p", "Bootstrap BH-adjusted p", "Valid replicates",
    "Requested replicates", "Bootstrap inference source"
  )
  recorded_by_table <- any(vapply(tables, function(value) {
    is.data.frame(value) && nrow(value) > 0L && any(c(
      bootstrap_record_columns
    ) %in% names(value))
  }, logical(1)))
  recorded <- execution_requested ||
    (is.data.frame(diagnostics) && nrow(diagnostics) > 0L) || recorded_by_table
  if (!recorded) return(empty)

  explicit_execution_state <- if (execution_pending) {
    c(state = "pending", reason = "The stratified bootstrap is still running.")
  } else if (execution_canceled) {
    c(state = "canceled", reason = "The stratified bootstrap was canceled by the user.")
  } else if (nzchar(execution_error)) {
    c(state = "failed", reason = paste0("The stratified bootstrap failed: ", execution_error))
  } else if (nzchar(execution_blocked)) {
    c(state = "blocked", reason = paste0("The stratified bootstrap was not run: ", execution_blocked))
  } else {
    character(0)
  }
  if (length(explicit_execution_state)) {
    return(list(
      recorded = TRUE, usable = FALSE,
      state = unname(explicit_execution_state[["state"]]),
      reason = unname(explicit_execution_state[["reason"]]),
      diagnostic_status = NULL, finite_inference = FALSE,
      finite_components = empty_components, usable_components = empty_components
    ))
  }

  logical_value <- function(value) {
    if (is.logical(value)) return(value)
    text <- tolower(trimws(as.character(value)))
    output <- rep(NA, length(text))
    output[text %in% c("true", "t", "yes", "y", "1", "usable", "available")] <- TRUE
    output[text %in% c("false", "f", "no", "n", "0", "unusable", "unavailable")] <- FALSE
    output
  }
  diagnostic_usable <- NA
  diagnostic_status <- character(0)
  if (is.data.frame(diagnostics) && nrow(diagnostics)) {
    if ("Inference usable" %in% names(diagnostics)) {
      values <- logical_value(diagnostics[["Inference usable"]])
      if (any(values %in% FALSE, na.rm = TRUE)) {
        diagnostic_usable <- FALSE
      } else if (any(values %in% TRUE, na.rm = TRUE)) {
        diagnostic_usable <- TRUE
      }
    }
    if ("Status" %in% names(diagnostics)) {
      diagnostic_status <- unique(trimws(as.character(diagnostics$Status)))
      diagnostic_status <- diagnostic_status[!is.na(diagnostic_status) & nzchar(diagnostic_status)]
      normalized <- tolower(diagnostic_status)
      unusable_status <- grepl(
        "unreliable|failed|canceled|cancelled|pending|suppressed|unavailable|not recorded|not run",
        normalized
      )
      usable_status <- normalized %in% c(
        "adequate", "caution", "available", "completed", "complete", "success", "usable"
      )
      if (any(unusable_status)) {
        diagnostic_usable <- FALSE
      } else if (is.na(diagnostic_usable) && any(usable_status)) {
        diagnostic_usable <- TRUE
      }
    }
  }

  numeric_values <- function(value) suppressWarnings(as.numeric(as.character(value)))
  finite_bootstrap_inference <- function(table) {
    if (!is.data.frame(table) || !nrow(table) || !"Bootstrap p" %in% names(table)) return(FALSE)
    interval_columns <- c("Bootstrap CI lower", "Bootstrap CI upper")
    if (!all(interval_columns %in% names(table))) return(FALSE)
    p <- numeric_values(table[["Bootstrap p"]])
    lower <- numeric_values(table[[interval_columns[[1L]]]])
    upper <- numeric_values(table[[interval_columns[[2L]]]])
    any(is.finite(lower) & is.finite(upper) & is.finite(p))
  }
  finite_components <- vapply(tables, finite_bootstrap_inference, logical(1))
  finite_inference <- any(finite_components)
  usable <- isTRUE(finite_inference) && !identical(diagnostic_usable, FALSE)
  usable_components <- finite_components & usable
  reason <- if (usable) {
    "Diagnostics permitted inference and at least one finite bootstrap CI/p result was recorded."
  } else if (identical(diagnostic_usable, FALSE)) {
    "Bootstrap execution was recorded, but diagnostics marked inference unusable."
  } else {
    "Bootstrap execution was recorded, but no finite bootstrap CI/p result was available."
  }
  list(
    recorded = TRUE,
    usable = usable,
    state = if (usable) "recorded_usable" else "recorded_unusable",
    reason = reason,
    diagnostic_status = if (length(diagnostic_status)) paste(diagnostic_status, collapse = "; ") else NULL,
    finite_inference = finite_inference,
    finite_components = as.list(finite_components),
    usable_components = as.list(usable_components)
  )
}

structural_canvas_git_provenance <- function() {
  run_git <- function(args) suppressWarnings(tryCatch(system2("git", args, stdout = TRUE, stderr = FALSE), error = function(error) character(0)))
  root <- run_git(c("rev-parse", "--show-toplevel"))
  commit <- run_git(c("rev-parse", "HEAD"))
  branch <- run_git(c("rev-parse", "--abbrev-ref", "HEAD"))
  status <- run_git(c("status", "--porcelain"))
  list(
    available = length(commit) > 0L,
    repository_root = if (length(root)) normalizePath(root[[1L]], winslash = "/", mustWork = FALSE) else NULL,
    commit = if (length(commit)) commit[[1L]] else NULL,
    branch = if (length(branch)) branch[[1L]] else NULL,
    dirty = if (length(commit)) length(status) > 0L else NULL,
    changed_entry_count = if (length(commit)) length(status) else NULL
  )
}

structural_canvas_analysis_code_fingerprint <- function() {
  function_names <- c(
    "run_structural_canvas_analysis", "structural_canvas_execute_analysis", "structural_canvas_execute_settings",
    "structural_canvas_sampling_design_gate", "structural_canvas_bootstrap_quantile_type",
    "structural_canvas_lavaan_syntax", "structural_canvas_run_pls_analysis", "structural_canvas_apply_plsc",
    "structural_canvas_run_pls_predict", "structural_canvas_pls_predict_repetition",
    "structural_canvas_pls_predict_estimate_fold", "structural_canvas_pls_predict_fold_data",
    "structural_canvas_pls_predict_summary", "structural_canvas_effect_bootstrap",
    "structural_canvas_rng_streams", "structural_canvas_run_pls_bootstrap",
    "structural_canvas_run_plsc_bootstrap", "structural_canvas_pls_bootstrap_min_valid_ratio",
    "structural_canvas_pls_bootstrap_validity", "structural_canvas_pls_bootstrap_required_masks",
    "structural_canvas_pls_bootstrap_components_contract", "structural_canvas_pls_bootstrap_select_draws",
    "structural_canvas_pls_bootstrap_suppress_inference", "structural_canvas_pls_bootstrap_contract_metadata",
    "structural_canvas_pls_bootstrap_unavailable_result",
    "structural_canvas_pls_moderation_specification",
    "structural_canvas_pls_moderation_point_tables",
    "structural_canvas_pls_moderation_bootstrap_tables",
    "structural_canvas_pls_modmed_inference",
    "structural_canvas_pls_modmed_adjust",
    "structural_canvas_pls_modmed_effects",
    "structural_canvas_pls_modmed_from_bootstrap",
    "structural_canvas_pls_modmed_mga_gate",
    "structural_canvas_pls_modmed_mga_admission",
    "structural_canvas_pls_modmed_mga_family",
    "structural_canvas_pls_modmed_mga",
    "structural_canvas_pls_modmed_compact",
    "structural_canvas_pls_mga_micom_gate",
    "structural_canvas_pls_mga_pair_admission",
    "structural_canvas_pls_mga_pairwise_rows",
    "structural_canvas_pls_mga_compile",
    "structural_canvas_pls_mga_effects",
    "structural_canvas_structural_path_group_comparison",
    "structural_canvas_multigroup_path_inference",
    "structural_canvas_prepare_group_product_indicators",
    "structural_canvas_structural_moderation_terms",
    "structural_canvas_base_measurement_syntax",
    "structural_canvas_run_measurement_invariance",
    "structural_canvas_multigroup_interaction_tables",
    "structural_canvas_multigroup_modmed_specs",
    "structural_canvas_multigroup_moderated_mediation_inference",
    "structural_canvas_multigroup_moderation_bootstrap_worker",
    "structural_canvas_multigroup_moderation_bootstrap",
    "structural_canvas_run_bootstrap_components",
    "structural_canvas_apply_multigroup_moderation_bootstrap",
    "structural_canvas_metric_invariance_gate",
    "structural_canvas_metric_invariance_gate_reason"
  )
  definitions <- lapply(function_names, function(name) {
    value <- get0(name, mode = "function", inherits = TRUE)
    if (is.null(value)) NULL else paste(deparse(body(value)), collapse = "\n")
  })
  names(definitions) <- function_names
  list(sha256 = structural_canvas_sha256(definitions), included_functions = function_names[!vapply(definitions, is.null, logical(1))])
}

structural_canvas_audit_warnings <- function(bundle, analysis_type, diagnostics) {
  warnings <- list()
  add <- function(category, severity, message) {
    warnings[[length(warnings) + 1L]] <<- data.frame(Category = category, Severity = severity, Message = message, stringsAsFactors = FALSE)
  }
  if (!isTRUE(diagnostics$converged %||% FALSE)) add("Estimation", "Critical", "The fitted model did not converge.")
  if (!isTRUE(diagnostics$admissible %||% FALSE)) add("Admissibility", "Critical", paste(c("The fitted solution was not admissible.", diagnostics$admissibility_reasons %||% character(0)), collapse = " "))
  if (length(diagnostics$ignored_covariances %||% character(0))) add("Specification", "Major", "One or more covariance paths were ignored by the selected engine.")
  if (isTRUE(bundle$modified_from_baseline)) {
    gate <- bundle$mi_validation_gate %||% structural_canvas_mi_validation_gate(length(bundle$mi_history %||% list()) > 0L, bundle$mi_holdout_enabled, bundle$holdout_comparison)
    add("Model modification", "Major", paste0("The reported model was modified using the analyzed data. ", gate$label, ". External independent replication is still required for confirmatory interpretation."))
  }
  missing_diagnostics <- bundle$missing_diagnostics %||% list()
  if (isTRUE(missing_diagnostics$available) && missing_diagnostics$incomplete_n > 0L) {
    if (identical(bundle$missing %||% "", "listwise")) add("Missing data", "Advisory", paste0("Listwise deletion excluded ", missing_diagnostics$incomplete_n, " cases (", round(missing_diagnostics$incomplete_percent, 3), "%); assess selection bias and report retained versus excluded cases."))
    if (identical(bundle$missing %||% "", "fiml")) add("Missing data", "Advisory", "FIML assumes MAR conditional on modeled variables; the assumption is not empirically confirmed by choosing FIML and requires substantive justification and sensitivity analysis.")
    if (identical(bundle$missing %||% "", "pairwise")) add("Missing data", "Advisory", paste0("Pairwise missing-data handling used different case sets; minimum indicator-pair N was ", missing_diagnostics$minimum_pairwise_n, "."))
    if (identical(bundle$missing %||% "", "mean_replacement")) {
      imputed_rows <- missing_diagnostics$imputed_row_n %||% missing_diagnostics$incomplete_n %||% NA_integer_
      imputed_cells <- missing_diagnostics$imputed_cell_n %||% sum(missing_diagnostics$variables$Missing %||% NA_real_)
      total_cells <- missing_diagnostics$total_indicator_cells %||%
        ((missing_diagnostics$n %||% NA_integer_) * nrow(missing_diagnostics$variables %||% data.frame()))
      cell_percent <- missing_diagnostics$missing_cell_percent %||%
        if (is.finite(total_cells) && total_cells > 0L) 100 * imputed_cells / total_cells else NA_real_
      display_count <- function(value) if (length(value) && is.finite(value[[1L]])) as.character(value[[1L]]) else "not recorded"
      display_percent <- if (length(cell_percent) && is.finite(cell_percent[[1L]])) paste0(round(cell_percent[[1L]], 3), "%") else "percent not recorded"
      add(
        "Missing data", "Advisory",
        paste0(
          "PLS/PLSc indicator-mean replacement affected ", display_count(imputed_rows),
          " rows and ", display_count(imputed_cells), " indicator cells (", display_percent,
          "). Bootstrap resamples recompute indicator means within each resample; report missingness and assess sensitivity when material."
        )
      )
    }
  }
  sensitivity <- structural_canvas_missing_sensitivity_rows(bundle)
  if (nrow(sensitivity) && identical(sensitivity$Status[[1L]], "Review")) add("Missing data", "Major", sensitivity$Guidance[[1L]])
  normality_diagnostics <- bundle$normality_diagnostics %||% list()
  if (isTRUE(normality_diagnostics$test_flag) && identical(toupper(as.character(bundle$estimator %||% "")), "ML")) {
    add("Distribution", "Advisory", "A sample-size-sensitive Mardia omnibus test flagged nonnormality while ML was used; justify the estimator and report robust or other sensitivity results where appropriate.")
  }
  residual_diagnostics <- if (inherits(bundle$fit, "lavaan")) structural_canvas_residual_diagnostics(bundle$fit) else list(available = FALSE)
  if (isTRUE(residual_diagnostics$available) && nrow(residual_diagnostics$group_largest %||% data.frame()) > 0L) add("Local fit", "Advisory", paste0(nrow(residual_diagnostics$group_largest), " unique indicator-pair residuals exceeded the descriptive |2.58| screen; review BH screening values, estimator limitations, and substantive causes without automatic respecification."))
  higher_order_edges <- Filter(function(edge) identical(as.character(edge$pathType %||% ""), "higherOrder"), bundle$snapshot$edges %||% list())
  if (length(higher_order_edges)) add("Higher-order model", "Advisory", "A higher-order factor implies indirect general-factor effects through lower-order factors and is not equivalent to a bifactor model or proof of unidimensionality; interpret omega-h as model- and scoring-conditional.")
  factor_score_quality <- if (inherits(bundle$fit, "lavaan")) structural_canvas_factor_score_quality(bundle$fit) else data.frame()
  if (identical(bundle$objective %||% "", "scores")) {
    if (!nrow(factor_score_quality) || any(!is.finite(factor_score_quality$Determinacy))) add("Factor scores", "Major", "Construct-score use was selected but factor-score determinacy could not be established for every factor.")
    else if (any(factor_score_quality$Determinacy < .80)) add("Factor scores", "Major", "Construct-score use was selected and at least one factor-score determinacy estimate was below the descriptive .80 reference; review scoring and avoid treating scores as error-free observations.")
    else add("Factor scores", "Advisory", "Construct-score use remains conditional on the fitted model and scoring method; propagate score error and obtain separate evidence for any individual-level decisions.")
  }
  if (analysis_type %in% c("cbsem", "sem", "plssem")) add("Causal interpretation", "Advisory", "Causal identification was not established; directed coefficients and indirect effects are associational unless justified externally.")
  if (!isTRUE(bundle$sampling_design_gate$supported %||% FALSE)) add("Sampling design", "Major", "A supported independent-observation sampling-design declaration was not recorded in this bundle.")
  if (identical(bundle$power_basis %||% "not_recorded", "not_recorded")) add("Sample size", "Advisory", "No a-priori sample-size or power basis was recorded; fitted-sample significance, N-to-parameter ratios, and the PLS 10-times rule do not establish adequate power.")
  if (!identical(bundle$power_basis %||% "not_recorded", "not_recorded") && !nzchar(bundle$power_details %||% "")) add("Sample size", "Advisory", "A power-basis category was selected without assumptions, target power/precision, attrition allowance, or calculation source.")
  if (identical(bundle$objective %||% "", "confirmatory") && identical(bundle$analysis_plan_status %||% "not_recorded", "not_recorded")) add("Analysis plan", "Advisory", "Confirmatory analysis was selected but preregistration/protocol status was not recorded; do not describe hypotheses or analytic decisions as preregistered without an external record.")
  if ((bundle$analysis_plan_status %||% "not_recorded") %in% c("preregistered", "protocol_defined") && !nzchar(bundle$analysis_plan_reference %||% "")) add("Analysis plan", "Advisory", "An a-priori analysis-plan status was selected without a registration or protocol reference.")
  if (is.list(bundle$invariance_result$measurement_gate) && !isTRUE(bundle$invariance_result$measurement_gate$passed)) add("Measurement invariance", "Critical", "The engine-appropriate measurement-invariance gate did not pass; structural group comparisons must not be interpreted.")
  bootstrap_tables <- list(bundle$effect_bootstrap_result, bundle$htmt_bootstrap_result, bundle$reliability_bootstrap_result, bundle$bollen_stine_result)
  bootstrap_status <- unique(unlist(lapply(bootstrap_tables, function(value) if (is.data.frame(value) && "Status" %in% names(value)) as.character(value$Status) else character(0))))
  if (any(bootstrap_status %in% c("Caution", "Unreliable"))) add("Resampling", if ("Unreliable" %in% bootstrap_status) "Critical" else "Major", paste0("Bootstrap diagnostics include: ", paste(bootstrap_status, collapse = ", "), "."))
  pls_bootstrap <- bundle$pls_bootstrap_result %||% NULL
  if (identical(analysis_type, "plssem") && is.list(pls_bootstrap)) {
    pls_status <- as.character(pls_bootstrap$bootstrap_status %||% "")
    pls_status <- if (length(pls_status)) pls_status[[1L]] else ""
    pls_inference_unavailable <- !isTRUE(pls_bootstrap$inference_available) || identical(pls_status, "Insufficient")
    if (pls_inference_unavailable) {
      audit_count <- function(value, fallback = NA_integer_) {
        value <- suppressWarnings(as.numeric(value %||% fallback))
        if (!length(value) || !is.finite(value[[1L]])) return(as.integer(fallback))
        as.integer(value[[1L]])
      }
      display_count <- function(value) if (is.finite(value)) as.character(value) else "not recorded"
      valid_n <- audit_count(pls_bootstrap$nboot)
      requested_n <- audit_count(pls_bootstrap$requested_nboot, bundle$pls_bootstrap %||% NA_integer_)
      timeout_n <- audit_count(pls_bootstrap$timeout_failures, 0L)
      estimation_n <- audit_count(pls_bootstrap$estimation_failures, 0L)
      nonconvergence_n <- audit_count(pls_bootstrap$nonconvergence_failures, 0L)
      inadmissible_n <- audit_count(pls_bootstrap$inadmissible_failures, 0L)
      invalid_n <- audit_count(pls_bootstrap$invalid_statistic_failures, 0L)
      execution_n <- audit_count(pls_bootstrap$execution_failures, 0L)
      canceled_n <- audit_count(pls_bootstrap$canceled_failures, 0L)
      failure_message <- as.character(pls_bootstrap$failure_message %||% "")
      failure_message <- if (length(failure_message)) trimws(failure_message[[1L]]) else ""
      add(
        "Resampling", "Critical",
        paste0(
          "PLS bootstrap inference is unavailable (status ", if (nzchar(pls_status)) pls_status else "not recorded",
          "): valid ", display_count(valid_n), "/", display_count(requested_n),
          " requested draws; failures timeout=", display_count(timeout_n),
          ", estimation=", display_count(estimation_n),
          ", nonconvergence=", display_count(nonconvergence_n),
          ", inadmissible=", display_count(inadmissible_n),
          ", invalid-statistics=", display_count(invalid_n),
          ", execution=", display_count(execution_n),
          ", canceled=", display_count(canceled_n),
          if (nzchar(failure_message)) paste0("; detail: ", failure_message) else "",
          ". Bootstrap SE, CI, t, and p values must not be interpreted."
        )
      )
    }
  } else if (identical(analysis_type, "plssem")) {
    requested_n <- suppressWarnings(as.integer(bundle$pls_bootstrap %||% 0L))
    if (is.finite(requested_n) && requested_n > 0L) {
      add(
        "Resampling", "Critical",
        paste0(
          "PLS bootstrap inference is unavailable because no bootstrap result contract was recorded: valid 0/",
          requested_n, " requested draws. Bootstrap SE, CI, t, and p values must not be interpreted."
        )
      )
    }
  }
  if (is.list(bundle$pls_predict_result)) {
    predict_reps <- as.integer(bundle$pls_predict_result$reps %||% 0L)
    if (predict_reps < 5L) {
      add("Prediction", "Major", paste0("PLSpredict used ", predict_reps, " independent repetition(s), fewer than the minimum displayed option of five; predictive stability is insufficiently characterized."))
    } else if (predict_reps < 10L) {
      add("Prediction", "Advisory", paste0("PLSpredict used ", predict_reps, " independent repetitions, below the UI default and recommended stability setting of 10; report split-to-split variability and consider a 10-or-more-repetition sensitivity analysis."))
    }
  }
  if (!length(warnings)) return(data.frame(Category = character(0), Severity = character(0), Message = character(0)))
  do.call(rbind, warnings)
}

structural_canvas_audit_manifest <- function(bundle, analysis_type = NULL, generated_at = Sys.time()) {
  recommendation <- bundle$method_recommendation %||% list()
  diagnostics <- bundle$diagnostics %||% list()
  analysis_type <- as.character(analysis_type %||% if (toupper(as.character(bundle$estimator %||% "")) %in% c("PLS", "PLSC")) "plssem" else "cbsem")
  analysis_data <- bundle$analysis_data %||% data.frame()
  validation_data <- bundle$validation_data %||% data.frame()
  parameterization <- if (inherits(bundle$fit, "lavaan")) {
    as.character(lavaan::lavInspect(bundle$fit, "options")$parameterization %||% bundle$parameterization %||% "")
  } else {
    as.character(bundle$parameterization %||% "")
  }
  specification_payload <- list(
    snapshot = bundle$snapshot %||% list(),
    syntax = as.character(bundle$syntax %||% ""),
    estimator = as.character(bundle$estimator %||% ""),
    parameterization = parameterization,
    ordered = as.character(bundle$ordered %||% character(0))
  )
  specification_hash <- structural_canvas_sha256(specification_payload)
  git_provenance <- structural_canvas_git_provenance()
  code_fingerprint <- structural_canvas_analysis_code_fingerprint()
  audit_warnings <- structural_canvas_audit_warnings(bundle, analysis_type, diagnostics)
  residual_diagnostics <- if (inherits(bundle$fit, "lavaan")) structural_canvas_residual_diagnostics(bundle$fit) else list(available = FALSE)
  factor_score_quality <- if (inherits(bundle$fit, "lavaan")) structural_canvas_factor_score_quality(bundle$fit) else data.frame()
  structural_group_comparison <- structural_canvas_structural_group_comparison_export(bundle)
  latent_moderation_group_comparison <- is.list(structural_group_comparison) && (
    identical(as.character(structural_group_comparison$subtype %||% ""), "latent_product_indicator") ||
      any(vapply(c(
        "interaction_group_estimates", "interaction_omnibus_tests", "interaction_pairwise_differences",
        "moderated_mediation_group_indices", "moderated_mediation_delta_tests",
        "moderated_mediation_pairwise_differences", "moderated_mediation_bootstrap_diagnostics"
      ), function(name) {
        value <- structural_group_comparison[[name]]
        is.data.frame(value) && nrow(value) > 0L
      }, logical(1)))
  )
  latent_moderation_bootstrap_state <-
    structural_canvas_multigroup_latent_moderation_bootstrap_state(structural_group_comparison)
  latent_moderation_bootstrap_recorded <- latent_moderation_bootstrap_state$recorded
  latent_moderation_bootstrap_usable <- latent_moderation_bootstrap_state$usable
  latent_moderation_usable_components <- latent_moderation_bootstrap_state$usable_components %||% list()
  latent_moderation_index_pair_bootstrap_usable <- isTRUE(
    latent_moderation_usable_components$moderated_mediation_pairwise_differences
  )
  latent_moderation_has_index_pairwise <- is.data.frame(
    structural_group_comparison$moderated_mediation_pairwise_differences
  ) && nrow(structural_group_comparison$moderated_mediation_pairwise_differences) > 0L
  pls_modmed_summary <- if (identical(analysis_type, "plssem")) {
    structural_canvas_pls_modmed_audit_summary(
      bundle$pls_modmed_result %||% NULL,
      bundle$pls_bootstrap_result %||% NULL,
      bundle$pls_modmed_error %||% NULL
    )
  } else NULL
  pls_mga_result <- if (identical(analysis_type, "plssem")) {
    bundle$invariance_result$pls_mga %||% NULL
  } else NULL
  pls_modmed_mga_result <- if (identical(analysis_type, "plssem")) {
    bundle$invariance_result$pls_modmed_mga %||%
      pls_mga_result$pls_modmed_mga %||% NULL
  } else NULL
  pls_mga_summary <- structural_canvas_pls_mga_audit_summary(pls_mga_result)
  if (!is.null(pls_mga_summary) && is.null(pls_mga_summary$moderated_mediation)) {
    pls_mga_summary$moderated_mediation <-
      structural_canvas_pls_modmed_mga_audit_summary(pls_modmed_mga_result)
  }
  pls_micom_summary <- if (identical(analysis_type, "plssem")) {
    structural_canvas_pls_micom_audit_summary(bundle$invariance_result %||% NULL)
  } else NULL
  list(
    schema = list(name = "StatEdu SEM audit manifest", version = "1.8"),
    generated = list(
      timestamp = format(generated_at, "%Y-%m-%dT%H:%M:%S%z"),
      timezone = format(generated_at, "%Z"),
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      package_versions = list(
        lavaan = if (requireNamespace("lavaan", quietly = TRUE)) as.character(utils::packageVersion("lavaan")) else NULL,
        seminr = if (requireNamespace("seminr", quietly = TRUE)) as.character(utils::packageVersion("seminr")) else NULL,
        shiny = if (requireNamespace("shiny", quietly = TRUE)) as.character(utils::packageVersion("shiny")) else NULL,
        digest = if (requireNamespace("digest", quietly = TRUE)) as.character(utils::packageVersion("digest")) else NULL,
        jsonlite = if (requireNamespace("jsonlite", quietly = TRUE)) as.character(utils::packageVersion("jsonlite")) else NULL
      ),
      platform = R.version$platform,
      os = Sys.info()[c("sysname", "release", "version", "machine")],
      locale = Sys.getlocale(),
      rng_kind = RNGkind(),
      git = git_provenance,
      analysis_code = code_fingerprint
    ),
    analysis = list(
      type = analysis_type,
      context = structural_canvas_analysis_context(bundle),
      sampling_design = bundle$sampling_design %||% "not recorded",
      sampling_design_gate = bundle$sampling_design_gate %||% NULL,
      power_basis = bundle$power_basis %||% "not_recorded",
      power_details = bundle$power_details %||% "",
      objective = bundle$objective %||% "not recorded",
      analysis_plan = list(status = bundle$analysis_plan_status %||% "not_recorded", reference = bundle$analysis_plan_reference %||% ""),
      selected_method = bundle$selected_method %||% "not recorded",
      estimator = bundle$estimator %||% "not recorded",
      ml_likelihood_convention = if (identical(analysis_type, "plssem")) "not applicable" else structural_canvas_ml_likelihood_label(bundle),
      parameterization = if (nzchar(parameterization)) parameterization else "not applicable",
      missing = bundle$missing %||% "not recorded",
      missing_policy = if (identical(analysis_type, "plssem")) bundle$missing_diagnostics$policy %||% structural_canvas_pls_missing_policy() else NULL,
      missing_sensitivity = structural_canvas_missing_sensitivity_rows(bundle),
      latent_scaling = if (identical(analysis_type, "plssem")) {
        "PLS construct-score scaling/normalization as recorded by the fitted algorithm; marker-loading and latent-variance scaling are not applicable"
      } else if (isTRUE(bundle$std_lv)) {
        "latent variance fixed to 1"
      } else {
        "marker loading fixed to 1"
      },
      moderation_method = bundle$moderation_method %||% bundle$snapshot$moderationMethod %||% "not requested",
      ordered_indicators = bundle$ordered %||% character(0),
      n_analysis = if (is.data.frame(analysis_data)) nrow(analysis_data) else NA_integer_,
      n_validation = if (is.data.frame(validation_data)) nrow(validation_data) else 0L
    ),
    decision = list(
      recommendation_status = recommendation$status %||% "not available",
      primary_candidate = recommendation$primary %||% "not available",
      candidates = recommendation$candidates %||% data.frame(),
      construct_specification = structural_canvas_construct_specification(bundle$snapshot %||% list()),
      resolved_construct_specification = bundle$resolved_construct_specification %||% structural_canvas_resolve_construct_specification(bundle$snapshot %||% list(), analysis_type, bundle$estimator %||% NULL),
      pls_estimator_selection = if (identical(analysis_type, "plssem")) list(
        requested = diagnostics$estimator_requested %||% bundle$estimator_requested %||% NULL,
        selected = diagnostics$estimator %||% bundle$estimator %||% NULL,
        recommendation_confirmed = isTRUE(bundle$estimator_recommendation_confirmed %||% FALSE),
        mode = diagnostics$estimator_selection_mode %||% bundle$estimator_selection_mode %||% NULL,
        reason = diagnostics$estimator_selection_reason %||% bundle$estimator_selection_reason %||% NULL,
        corrected_common_factors = diagnostics$plsc_corrected_constructs %||% bundle$plsc_corrected_constructs %||% character(0),
        uncorrected_composites = diagnostics$plsc_uncorrected_composites %||% bundle$plsc_uncorrected_composites %||% character(0),
        correction_status = diagnostics$plsc_correction_status %||% bundle$plsc_correction_status %||% NULL,
        corrected_endogenous = diagnostics$plsc_corrected_endogenous %||% bundle$plsc_corrected_endogenous %||% character(0)
      ) else NULL,
      structural_effect_plan = bundle$structural_effect_plan %||% diagnostics$structural_effect_plan %||% data.frame(),
      causal_interpretation = structural_canvas_causal_interpretation(bundle$snapshot %||% list(), analysis_type),
      mi_validation_gate = bundle$mi_validation_gate %||% NULL,
      structural_multiplicity = if (analysis_type %in% c("cbsem", "sem")) list(method = "Benjamini-Hochberg", family = "Displayed direct, indirect, and total structural effects", raw_p_retained_for = "Prespecified primary hypotheses") else list(method = "Benjamini-Hochberg", families = c("Direct structural paths", "Indirect effects", "Total effects", "Outer loadings", "Outer weights"), raw_p_retained_for = "Prespecified primary hypotheses"),
      latent_moderation_multiplicity = if (latent_moderation_group_comparison) list(
        method = "Benjamini-Hochberg",
        families = c(
          "Group-specific latent-interaction bootstrap tests",
          "Latent-interaction path-by-group-pair bootstrap contrasts",
          "Group-specific moderated-mediation-index bootstrap tests",
          "Moderated-mediation-index path-by-group-pair bootstrap contrasts"
        ),
        family_scope = "Bootstrap p values are BH-adjusted separately within each of the four families; families are never pooled.",
        primary_moderated_mediation_inference = if (latent_moderation_index_pair_bootstrap_usable) {
          "Stratified bootstrap pairwise index differences"
        } else if (!latent_moderation_has_index_pairwise) {
          "Not applicable (no estimable moderated-mediation pairwise index difference was recorded)"
        } else if (latent_moderation_bootstrap_recorded) {
          "No bootstrap-primary inference (stratified bootstrap recorded; inference suppressed)"
        } else {
          "Model-based Delta/Wald inference (stratified bootstrap was not recorded)"
        },
        delta_wald_role = if (latent_moderation_index_pair_bootstrap_usable) {
          "Auxiliary sensitivity analysis"
        } else if (!latent_moderation_has_index_pairwise) {
          "Not applicable (no estimable moderated-mediation pairwise index difference was recorded)"
        } else if (latent_moderation_bootstrap_recorded) {
          "Auxiliary model-based inference; bootstrap execution was recorded but unusable"
        } else {
          "Available model-based inference; request bootstrap for product-distribution primary inference"
        }
      ) else NULL,
      pls_mga_multiplicity = if (
        identical(analysis_type, "plssem") &&
          is.list(bundle$invariance_result$pls_mga %||% NULL)
      ) list(
        methods = c("Benjamini-Hochberg", "Holm"),
        families = c("Direct effects", "Specific indirect effects", "Total indirect effects", "Total effects"),
        family_scope = "Raw, BH-adjusted, and Holm-adjusted p values are computed separately within each canonical effect family across the admitted group-pair contrasts.",
        prerequisite = "Pair-specific partial composite measurement invariance under MICOM",
        estimand_scope = "PLS composite-score effects only; PLSc/common-factor multi-group inference is blocked",
        omnibus = bundle$invariance_result$pls_mga$omnibus_status %||% "not_provided"
      ) else NULL,
      pls_latent_moderation_multiplicity = if (!is.null(pls_modmed_summary)) list(
        method = "Benjamini-Hochberg",
        families = c(
          "PLS latent-interaction effects", "PLS simple-slope probes",
          "PLS moderated-mediation indices",
          "PLS conditional indirect effects"
        ),
        family_scope = "Raw and BH-adjusted p values are computed separately within each PLS score-scale effect family.",
        multi_group_follow_up = if (!is.null(pls_mga_summary$moderated_mediation)) {
          "Pairwise interaction and moderated-mediation-index differences use separate BH and Holm families after pair-specific MICOM admission."
        } else NULL,
        plsc_policy = "For PLSc base models, latent interactions remain uncorrected composite-score interactions."
      ) else NULL
    ),
    model = list(
      specification_sha256 = specification_hash,
      canvas_snapshot = bundle$snapshot %||% list(),
      fitted_syntax = as.character(bundle$syntax %||% "")
    ),
    data_fingerprints = list(
      analysis = structural_canvas_data_fingerprint(analysis_data),
      validation = structural_canvas_data_fingerprint(validation_data)
    ),
    resampling = list(
      reproducibility_policy = list(
        requirement = "Reuse the recorded seed and RNG configuration; rerunning unchanged options without supplying the recorded seed is not a reproducibility guarantee.",
        scope = "All bootstrap, permutation, cross-validation, prediction, and holdout procedures recorded below.",
        additional_conditions = "Match the recorded data and model fingerprints, analysis-code fingerprint, package versions, and analysis settings.",
        quantile_definition = "R quantile type is recorded separately for every bootstrap CI method implemented by StatEdu; package-owned procedures retain their package-defined calculation."
      ),
      reliability = list(
        replicates = bundle$reliability_bootstrap %||% 0L,
        seed = bundle$reliability_seed %||% NULL,
        ci_method = bundle$reliability_ci_method %||% "bias_corrected",
        quantile_type = structural_canvas_bootstrap_quantile_type(bundle$reliability_ci_method %||% "bias_corrected", "reliability")
      ),
      htmt = list(
        replicates = bundle$htmt_bootstrap %||% 0L,
        seed = bundle$htmt_seed %||% NULL,
        ci_method = bundle$htmt_ci_method %||% "bias_corrected",
        quantile_type = structural_canvas_bootstrap_quantile_type(bundle$htmt_ci_method %||% "bias_corrected", "htmt")
      ),
      bollen_stine = list(replicates = bundle$bollen_stine_bootstrap %||% 0L, seed = bundle$bollen_stine_seed %||% NULL),
      structural_effects = list(
        replicates = bundle$effect_bootstrap %||% 0L,
        seed = bundle$effect_bootstrap_seed %||% NULL,
        ci_method = bundle$effect_bootstrap_ci_method %||% "bias_corrected",
        quantile_type = structural_canvas_bootstrap_quantile_type(bundle$effect_bootstrap_ci_method %||% "bias_corrected", "structural_effects"),
        interval = paste("case-resampling", bundle$effect_bootstrap_ci_method %||% "bias_corrected", "for unstandardized and standardized effects"),
        diagnostics = bundle$effect_bootstrap_result %||% NULL
      ),
      multi_group_latent_moderation = if (latent_moderation_group_comparison) list(
        enabled = isTRUE(structural_group_comparison$bootstrap_execution$requested) ||
          latent_moderation_bootstrap_recorded,
        execution = structural_group_comparison$bootstrap_execution %||% list(),
        recorded = latent_moderation_bootstrap_recorded,
        inference_usable = latent_moderation_bootstrap_usable,
        state = latent_moderation_bootstrap_state$state,
        state_reason = latent_moderation_bootstrap_state$reason,
        diagnostic_status = latent_moderation_bootstrap_state$diagnostic_status,
        finite_inference = latent_moderation_bootstrap_state$finite_inference,
        finite_components = latent_moderation_bootstrap_state$finite_components,
        usable_components = latent_moderation_usable_components,
        sampling = if (latent_moderation_bootstrap_recorded) {
          "Within-group stratified case resampling"
        } else {
          "Not run or not recorded"
        },
        primary_inference = if (latent_moderation_bootstrap_usable) {
          "Bootstrap confidence intervals and p values are primary only for components marked usable_components=TRUE"
        } else if (latent_moderation_bootstrap_recorded) {
          "Stratified bootstrap execution was recorded, but inferential confidence intervals and p values were suppressed"
        } else {
          "Model-based Delta/Wald inference only; no stratified bootstrap record is available"
        },
        diagnostics = structural_group_comparison$moderated_mediation_bootstrap_diagnostics %||% NULL,
        product_indicator_policy = structural_group_comparison$product_indicator_policy %||% NULL,
        product_indicator_audit = structural_group_comparison$product_indicator_audit %||% NULL
      ) else NULL,
      pls = list(
        replicates = bundle$pls_bootstrap %||% 0L,
        requested_replicates = bundle$pls_bootstrap_result$requested_nboot %||% bundle$pls_bootstrap %||% 0L,
        seed = bundle$pls_seed %||% NULL,
        rng = bundle$pls_bootstrap_result$rng %||% "L'Ecuyer-CMRG independent stream per requested position",
        draw_order = bundle$pls_bootstrap_result$draw_order %||% NULL,
        valid_positions = structural_canvas_audit_valid_positions(
          bundle$pls_bootstrap_result$valid_positions %||% character(0)
        ),
        valid_replicates = bundle$pls_bootstrap_result$nboot %||% NULL,
        valid_ratio = bundle$pls_bootstrap_result$valid_ratio %||% NULL,
        minimum_valid_ratio = bundle$pls_bootstrap_result$minimum_valid_ratio %||% .80,
        inference_available = bundle$pls_bootstrap_result$inference_available %||% NULL,
        status = bundle$pls_bootstrap_result$bootstrap_status %||% NULL,
        timeout_failures = bundle$pls_bootstrap_result$timeout_failures %||% NULL,
        estimation_failures = bundle$pls_bootstrap_result$estimation_failures %||% NULL,
        nonconvergence_failures = bundle$pls_bootstrap_result$nonconvergence_failures %||% NULL,
        inadmissible_failures = bundle$pls_bootstrap_result$inadmissible_failures %||% NULL,
        retained_nonpositive_definite_plsc_draws = bundle$pls_bootstrap_result$retained_nonpositive_definite_plsc_draws %||% NULL,
        invalid_statistic_failures = bundle$pls_bootstrap_result$invalid_statistic_failures %||% NULL,
        execution_failures = bundle$pls_bootstrap_result$execution_failures %||% NULL,
        canceled_failures = bundle$pls_bootstrap_result$canceled_failures %||% NULL,
        failure_message = bundle$pls_bootstrap_result$failure_message %||% NULL,
        failure_counts = list(
          timeout = bundle$pls_bootstrap_result$timeout_failures %||% NULL,
          estimation = bundle$pls_bootstrap_result$estimation_failures %||% NULL,
          nonconvergence = bundle$pls_bootstrap_result$nonconvergence_failures %||% NULL,
          inadmissible = bundle$pls_bootstrap_result$inadmissible_failures %||% NULL,
          invalid_statistics = bundle$pls_bootstrap_result$invalid_statistic_failures %||% NULL,
          execution = bundle$pls_bootstrap_result$execution_failures %||% NULL,
          canceled = bundle$pls_bootstrap_result$canceled_failures %||% NULL
        ),
        validity_contract = bundle$pls_bootstrap_result$validity_contract %||% "whole-draw finite/shape contract",
        missing_data_policy = bundle$missing_diagnostics$policy %||% structural_canvas_pls_missing_policy(),
        latent_moderation_and_moderated_mediation = pls_modmed_summary
      ),
      micom = list(
        enabled = isTRUE(bundle$invariance_enabled) && identical(analysis_type, "plssem"),
        estimator_scope = "PLS composite scores; PLSc/common-factor invariance is not supported",
        permutations = bundle$micom_permutations %||% NULL,
        seed = bundle$micom_seed %||% NULL,
        result = pls_micom_summary
      ),
      pls_multi_group = pls_mga_summary,
      pls_predict = list(
        folds = bundle$pls_predict_folds %||% NULL,
        repetitions = bundle$pls_predict_reps %||% NULL,
        seed = bundle$pls_predict_seed %||% NULL,
        rng = bundle$pls_predict_result$rng %||% NULL,
        estimator = bundle$pls_predict_result$estimator %||% NULL,
        technique = bundle$pls_predict_result$technique %||% "Direct antecedents",
        benchmark = "linear model",
        missing_preprocessing = bundle$pls_predict_result$missing_preprocessing %||% NULL,
        effective_n = bundle$pls_predict_result$effective_n %||% NULL,
        repetition_diagnostics = bundle$pls_predict_result$repetition_diagnostics %||% NULL,
        repetition_results = bundle$pls_predict_result$repetition_summaries %||% NULL
      ),
      mi_holdout = list(enabled = isTRUE(bundle$mi_holdout_enabled), fraction = bundle$mi_holdout_fraction %||% NULL, seed = bundle$mi_holdout_seed %||% NULL)
    ),
    requested_assessments = list(
      measurement_invariance = list(
        enabled = isTRUE(bundle$invariance_enabled), group = bundle$invariance_group %||% NULL,
        metric_gate = structural_group_comparison$measurement_gate %||% bundle$invariance_result$measurement_gate %||% NULL,
        measurement_table = bundle$invariance_result$measurement_invariance$table %||% bundle$invariance_result$table %||% NULL,
        partial_invariance = if (identical(analysis_type, "plssem")) {
          list(
            status = "Not applicable",
            reason = "MICOM classifies compositional plus pooled-score mean/variance invariance pair by pair; CBSEM equality-constraint freeing is not applicable.",
            measurement_gate = bundle$invariance_result$measurement_gate %||% NULL,
            pairwise_gate = bundle$invariance_result$pairwise_gate %||% NULL
          )
        } else {
          bundle$invariance_result$partial_invariance %||%
            bundle$invariance_result$measurement_invariance$partial_invariance %||%
            structural_canvas_partial_invariance_status()
        }
      ),
      structural_path_group_comparison = structural_group_comparison,
      pls_latent_moderation = pls_modmed_summary,
      pls_multi_group = pls_mga_summary,
      common_method = list(
        enabled = isTRUE(bundle$common_method_enabled),
        methods = bundle$common_method_methods %||% character(0),
        procedural_controls = bundle$common_method_procedural_controls %||% "",
        marker_variable = bundle$common_method_marker_variable %||% "",
        marker_rationale = bundle$common_method_marker_rationale %||% "",
        marker_analysis_status = "Recorded only; the current engine does not estimate or adjust for a marker-variable effect."
      ),
      formative_redundancy = list(construct = bundle$redundancy_construct %||% NULL, criterion = bundle$redundancy_criterion %||% NULL, result = bundle$redundancy_result %||% NULL),
      formative_content_validity = structural_canvas_formative_content_validity_rows(bundle$snapshot %||% list(), bundle$redundancy_result %||% NULL, bundle$redundancy_construct %||% NULL),
      parcel_preview = list(enabled = isTRUE(bundle$parcel_enabled), construct = bundle$parcel_construct %||% NULL, count = bundle$parcel_count %||% NULL, purpose = bundle$parcel_purpose %||% NULL, result = bundle$parcel_result %||% NULL)
    ),
    diagnostics = list(
      identification = bundle$identification %||% data.frame(),
      converged = diagnostics$converged %||% NA,
      identified = diagnostics$identified %||% NA,
      admissible = diagnostics$admissible %||% NA,
      admissibility_reasons = diagnostics$admissibility_reasons %||% character(0),
      ignored_covariances = diagnostics$ignored_covariances %||% character(0),
      missing_data = bundle$missing_diagnostics %||% NULL,
      multivariate_normality = bundle$normality_diagnostics %||% NULL,
      local_fit = if (isTRUE(residual_diagnostics$available)) list(cutoff = residual_diagnostics$cutoff, standardized_available = residual_diagnostics$standardized_available, group_summary = residual_diagnostics$group_summary, flagged_pairs = residual_diagnostics$group_largest) else NULL,
      factor_score_quality = if (nrow(factor_score_quality)) factor_score_quality else NULL,
      modified_from_baseline = isTRUE(bundle$modified_from_baseline),
      mi_validation_gate = bundle$mi_validation_gate %||% NULL,
      modification_history = bundle$mi_history %||% data.frame()
    ),
    warnings = audit_warnings,
    privacy = list(
      raw_data_included = FALSE,
      fitted_object_included = FALSE,
      bootstrap_draw_arrays_included = FALSE,
      data_content_hash_included = TRUE,
      note = "SHA-256 fingerprints permit equality checks without embedding raw observations, fitted objects, or bootstrap draw arrays. Long accepted-position vectors are compacted to count/range/hash summaries."
    )
  )
}

structural_canvas_write_audit_manifest <- function(bundle, file, analysis_type = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("The jsonlite package is required to export the SEM audit manifest.")
  jsonlite::write_json(
    structural_canvas_audit_manifest(bundle, analysis_type), file,
    pretty = TRUE, auto_unbox = TRUE, na = "null", null = "null", dataframe = "rows"
  )
  invisible(file)
}

structural_canvas_reproducibility_table_lines <- function(title, table) {
  heading <- c("", title, paste(rep("-", nchar(title)), collapse = ""))
  if (!is.data.frame(table) || !nrow(table)) return(c(heading, "Not available."))
  old_width <- getOption("width")
  on.exit(options(width = old_width), add = TRUE)
  options(width = max(160L, old_width %||% 80L))
  c(heading, capture.output(print(table, row.names = FALSE, right = FALSE)))
}

structural_canvas_reproducibility_object_lines <- function(title, value) {
  heading <- c("", title, paste(rep("-", nchar(title)), collapse = ""))
  if (is.null(value) || !length(value)) return(c(heading, "Not available."))
  c(heading, capture.output(str(value, give.attr = FALSE, vec.len = 100L)))
}

structural_canvas_structural_group_record_lines <- function(bundle) {
  comparison <- structural_canvas_structural_group_comparison_export(bundle)
  if (is.null(comparison)) return(character(0))
  bootstrap_state <- structural_canvas_multigroup_latent_moderation_bootstrap_state(comparison)
  usable_component_names <- names(Filter(isTRUE, bootstrap_state$usable_components %||% list()))
  index_pairwise_bootstrap_usable <- isTRUE(
    bootstrap_state$usable_components$moderated_mediation_pairwise_differences
  )
  has_index_pairwise <- is.data.frame(comparison$moderated_mediation_pairwise_differences) &&
    nrow(comparison$moderated_mediation_pairwise_differences) > 0L
  gate <- comparison$measurement_gate %||% list()
  c(
    "",
    "Multi-group structural-path comparison",
    "--------------------------------------",
    paste0("Grouping variable: ", comparison$grouping_variable %||% "not recorded"),
    paste0("Measurement gate passed: ", toupper(as.character(isTRUE(gate$passed)))),
    paste0("Measurement gate reason code: ", gate$reason_code %||% "not_recorded"),
    paste0("Measurement gate reason: ", comparison$measurement_gate_reason_en %||% "not recorded"),
    paste0("Structural-path scope: ", comparison$path_scope %||% "all"),
    paste0(
      "Requested structural-path edge IDs: ",
      if (length(comparison$requested_path_ids %||% character(0))) {
        paste(comparison$requested_path_ids, collapse = ", ")
      } else {
        "none (all eligible paths)"
      }
    ),
    paste0("Specification policy: ", comparison$specification_policy %||% "not recorded"),
    paste0("Structural comparison subtype: ", comparison$subtype %||% "standard structural paths"),
    paste0("Path-equality estimand: ", comparison$comparison_policy$estimand_statement %||% "not recorded"),
    paste0("Partial-invariance support: ", comparison$comparison_policy$partial_invariance_status %||% comparison$partial_invariance$status %||% "not recorded"),
    paste0("Stratified bootstrap recorded: ", toupper(as.character(isTRUE(bootstrap_state$recorded)))),
    paste0("Stratified bootstrap inference usable: ", toupper(as.character(isTRUE(bootstrap_state$usable)))),
    paste0("Stratified bootstrap state: ", bootstrap_state$state),
    paste0("Stratified bootstrap state reason: ", bootstrap_state$reason),
    structural_canvas_reproducibility_object_lines(
      "Multi-group latent-moderation bootstrap execution state",
      comparison$bootstrap_execution %||% list()
    ),
    paste0(
      "Stratified bootstrap usable components: ",
      if (length(usable_component_names)) paste(usable_component_names, collapse = ", ") else "none"
    ),
    structural_canvas_reproducibility_table_lines("Measurement-invariance gate models", comparison$measurement_model_comparison),
    if (identical(comparison$path_scope %||% "all", "selected")) {
      structural_canvas_reproducibility_table_lines("Selected structural-path registry", comparison$selected_path_registry)
    } else character(0),
    if (identical(comparison$path_scope %||% "all", "selected")) {
      structural_canvas_reproducibility_object_lines("Unselected regressions kept group-specific", comparison$unselected_group_partial)
    } else character(0),
    if (identical(comparison$path_scope %||% "all", "selected")) {
      structural_canvas_reproducibility_object_lines("Selected-path constraint df audit", comparison$constraint_df_audit)
    } else character(0),
    structural_canvas_reproducibility_table_lines("Free versus equal structural-path models", comparison$structural_model_comparison),
    structural_canvas_reproducibility_table_lines("Group diagnostics", comparison$group_diagnostics),
    structural_canvas_reproducibility_table_lines("Group-specific structural path estimates", comparison$group_path_estimates),
    structural_canvas_reproducibility_table_lines("Formal path-level equality tests", comparison$formal_path_equality_tests),
    structural_canvas_reproducibility_table_lines("Pairwise path differences", comparison$pairwise_path_differences),
    structural_canvas_reproducibility_table_lines("Group-specific latent interaction effects", comparison$interaction_group_estimates),
    structural_canvas_reproducibility_table_lines("Omnibus latent interaction equality tests", comparison$interaction_omnibus_tests),
    structural_canvas_reproducibility_table_lines("Pairwise latent interaction differences", comparison$interaction_pairwise_differences),
    structural_canvas_reproducibility_table_lines("Group-specific indices of moderated mediation", comparison$moderated_mediation_group_indices),
    structural_canvas_reproducibility_table_lines("Auxiliary Delta/Wald tests of moderated-mediation index equality", comparison$moderated_mediation_delta_tests),
    structural_canvas_reproducibility_table_lines(
      if (!has_index_pairwise) {
        "Moderated-mediation pairwise differences (not estimable or not requested)"
      } else if (index_pairwise_bootstrap_usable) {
        "Primary stratified-bootstrap pairwise differences in moderated-mediation indices"
      } else if (bootstrap_state$recorded) {
        "Stratified-bootstrap pairwise differences in moderated-mediation indices (execution recorded; inference suppressed)"
      } else {
        "Model-based Delta/Wald pairwise differences in moderated-mediation indices (bootstrap not recorded)"
      },
      comparison$moderated_mediation_pairwise_differences
    ),
    structural_canvas_reproducibility_table_lines("Multi-group moderated-mediation bootstrap diagnostics", comparison$moderated_mediation_bootstrap_diagnostics),
    structural_canvas_reproducibility_object_lines("Unsupported moderated-mediation paths", comparison$moderated_mediation_unsupported_paths),
    structural_canvas_reproducibility_object_lines("Product-indicator policy", comparison$product_indicator_policy),
    structural_canvas_reproducibility_object_lines("Product-indicator audit", comparison$product_indicator_audit),
    structural_canvas_reproducibility_object_lines("Joint product-factor model gate", comparison$product_factor_joint_gate),
    structural_canvas_reproducibility_object_lines("Structural comparison policy", comparison$comparison_policy),
    structural_canvas_reproducibility_object_lines("Parameter constraint audit", comparison$constraint_audit)
  )
}

structural_canvas_reproducibility_record <- function(bundle, generated_at = Sys.time()) {
  fit <- bundle$fit
  if (is.null(fit) || !inherits(fit, "lavaan")) {
    stop("The text reproducibility record requires a fitted lavaan CFA/CB-SEM object. Use the JSON audit manifest for PLS/PLSc analyses.", call. = FALSE)
  }
  options <- lavaan::lavInspect(fit, "options")
  recommendation <- bundle$method_recommendation %||% list()
  recommendation_candidates <- recommendation$candidates %||% data.frame()
  analysis_type <- as.character(bundle$analysis_type %||% "cfa")
  construct_rows <- structural_canvas_construct_reporting_rows(bundle, analysis_type, FALSE)
  construct_lines <- if (nrow(construct_rows)) apply(construct_rows, 1L, function(row) paste(row, collapse = " | ")) else "none"
  structural_group_lines <- structural_canvas_structural_group_record_lines(bundle)
  lines <- c(
    "CFA analysis reproducibility record",
    paste0("Generated: ", format(generated_at, "%Y-%m-%d %H:%M:%S %Z")),
    paste0("R version: ", paste(R.version$major, R.version$minor, sep = ".")),
    paste0("lavaan version: ", as.character(utils::packageVersion("lavaan"))),
    paste0("Analysis context: ", structural_canvas_analysis_context(bundle)),
    paste0("Estimator: ", bundle$estimator %||% options$estimator %||% ""),
    paste0("ML likelihood convention: ", structural_canvas_ml_likelihood_label(bundle)),
    paste0("Primary analysis objective: ", bundle$objective %||% "not recorded"),
    paste0("Recommended method candidate: ", recommendation$primary %||% "not available"),
    paste0("Selected method: ", bundle$selected_method %||% "not recorded"),
    paste0("Recommendation rationale: ", if (nrow(recommendation_candidates)) paste(paste0(recommendation_candidates$Method, " [", recommendation_candidates$Role, "]: ", recommendation_candidates$Reason, ifelse(nzchar(recommendation_candidates$Limitation), paste0(" Limitation: ", recommendation_candidates$Limitation), "")), collapse = " | ") else "not available"),
    paste0("Missing-data option: ", bundle$missing %||% options$missing %||% ""),
    paste0("Latent scaling: ", if (isTRUE(bundle$std_lv)) "latent variance fixed to 1" else "marker loading fixed to 1"),
    paste0("N used: ", lavaan::lavInspect(fit, "ntotal")),
    paste0("Ordered indicators: ", if (length(bundle$ordered %||% character(0))) paste(bundle$ordered, collapse = ", ") else "none"),
    paste0("AVE/CR formula: ", bundle$validity_formula %||% "standardized"),
    paste0("RMSEA CI level: ", bundle$rmsea_ci %||% .90),
    paste0("HTMT threshold: ", bundle$htmt_threshold %||% .85),
    paste0("HTMT bootstrap: ", bundle$htmt_bootstrap %||% 0L, "; seed: ", bundle$htmt_seed %||% "not used", "; CI method: ", bundle$htmt_ci_method %||% "bias_corrected", "; quantile type: R type ", structural_canvas_bootstrap_quantile_type(bundle$htmt_ci_method %||% "bias_corrected", "htmt")),
    paste0("AVE/reliability bootstrap: ", bundle$reliability_bootstrap %||% 0L, "; seed: ", bundle$reliability_seed %||% "not used", "; CI method: ", bundle$reliability_ci_method %||% "bias_corrected", "; quantile type: R type ", structural_canvas_bootstrap_quantile_type(bundle$reliability_ci_method %||% "bias_corrected", "reliability")),
    paste0("Path/indirect/total-effect bootstrap: ", bundle$effect_bootstrap %||% 0L, "; seed: ", bundle$effect_bootstrap_seed %||% "not used", "; CI method: ", bundle$effect_bootstrap_ci_method %||% "bias_corrected", "; quantile type: R type ", structural_canvas_bootstrap_quantile_type(bundle$effect_bootstrap_ci_method %||% "bias_corrected", "structural_effects")),
    paste0("Bollen-Stine bootstrap: ", bundle$bollen_stine_bootstrap %||% 0L, "; seed: ", bundle$bollen_stine_seed %||% "not used"),
    paste0("Measurement invariance: ", if (isTRUE(bundle$invariance_enabled)) paste0("enabled; group = ", bundle$invariance_group) else "disabled"),
    paste0("Common method bias diagnostics: ", if (isTRUE(bundle$common_method_enabled)) paste0("enabled; methods = ", paste(bundle$common_method_methods %||% character(0), collapse = ", ")) else "disabled"),
    paste0("Common method procedural controls: ", bundle$common_method_procedural_controls %||% "not recorded"),
    paste0("Common method marker variable: ", bundle$common_method_marker_variable %||% "not recorded", "; rationale: ", bundle$common_method_marker_rationale %||% "not recorded", "; analysis status: recorded only, not estimated"),
    paste0("Parcel item-level model: ", if (isTRUE(bundle$parcel_enabled)) paste0(
      if (isTRUE(bundle$parcel_result$applied)) "fitted" else "requested",
      "; construct = ", bundle$parcel_construct %||% "",
      "; parcels = ", bundle$parcel_count %||% "",
      "; purpose = ", bundle$parcel_purpose %||% "not recorded",
      "; data parcel variables created = no"
    ) else "disabled"),
    paste0("MI holdout validation: ", if (isTRUE(bundle$mi_holdout_enabled)) paste0("enabled; validation fraction = ", bundle$mi_holdout_fraction, "; seed = ", bundle$mi_holdout_seed, "; exploration N = ", nrow(bundle$analysis_data), "; validation N = ", nrow(bundle$validation_data)) else "disabled"),
    if (!is.null(bundle$holdout_comparison)) paste0("MI holdout N used after missing-data handling: ", paste(bundle$holdout_comparison$validation_n_used, collapse = ", ")),
    paste0("MI output mode: ", bundle$mi_mode %||% "theory"),
    paste0("Admissible solution: ", isTRUE(bundle$diagnostics$admissible)),
    paste0("Admissibility reasons: ", if (length(bundle$diagnostics$admissibility_reasons %||% character(0))) paste(bundle$diagnostics$admissibility_reasons, collapse = "; ") else "none"),
    "",
    "Construct specification (construct | declared type | measurement direction | requested weighting | effective weighting | engine representation | estimand | migration)",
    "----------------------------------------------------------------------------------------------------------------------------------------------------",
    construct_lines,
    structural_group_lines,
    "",
    "lavaan model syntax",
    "-------------------",
    as.character(bundle$syntax %||% "Syntax unavailable")
  )
  paste(lines, collapse = "\n")
}

structural_canvas_export_notes <- function(bundle) {
  ordered <- length(bundle$ordered %||% character(0)) > 0L
  admissible <- isTRUE(bundle$diagnostics$admissible %||% FALSE)
  missing_covariances <- structural_canvas_missing_exogenous_covariances(bundle$snapshot %||% list())
  notes <- data.frame(
    Section = c("Analysis context", "Fit", "Fit", "RMSEA tests", "Information criteria", "Validity", "Reliability", "Measurement", "Modification indices", "Admissibility"),
    Note = c(
      structural_canvas_analysis_context(bundle),
      "Robust/scaled fit statistics are reported when available for the fitted estimator.",
      "Descriptive guidance uses CFI/TLI >= .95 (good) and >= .90 (marginal), RMSEA <= .06 (good) and <= .08 (marginal), and SRMR <= .08 (good) and <= .10 (marginal). These are not universal acceptance rules.",
      "Close-fit tests H0: RMSEA <= .05; not-close tests H0: RMSEA >= .08. Estimator-matched robust/scaled p values are used when available, and neither test is a standalone acceptance rule.",
      "AIC, BIC, and adjusted BIC are relative criteria for models fitted to the same observations and variables with the same likelihood and estimator family; lower values are preferred, but they do not establish absolute fit.",
      "Fornell-Larcker diagonal entries are sqrt(AVE); lower-triangle entries are latent correlations. AVE is reported separately.",
      "AVE >= .50 and CR, Cronbach's alpha, and omega >= .70 are descriptive guidelines that require substantive and model-based interpretation.",
      "Fixed reference loadings have no estimated unstandardized SE, z, or p value. R-squared and residual diagnostics should be considered alongside loadings.",
      "MI p values use the unscaled asymptotic 1-df chi-square reference from each modification index and BH adjustment across all finite lavaan candidates before display filters. In sequential output, each step refits the preceding model, skips candidates that fail convergence or post-estimation admissibility, and recomputes its own MI family and EPC values.",
      if (admissible) "The fitted solution passed the implemented admissibility checks." else "The fitted solution failed or did not complete one or more admissibility checks; inferential and validity results require caution."
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (ordered) notes <- rbind(notes, data.frame(
    Section = "Ordered indicators",
    Note = "Ordered-indicator results use the fitted latent-response model; alpha uses the polychoric correlation matrix and AVE/CR/omega use standardized latent-response parameters.",
    stringsAsFactors = FALSE
  ))
  if (!is.null(bundle$bollen_stine_result) && nrow(bundle$bollen_stine_result)) notes <- rbind(notes, data.frame(
    Section = "Bollen-Stine",
    Note = paste0(
      "Model-based AVE/reliability and Bollen-Stine bootstraps require an admissible original CFA and use only replicates that pass the same full admissibility checks; Bollen-Stine additionally uses a plus-one correction and reports finite-simulation error.",
      if (isTRUE(bundle$modified_from_baseline)) " Because the model was modified using the analyzed data, this result is exploratory rather than confirmatory." else ""
    ),
    stringsAsFactors = FALSE
  ))
  if (!is.null(bundle$common_method_result)) notes <- rbind(notes, data.frame(
    Section = "Common method bias",
    Note = "Common method diagnostics are screening evidence only. Report them as indicating whether serious common-method concentration was detected, not as proof that common method bias is absent.",
    stringsAsFactors = FALSE
  ))
  if (isTRUE(bundle$parcel_enabled)) notes <- rbind(notes, data.frame(
    Section = "Parcel planning",
    Note = "The parcel allocation is sample-dependent. No parcel variables were created in the data; the fitted parcel option represents parcels as lower-order item-level factors using the original indicators. Substantive homogeneity, local-dependence review, and sensitivity to alternative allocations remain required.",
    stringsAsFactors = FALSE
  ))
  if (length(missing_covariances)) notes <- rbind(notes, data.frame(
    Section = "Latent covariances",
    Note = paste0("Omitted exogenous latent covariance paths were fixed to zero: ", paste(missing_covariances, collapse = ", "), "."),
    stringsAsFactors = FALSE
  ))
  effect_bootstrap_state <- if (exists("structural_canvas_effect_bootstrap_bundle_state", mode = "function")) {
    structural_canvas_effect_bootstrap_bundle_state(bundle)
  } else {
    list(requested = FALSE, note = "")
  }
  if (isTRUE(effect_bootstrap_state$requested)) notes <- rbind(notes, data.frame(
    Section = "Structural-effect bootstrap",
    Note = as.character(effect_bootstrap_state$note %||% "Structural-effect bootstrap status was not recorded."),
    stringsAsFactors = FALSE
  ))
  group_comparison <- structural_canvas_structural_group_comparison_export(bundle)
  has_multigroup_latent_moderation <- is.list(group_comparison) && (
    identical(as.character(group_comparison$subtype %||% ""), "latent_product_indicator") ||
      any(vapply(c(
        "interaction_group_estimates", "interaction_omnibus_tests", "interaction_pairwise_differences",
        "moderated_mediation_group_indices", "moderated_mediation_delta_tests",
        "moderated_mediation_pairwise_differences", "moderated_mediation_bootstrap_diagnostics"
      ), function(name) {
        value <- group_comparison[[name]]
        is.data.frame(value) && nrow(value) > 0L
      }, logical(1)))
  )
  if (has_multigroup_latent_moderation) {
    bootstrap_state <- structural_canvas_multigroup_latent_moderation_bootstrap_state(group_comparison)
    usable_component_names <- names(Filter(isTRUE, bootstrap_state$usable_components %||% list()))
    index_pairwise_bootstrap_usable <- isTRUE(
      bootstrap_state$usable_components$moderated_mediation_pairwise_differences
    )
    notes <- rbind(notes, data.frame(
      Section = "Multi-group latent moderation",
      Note = paste(
        "Interaction-coefficient equality tests target unstandardized B.",
        if (index_pairwise_bootstrap_usable) {
          "Pairwise moderated-mediation index differences use within-group stratified case-resampling bootstrap inference as the primary result. Delta-method/Wald index-equality tests are auxiliary sensitivity analyses."
        } else if (bootstrap_state$usable) {
          paste0(
            "Usable stratified-bootstrap inference is available only for: ",
            paste(usable_component_names, collapse = ", "),
            ". No bootstrap-primary moderated-mediation index-difference claim is made."
          )
        } else if (bootstrap_state$recorded) {
          "The stratified-bootstrap execution was recorded, but its diagnostics or finite CI/p results did not support inference; bootstrap confidence intervals and p values were suppressed, and no bootstrap-primary claim may be made. Delta/Wald results remain auxiliary model-based evidence."
        } else {
          "Only auxiliary model-based Delta/Wald inference is recorded for moderated-mediation indices; no stratified bootstrap result is available, so bootstrap-primary claims must not be made."
        },
        "Standardized product-indicator indices are not reported because they are scale-dependent.",
        if (bootstrap_state$usable) {
          "Review requested and jointly valid replicate counts, CI method, RNG seed, centering scope, failure diagnostics, and BH multiplicity families before interpretation."
        } else if (bootstrap_state$recorded) {
          paste0("Bootstrap state: ", bootstrap_state$state, ". ", bootstrap_state$reason)
        } else {
          "Request the SEM structural-effect bootstrap to obtain stratified product-index inference."
        }
      ),
      stringsAsFactors = FALSE
    ))
  }
  rownames(notes) <- NULL
  notes
}

structural_canvas_report_summary <- function(bundle) {
  fit <- bundle$fit
  analysis_type <- tolower(as.character(bundle$analysis_type %||% ""))
  if (identical(analysis_type, "plssem") || !inherits(fit, "lavaan")) {
    estimator <- toupper(as.character(bundle$estimator %||% "PLS"))
    analysis_data <- bundle$analysis_data %||% fit$data %||% data.frame()
    n_used <- if (is.data.frame(analysis_data) || is.matrix(analysis_data)) nrow(analysis_data) else NA_integer_
    snapshot_nodes <- bundle$snapshot$nodes %||% list()
    indicator_nodes <- Filter(function(node) identical(node$role %||% "", "indicator"), snapshot_nodes)
    latent_nodes <- Filter(function(node) identical(node$role %||% "", "latent"), snapshot_nodes)
    observed <- as.character(bundle$observed %||% character(0))
    if (!length(observed) && length(indicator_nodes)) {
      observed <- unique(vapply(indicator_nodes, structural_canvas_name, character(1)))
    }
    constructs <- colnames(as.matrix(fit$construct_scores %||% matrix(numeric(0), 0L, 0L)))
    if (!length(constructs) && length(latent_nodes)) {
      constructs <- unique(vapply(latent_nodes, structural_canvas_name, character(1)))
    }
    path_matrix <- tryCatch(
      as.matrix(structural_canvas_pls_summary(fit)$paths %||% matrix(numeric(0), 0L, 0L)),
      error = function(error) matrix(numeric(0), 0L, 0L)
    )
    path_count <- if (length(path_matrix)) sum(is.finite(path_matrix) & path_matrix != 0) else {
      latent_ids <- vapply(latent_nodes, function(node) as.character(node$id %||% ""), character(1))
      sum(vapply(bundle$snapshot$edges %||% list(), function(edge) {
        !identical(edge$kind %||% "", "covariance") &&
          as.character(edge$from %||% "") %in% latent_ids &&
          as.character(edge$to %||% "") %in% latent_ids
      }, logical(1)))
    }
    missing_policy <- bundle$missing_diagnostics$policy %||% structural_canvas_pls_missing_policy()
    missing_policy_text <- if (is.list(missing_policy)) {
      paste(
        as.character(missing_policy$label %||% missing_policy$method %||% "Indicator mean replacement"),
        as.character(missing_policy$analysis %||% ""),
        as.character(missing_policy$bootstrap %||% "")
      )
    } else {
      paste(as.character(missing_policy), collapse = "; ")
    }
    return(data.frame(
      Section = c(rep("Analysis", 7L), "Interpretation"),
      Item = c(
        "Analysis context", "Estimator", "N used", "Model indicators",
        "Constructs", "Structural paths", "Missing-data policy", "Reporting caution"
      ),
      Value = c(
        structural_canvas_analysis_context(bundle), estimator,
        if (is.finite(n_used)) as.character(n_used) else "Not recorded",
        as.character(length(observed)), as.character(length(constructs)),
        as.character(path_count), missing_policy_text,
        paste0(
          "PLS/PLSc global-fit indices are descriptive rather than covariance-model exact-fit tests. ",
          if (isTRUE(bundle$modified_from_baseline)) {
            "Label the model as exploratory in manuscripts and reports."
          } else {
            "Report as prespecified only if the model was specified before inspecting these data."
          }
        )
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    ))
  }
  estimator <- bundle$estimator %||% lavaan::lavInspect(fit, "options")$estimator %||% ""
  fit_values <- structural_canvas_fit_measures(fit, estimator, bundle$rmsea_ci %||% .90)$values
  data.frame(
    Section = c(rep("Analysis", 7L), rep("Model fit", 8L), "Interpretation"),
    Item = c(
      "Analysis context", "Estimator", "ML likelihood convention", "N used", "Observed variables", "Latent variables", "Free parameters",
      "Chi-square", "df", "p", "CFI", "TLI", "SRMR", "RMSEA", paste0(round(100 * as.numeric(bundle$rmsea_ci %||% .90)), "% RMSEA CI"),
      "Reporting caution"
    ),
    Value = c(
      structural_canvas_analysis_context(bundle),
      as.character(estimator),
      structural_canvas_ml_likelihood_label(bundle),
      as.character(lavaan::lavInspect(fit, "ntotal")),
      as.character(length(lavaan::lavNames(fit, "ov"))),
      as.character(length(lavaan::lavNames(fit, "lv"))),
      as.character(lavaan::lavInspect(fit, "npar")),
      format_decimal3(fit_values[[1L]]),
      format_decimal3(fit_values[[2L]]),
      format_p(fit_values[[3L]]),
      format_decimal3(fit_values[[5L]]),
      format_decimal3(fit_values[[6L]]),
      format_decimal3(fit_values[[7L]]),
      format_decimal3(fit_values[[8L]]),
      paste0(format_decimal3(fit_values[[9L]]), ", ", format_decimal3(fit_values[[10L]])),
      if (isTRUE(bundle$modified_from_baseline)) "Label the model as exploratory in manuscripts and reports." else "Report as prespecified only if the model was specified before inspecting these data."
    ),
    check.names = FALSE
  )
}
