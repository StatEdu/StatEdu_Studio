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
    "run_structural_canvas_analysis", "structural_canvas_execute_analysis",
    "structural_canvas_lavaan_syntax", "structural_canvas_run_pls_analysis",
    "structural_canvas_run_pls_predict", "structural_canvas_effect_bootstrap"
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
  if (is.list(bundle$invariance_result$measurement_gate) && !isTRUE(bundle$invariance_result$measurement_gate$passed)) add("Measurement invariance", "Critical", "The engine-appropriate measurement-invariance gate did not pass; structural group comparisons must not be interpreted.")
  bootstrap_tables <- list(bundle$effect_bootstrap_result, bundle$htmt_bootstrap_result, bundle$reliability_bootstrap_result, bundle$bollen_stine_result)
  bootstrap_status <- unique(unlist(lapply(bootstrap_tables, function(value) if (is.data.frame(value) && "Status" %in% names(value)) as.character(value$Status) else character(0))))
  if (any(bootstrap_status %in% c("Caution", "Unreliable"))) add("Resampling", if ("Unreliable" %in% bootstrap_status) "Critical" else "Major", paste0("Bootstrap diagnostics include: ", paste(bootstrap_status, collapse = ", "), "."))
  if (is.list(bundle$pls_predict_result) && as.integer(bundle$pls_predict_result$reps %||% 0L) < 5L) add("Prediction", "Major", "PLSpredict used fewer than five independent repetitions; predictive stability is insufficiently characterized.")
  if (!length(warnings)) return(data.frame(Category = character(0), Severity = character(0), Message = character(0)))
  do.call(rbind, warnings)
}

structural_canvas_audit_manifest <- function(bundle, analysis_type = NULL, generated_at = Sys.time()) {
  recommendation <- bundle$method_recommendation %||% list()
  diagnostics <- bundle$diagnostics %||% list()
  analysis_type <- as.character(analysis_type %||% if (toupper(as.character(bundle$estimator %||% "")) %in% c("PLS", "PLSC")) "plssem" else "cbsem")
  analysis_data <- bundle$analysis_data %||% data.frame()
  validation_data <- bundle$validation_data %||% data.frame()
  specification_payload <- list(
    snapshot = bundle$snapshot %||% list(),
    syntax = as.character(bundle$syntax %||% ""),
    estimator = as.character(bundle$estimator %||% ""),
    ordered = as.character(bundle$ordered %||% character(0))
  )
  specification_hash <- structural_canvas_sha256(specification_payload)
  git_provenance <- structural_canvas_git_provenance()
  code_fingerprint <- structural_canvas_analysis_code_fingerprint()
  audit_warnings <- structural_canvas_audit_warnings(bundle, analysis_type, diagnostics)
  residual_diagnostics <- if (inherits(bundle$fit, "lavaan")) structural_canvas_residual_diagnostics(bundle$fit) else list(available = FALSE)
  factor_score_quality <- if (inherits(bundle$fit, "lavaan")) structural_canvas_factor_score_quality(bundle$fit) else data.frame()
  list(
    schema = list(name = "StatEdu SEM audit manifest", version = "1.4"),
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
      selected_method = bundle$selected_method %||% "not recorded",
      estimator = bundle$estimator %||% "not recorded",
      missing = bundle$missing %||% "not recorded",
      missing_sensitivity = structural_canvas_missing_sensitivity_rows(bundle),
      latent_scaling = if (isTRUE(bundle$std_lv)) "latent variance fixed to 1" else "marker loading fixed to 1",
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
      structural_effect_plan = bundle$structural_effect_plan %||% diagnostics$structural_effect_plan %||% data.frame(),
      causal_interpretation = structural_canvas_causal_interpretation(bundle$snapshot %||% list(), analysis_type),
      mi_validation_gate = bundle$mi_validation_gate %||% NULL,
      structural_multiplicity = if (analysis_type %in% c("cbsem", "sem")) list(method = "Benjamini-Hochberg", family = "Displayed direct, indirect, and total structural effects", raw_p_retained_for = "Prespecified primary hypotheses") else list(method = "Benjamini-Hochberg", families = c("Direct structural paths", "Indirect effects", "Total effects", "Outer loadings", "Outer weights"), raw_p_retained_for = "Prespecified primary hypotheses"),
      pls_mga_multiplicity = if (identical(analysis_type, "plssem") && nrow(bundle$invariance_result$mga_table %||% data.frame())) list(method = "Benjamini-Hochberg", family = "All MICOM-gated PLS-MGA path-difference permutation tests") else NULL
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
      reliability = list(replicates = bundle$reliability_bootstrap %||% 0L, seed = bundle$reliability_seed %||% NULL, ci_method = bundle$reliability_ci_method %||% NULL),
      htmt = list(replicates = bundle$htmt_bootstrap %||% 0L, seed = bundle$htmt_seed %||% NULL, ci_method = bundle$htmt_ci_method %||% NULL),
      bollen_stine = list(replicates = bundle$bollen_stine_bootstrap %||% 0L, seed = bundle$bollen_stine_seed %||% NULL),
      structural_effects = list(replicates = bundle$effect_bootstrap %||% 0L, seed = bundle$effect_bootstrap_seed %||% NULL, interval = paste("case-resampling", bundle$effect_bootstrap_ci_method %||% "bias_corrected", "for unstandardized and standardized effects"), diagnostics = bundle$effect_bootstrap_result %||% NULL),
      pls = list(replicates = bundle$pls_bootstrap %||% 0L, seed = bundle$pls_seed %||% NULL),
      micom = list(enabled = isTRUE(bundle$invariance_enabled) && identical(analysis_type, "plssem"), permutations = bundle$micom_permutations %||% NULL, seed = bundle$micom_seed %||% NULL, result = if (identical(bundle$invariance_result$type %||% "", "pls_micom")) bundle$invariance_result else NULL),
      pls_predict = list(folds = bundle$pls_predict_folds %||% NULL, repetitions = bundle$pls_predict_reps %||% NULL, seed = bundle$pls_predict_seed %||% NULL, benchmark = "linear model", repetition_results = bundle$pls_predict_result$repetition_summaries %||% NULL),
      mi_holdout = list(enabled = isTRUE(bundle$mi_holdout_enabled), fraction = bundle$mi_holdout_fraction %||% NULL, seed = bundle$mi_holdout_seed %||% NULL)
    ),
    requested_assessments = list(
      measurement_invariance = list(
        enabled = isTRUE(bundle$invariance_enabled), group = bundle$invariance_group %||% NULL,
        metric_gate = bundle$invariance_result$measurement_gate %||% NULL,
        measurement_table = bundle$invariance_result$measurement_invariance$table %||% NULL
      ),
      common_method = list(enabled = isTRUE(bundle$common_method_enabled), methods = bundle$common_method_methods %||% character(0)),
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
    privacy = list(raw_data_included = FALSE, fitted_object_included = FALSE, data_content_hash_included = TRUE, note = "SHA-256 fingerprints permit equality checks but do not embed raw observations.")
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

structural_canvas_reproducibility_record <- function(bundle, generated_at = Sys.time()) {
  fit <- bundle$fit
  options <- lavaan::lavInspect(fit, "options")
  recommendation <- bundle$method_recommendation %||% list()
  recommendation_candidates <- recommendation$candidates %||% data.frame()
  analysis_type <- as.character(bundle$analysis_type %||% "cfa")
  construct_rows <- structural_canvas_construct_reporting_rows(bundle, analysis_type, FALSE)
  construct_lines <- if (nrow(construct_rows)) apply(construct_rows, 1L, function(row) paste(row, collapse = " | ")) else "none"
  lines <- c(
    "CFA analysis reproducibility record",
    paste0("Generated: ", format(generated_at, "%Y-%m-%d %H:%M:%S %Z")),
    paste0("R version: ", paste(R.version$major, R.version$minor, sep = ".")),
    paste0("lavaan version: ", as.character(utils::packageVersion("lavaan"))),
    paste0("Analysis context: ", structural_canvas_analysis_context(bundle)),
    paste0("Estimator: ", bundle$estimator %||% options$estimator %||% ""),
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
    paste0("HTMT bootstrap: ", bundle$htmt_bootstrap %||% 0L, "; seed: ", bundle$htmt_seed %||% "not used", "; CI method: ", bundle$htmt_ci_method %||% "bias_corrected"),
    paste0("AVE/reliability bootstrap: ", bundle$reliability_bootstrap %||% 0L, "; seed: ", bundle$reliability_seed %||% "not used", "; CI method: ", bundle$reliability_ci_method %||% "percentile"),
    paste0("Bollen-Stine bootstrap: ", bundle$bollen_stine_bootstrap %||% 0L, "; seed: ", bundle$bollen_stine_seed %||% "not used"),
    paste0("Measurement invariance: ", if (isTRUE(bundle$invariance_enabled)) paste0("enabled; group = ", bundle$invariance_group) else "disabled"),
    paste0("Common method bias diagnostics: ", if (isTRUE(bundle$common_method_enabled)) paste0("enabled; methods = ", paste(bundle$common_method_methods %||% character(0), collapse = ", ")) else "disabled"),
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
  rownames(notes) <- NULL
  notes
}

structural_canvas_report_summary <- function(bundle) {
  fit <- bundle$fit
  estimator <- bundle$estimator %||% lavaan::lavInspect(fit, "options")$estimator %||% ""
  fit_values <- structural_canvas_fit_measures(fit, estimator, bundle$rmsea_ci %||% .90)$values
  data.frame(
    Section = c(rep("Analysis", 6L), rep("Model fit", 8L), "Interpretation"),
    Item = c(
      "Analysis context", "Estimator", "N used", "Observed variables", "Latent variables", "Free parameters",
      "Chi-square", "df", "p", "CFI", "TLI", "SRMR", "RMSEA", paste0(round(100 * as.numeric(bundle$rmsea_ci %||% .90)), "% RMSEA CI"),
      "Reporting caution"
    ),
    Value = c(
      structural_canvas_analysis_context(bundle),
      as.character(estimator),
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
