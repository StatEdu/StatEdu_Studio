# CFA/SEM canvas workbook assembly and Excel writing helpers.

structural_canvas_prune_dangling_drawing_relationships <- function(file) {
  if (!requireNamespace("zip", quietly = TRUE)) stop("The zip package is required to finalize CFA Excel exports.")
  archive_file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  extract_dir <- tempfile("structural-canvas-xlsx-")
  repacked_file <- tempfile(fileext = ".xlsx")
  dir.create(extract_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit({
    unlink(extract_dir, recursive = TRUE, force = TRUE)
    if (file.exists(repacked_file)) unlink(repacked_file, force = TRUE)
  }, add = TRUE)

  zip::unzip(archive_file, exdir = extract_dir)
  relationship_dir <- file.path(extract_dir, "xl", "worksheets", "_rels")
  relationship_files <- if (dir.exists(relationship_dir)) {
    list.files(relationship_dir, pattern = "[.]rels$", full.names = TRUE)
  } else {
    character(0)
  }
  removed <- 0L
  for (relationship_file in relationship_files) {
    xml <- paste(readLines(relationship_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    matches <- regmatches(xml, gregexpr("<Relationship\\b[^>]*/>", xml, perl = TRUE))[[1L]]
    if (!length(matches) || identical(matches, character(0)) || identical(matches, "")) next
    source_dir <- dirname(dirname(relationship_file))
    for (relationship in matches) {
      type <- sub('.*\\bType="([^"]+)".*', "\\1", relationship, perl = TRUE)
      target <- sub('.*\\bTarget="([^"]+)".*', "\\1", relationship, perl = TRUE)
      is_drawing <- grepl("/(drawing|vmlDrawing)$", type, perl = TRUE)
      if (!is_drawing || identical(target, relationship)) next
      target_file <- normalizePath(file.path(source_dir, target), winslash = "/", mustWork = FALSE)
      if (!file.exists(target_file)) {
        xml <- sub(relationship, "", xml, fixed = TRUE)
        removed <- removed + 1L
      }
    }
    writeLines(xml, relationship_file, useBytes = TRUE)
  }

  if (removed > 0L) {
    archive_entries <- list.files(extract_dir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    zip::zipr(
      zipfile = repacked_file,
      files = archive_entries,
      root = extract_dir,
      include_directories = FALSE,
      mode = "mirror"
    )
    if (!file.copy(repacked_file, archive_file, overwrite = TRUE)) {
      stop("Failed to finalize the CFA Excel export after removing invalid drawing relationships.")
    }
  }
  invisible(removed)
}

structural_canvas_write_result_workbook <- function(sheets, file) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("The openxlsx package is required to export CFA result tables.")
  sheets <- Filter(function(value) is.data.frame(value) || is.matrix(value), sheets)
  if (!length(sheets)) stop("At least one result table is required for Excel export.")
  sanitize_sheet_name <- function(name) {
    characters <- strsplit(as.character(name), "", fixed = TRUE)[[1L]]
    characters[characters %in% c("\\", "/", ":", "*", "?", "[", "]")] <- "_"
    value <- trimws(paste(characters, collapse = ""))
    if (!nzchar(value)) value <- "Sheet"
    substr(value, 1L, 31L)
  }
  candidates <- vapply(names(sheets), sanitize_sheet_name, character(1))
  valid_names <- character(length(candidates))
  used <- character(0)
  for (index in seq_along(candidates)) {
    base <- candidates[[index]]
    candidate <- base
    suffix_index <- 1L
    while (tolower(candidate) %in% tolower(used)) {
      suffix <- paste0("_", suffix_index)
      candidate <- paste0(substr(base, 1L, 31L - nchar(suffix)), suffix)
      suffix_index <- suffix_index + 1L
    }
    valid_names[[index]] <- candidate
    used <- c(used, candidate)
  }
  names(sheets) <- valid_names
  workbook <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "Bottom")
  decimal_style <- openxlsx::createStyle(numFmt = "0.000")
  integer_style <- openxlsx::createStyle(numFmt = "0")
  wrap_style <- openxlsx::createStyle(wrapText = TRUE, valign = "top")
  for (name in names(sheets)) {
    table <- as.data.frame(sheets[[name]], stringsAsFactors = FALSE, check.names = FALSE)
    openxlsx::addWorksheet(workbook, name)
    openxlsx::writeData(workbook, name, table, withFilter = nrow(table) > 0L)
    if (ncol(table)) {
      openxlsx::addStyle(workbook, name, header_style, rows = 1L, cols = seq_len(ncol(table)), gridExpand = TRUE)
      widths <- vapply(seq_len(ncol(table)), function(column) {
        values <- c(names(table)[[column]], as.character(table[[column]]))
        values <- values[!is.na(values)]
        observed <- if (length(values)) max(nchar(values, type = "width"), na.rm = TRUE) + 2L else 10L
        upper <- if (identical(name, "Notes") && identical(names(table)[[column]], "Note")) 80L else 40L
        min(max(observed, 10L), upper)
      }, numeric(1))
      openxlsx::setColWidths(workbook, name, cols = seq_len(ncol(table)), widths = widths)
      openxlsx::freezePane(workbook, name, firstRow = TRUE)
      if (nrow(table)) {
        body_rows <- seq.int(2L, nrow(table) + 1L)
        numeric_columns <- which(vapply(table, is.numeric, logical(1)))
        for (column in numeric_columns) {
          finite <- table[[column]][is.finite(table[[column]])]
          style <- if (length(finite) && all(finite == trunc(finite))) integer_style else decimal_style
          openxlsx::addStyle(workbook, name, style, rows = body_rows, cols = column, gridExpand = TRUE, stack = TRUE)
        }
        wrapped_columns <- which(vapply(table, function(values) {
          is.character(values) && any(nchar(values, type = "width") > 40L, na.rm = TRUE)
        }, logical(1)))
        if (length(wrapped_columns)) openxlsx::addStyle(
          workbook, name, wrap_style, rows = body_rows, cols = wrapped_columns,
          gridExpand = TRUE, stack = TRUE
        )
      }
    }
  }
  add_result_excel_cover(workbook)
  openxlsx::saveWorkbook(workbook, file, overwrite = TRUE)
  structural_canvas_prune_dangling_drawing_relationships(file)
  result_finalize_excel_package(file)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

structural_canvas_workbook_contents <- function(sheet_names) {
  describe <- function(name) {
    if (identical(name, "Contents")) return("Workbook sheet index and interpretation guide.")
    if (identical(name, "Report_Summary")) return("Copy-ready analysis context and key model-fit values for report drafting.")
    if (identical(name, "Construct_Specification")) return("Declared construct type and measurement direction alongside requested/effective weighting, engine representation, estimand, and saved-model migration provenance.")
    if (name %in% c("Overview", "Fit", "Validity", "Measurement")) return("Formatted reporting table; use the corresponding numeric sheet for calculations where available.")
    if (identical(name, "Fit_Numeric")) return("Numeric model-fit statistics, confidence limits, and selected robust/scaled source keys.")
    if (identical(name, "Admissibility_Diagnostics")) return("Numeric eigenvalue, condition-number, boundary-dimension, and reason diagnostics for fitted and compared models.")
    if (identical(name, "RMSEA_Tests")) return("RMSEA close-fit and not-close hypothesis tests using estimator-matched robust/scaled p values when available.")
    if (identical(name, "Information_Criteria")) return("Log-likelihood, AIC, BIC, adjusted BIC, and within-export delta values for likelihood-based model comparison.")
    if (identical(name, "Parameter_Estimates")) return("Numeric unstandardized and standardized parameter estimates with confidence intervals and fixed-parameter flags.")
    if (identical(name, "Structural_Paths")) return("Direct structural paths with model estimates and bootstrap p values when path/effect resampling was requested.")
    if (identical(name, "Structural_Path_CI")) return("Direct-path unstandardized and standardized confidence intervals with an explicit model-based or bootstrap source.")
    if (identical(name, "Structural_Effects")) return("Direct, indirect, and total structural effects with raw and BH-adjusted p values.")
    if (identical(name, "Structural_Effect_CI")) return("Direct, indirect, and total-effect confidence intervals with an explicit model-based or bootstrap source.")
    if (identical(name, "Specific_Indirect")) return("Mediation-path-specific indirect effects with bootstrap SE, confidence intervals, and valid-replicate diagnostics when requested.")
    if (identical(name, "Effect_Bootstrap_Diagnostics")) return("Raw path, indirect, total-effect, and moderated-mediation bootstrap estimates with requested and valid replicate counts.")
    if (identical(name, "Latent_Correlations")) return("Numeric latent-variable correlation matrix.")
    if (identical(name, "Reliability_Validity_Numeric")) return("Numeric AVE, sqrt(AVE), reliability, latent-correlation, and Fornell-Larcker results with assessment flags.")
    if (identical(name, "Sample_Descriptives")) return("Sample statistics used by the fitted model: variable means when estimated, variances, standard deviations, and model N.")
    if (identical(name, "Sample_Covariance")) return("Long-form covariance and correlation statistics used by the fitted model.")
    if (identical(name, "Thresholds")) return("Numeric ordered-indicator thresholds.")
    if (identical(name, "Bollen_Stine")) return("Bollen-Stine exact-fit bootstrap p value with Monte Carlo uncertainty and valid-replicate diagnostics.")
    if (identical(name, "Common_Method")) return("Common method bias diagnostic summary including Harman, single-factor CFA, and common latent factor screens when requested.")
    if (identical(name, "Common_Method_Fit")) return("Fit indices for the research model and requested common-method comparison models.")
    if (identical(name, "Common_Method_Comparison")) return("Difference statistics for requested common-method comparison models, including delta chi-square, delta df, delta CFI, delta RMSEA, and delta SRMR.")
    if (identical(name, "Common_Method_Loadings")) return("Standardized loading changes after adding the common latent method factor.")
    if (grepl("^Residual", name) || identical(name, "Large_Residuals")) return("Local-fit residual diagnostic output.")
    if (grepl("^MI_", name)) return("Exploratory modification-index or holdout-validation output; changes require theoretical justification.")
    if (identical(name, "Model_Difference")) return("Nested-model difference-test result or the explicit reason the formal test was suppressed.")
    if (identical(name, "Covariate_Effects")) return("Estimated covariate effects on their assigned latent-variable targets.")
    if (identical(name, "Covariate_Fit_Comparison")) return("Research-model, covariate-adjusted-model, and delta fit statistics with the estimator-matched fit and likelihood-ratio test basis stated in each row.")
    if (identical(name, "Higher_Order_Loadings")) return("Higher-order factor loadings, confidence intervals, explained lower-order-factor variance, and standardized residual variance.")
    if (identical(name, "Higher_Order_Omega")) return("Model-conditional hierarchical omega for the unit-weighted total score, with indicator count and interpretive guidance.")
    if (identical(name, "MG_Measurement_Gate")) return("Configural, metric, scalar, and strict measurement-invariance results used to gate multi-group structural comparisons.")
    if (identical(name, "MG_Structural_Models")) return("Free-path versus equal-path multi-group structural model comparison, omnibus fit change, estimator-matched difference-test method, and suppression status/reason.")
    if (identical(name, "MG_Group_Paths")) return("Group-specific unstandardized and standardized structural path estimates with confidence intervals.")
    if (identical(name, "MG_Formal_Path_Tests")) return("Formal path-level omnibus equality-constraint Wald tests of unstandardized regression coefficients B, with BH adjustment across the estimable structural-path family.")
    if (identical(name, "MG_Pairwise_Differences")) return("Pairwise unstandardized B differences and 95% confidence intervals from the joint multi-group covariance matrix, with BH adjustment across all estimable path-by-pair follow-up contrasts.")
    if (identical(name, "MG_Group_Diagnostics")) return("Group sample size, complete-case, missingness, and stability diagnostics for multi-group structural analysis.")
    if (identical(name, "MG_Specification_Policy")) return("Metric-gate decision and exact change criteria, joint product-factor gate, unsupported moderated-mediation paths, unstandardized-B equality estimand, loading constraints retained by both structural models, regression constraints added only to the equal-path model, partial-invariance support status, and parameter-label/constraint audit policy.")
    if (identical(name, "MG_Interaction_Estimates")) return("Group-specific unstandardized latent product-indicator interaction coefficients with uncertainty and inferential status.")
    if (identical(name, "MG_Interaction_Omnibus")) return("Omnibus Wald equality tests of latent interaction coefficients across all groups, with BH adjustment across the estimable interaction family.")
    if (identical(name, "MG_Interaction_Pairwise")) return("Pairwise group differences in unstandardized latent interaction coefficients, confidence intervals, and BH-adjusted follow-up tests.")
    if (identical(name, "MG_ModMed_Indices")) return("Group-specific unstandardized indices of moderated mediation with within-group stratified bootstrap uncertainty and valid-replicate diagnostics.")
    if (identical(name, "MG_ModMed_Delta_Tests")) return("Auxiliary Delta-method Wald group-equality tests of moderated-mediation indices; bootstrap differences are primary only when bootstrap diagnostics mark inference usable.")
    if (identical(name, "MG_ModMed_Differences")) return("Pairwise group differences in unstandardized moderated-mediation indices; bootstrap CI and BH-adjusted p are primary only when diagnostics mark inference usable, otherwise bootstrap inference is suppressed and Delta/Wald results remain auxiliary.")
    if (identical(name, "MG_ModMed_Boot_Diagnostics")) return("Requested and jointly valid stratified bootstrap replicates, RNG seed, centering scope, and failure diagnostics for multi-group moderated mediation.")
    if (identical(name, "MG_Product_Indicator_Policy")) return("Product-indicator generation, within-group centering, comparability, scaling, and resampling policy for the dedicated multi-group latent-moderation workflow.")
    if (identical(name, "MG_Product_Indicator_Audit")) return("Actual group-by-product generation audit: source-indicator pairing, method, complete pairs, within-group centers, and product means before and after double-mean-centering.")
    if (identical(name, "PLS_MICOM_Step1")) return("MICOM Step 1 audit of identical indicators, data treatment, PLS algorithm settings, and model specification across groups.")
    if (identical(name, "PLS_MICOM_Groups")) return("Group sample-size, missing-data, warning, and resampling diagnostics for PLS composite-score comparison.")
    if (identical(name, "PLS_MICOM_Steps2_3")) return("Pair-by-construct MICOM compositional, pooled-score mean, and pooled-score variance invariance results with Holm FWER adjustment.")
    if (identical(name, "PLS_MICOM_Pair_Gate")) return("Pair-specific partial composite-invariance gate controlling whether structural-effect comparisons are admitted.")
    if (identical(name, "PLS_MGA_Group_Effects")) return("Group-specific direct, specific indirect, total indirect, and total PLS composite-score effects with within-group bootstrap uncertainty.")
    if (identical(name, "PLS_MGA_Pair_Differences")) return("Pairwise group differences for four canonical structural-effect families with raw, BH-adjusted, and Holm-adjusted bootstrap p values.")
    if (identical(name, "PLS_MGA_Group_Validity")) return("Requested and valid bootstrap counts and the 80% inference gate for each group.")
    if (identical(name, "PLS_MGA_Pair_Validity")) return("Jointly valid bootstrap positions and the 80% inference gate for each group pair.")
    if (identical(name, "PLS_Path_Permutation_Sens")) return("Direct-path-only group-label permutation sensitivity analysis; this does not replace the four-family PLS-MGA bootstrap results.")
    if (identical(name, "PLS_MG_Interaction")) return("Group-specific latent interaction effects with whole-model bootstrap uncertainty.")
    if (identical(name, "PLS_MG_Simple_Slopes")) return("Group-specific simple slopes at the standardized moderator-score mean and plus/minus one standard deviation, with within-group whole-model bootstrap inference.")
    if (identical(name, "PLS_MG_ModMed_Index")) return("Group-specific unstandardized indices of moderated mediation and bootstrap inference.")
    if (identical(name, "PLS_MG_Conditional_Indirect")) return("Group-specific conditional indirect effects at the construct-score mean and plus/minus one standard deviation.")
    if (identical(name, "PLS_MG_ModMed_Differences")) return("MICOM-gated pairwise group differences in latent interaction effects and moderated-mediation indices with BH and Holm adjustments.")
    if (identical(name, "PLS_MG_ModMed_Validity")) return("MICOM admission and common-position 80% bootstrap validity gate for each moderated-effect group pair.")
    if (identical(name, "PLS_MG_Method_Audit")) return("Estimator scope, MICOM and PLS-MGA estimands, missing-data handling, multiplicity policy, seeds, validity gates, and documented limitations.")
    if (identical(name, "PLS_MG_Selected_Paths")) return("Edge-ID audit of the direct structural paths selected for PLS multi-group comparison.")
    if (identical(name, "MG_Selected_Paths")) return("Edge-ID audit of the SEM structural paths selected for equality constraints and direct path comparisons.")
    if (identical(name, "PLS_Fit_Guide")) return("PLS structural diagnostics, including direct effects, R2, f2, and inner-model collinearity guidance.")
    if (identical(name, "PLS_Bootstrap_Effects")) return("Canonical PLS effect estimates with bootstrap uncertainty, multiplicity adjustment, and valid-replicate diagnostics.")
    if (identical(name, "PLS_Direct_Effects")) return("Direct structural effects in canonical endogenous-variable order.")
    if (identical(name, "PLS_Specific_Indirect")) return("Path-specific indirect effects in canonical endogenous-variable order.")
    if (identical(name, "PLS_Total_Indirect")) return("Total indirect effects for reachable predictor-outcome pairs.")
    if (identical(name, "PLS_Total_Effects")) return("Total effects for reachable predictor-outcome pairs.")
    if (identical(name, "PLS_Moderation")) return("Latent interaction estimates for the selected two-stage, product-indicator, or orthogonalized PLS procedure. Predictor and moderator main effects are retained under strong hierarchy; 'Moderator main effect auto-added' identifies moderator direct paths added by the analysis engine because they were absent from the canvas.")
    if (identical(name, "PLS_Simple_Slopes")) return("Simple slopes at the standardized moderator-score mean and plus/minus one standard deviation, with whole-model bootstrap uncertainty and BH adjustment across the displayed probe family.")
    if (identical(name, "PLS_ModMed_Index")) return("Unstandardized indices of moderated mediation with whole-draw bootstrap inference.")
    if (identical(name, "PLS_Conditional_Indirect")) return("Conditional indirect effects at the moderator-score mean and plus/minus one standard deviation.")
    if (identical(name, "PLS_HTMT")) return("HTMT discriminant-validity matrix for reflective construct pairs.")
    if (identical(name, "PLS_Measurement_Guide")) return("Outer loading, weight, item-collinearity, cross-loading, and measurement-mode diagnostics.")
    if (identical(name, "PLS_Measurement_Bootstrap")) return("Outer loading and weight bootstrap uncertainty with valid-replicate diagnostics.")
    if (identical(name, "PLS_Analysis_Record")) return("PLS estimator, model/data fingerprints, resampling settings, MICOM, and multi-group reproducibility metadata.")
    if (identical(name, "PLS_Notes")) return("PLS/PLSc estimand definitions, inference gates, missing-data policy, and interpretation limitations.")
    if (grepl("^Inv", name)) return("Measurement-invariance fit, group, or equality-constraint diagnostic output.")
    if (grepl("_CI$", name)) return("Confidence-interval output; consult Notes and valid-replicate counts before interpretation.")
    if (identical(name, "Model_Syntax")) return("lavaan model syntax used for the fitted model.")
    if (identical(name, "Analysis_Record")) return("Reproducibility record containing analysis options and computational context.")
    if (identical(name, "Notes")) return("Statistical definitions, descriptive cutoffs, caveats, and model-specific warnings.")
    "Supplementary CFA result table."
  }
  data.frame(
    Sheet = sheet_names,
    Description = vapply(sheet_names, describe, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

structural_canvas_result_workbook_sheets <- function(bundle, table_fn, display_name = identity) {
  analysis_type <- as.character(bundle$analysis_type %||% if (inherits(bundle$fit, "lavaan")) "cfa" else "plssem")
  sheets <- list(
    Overview = table_fn("overview"), Report_Summary = structural_canvas_report_summary(bundle), Fit = table_fn("fit"),
    Validity = table_fn("validity"), Measurement = table_fn("measurement"),
    Construct_Specification = structural_canvas_construct_reporting_rows(bundle, analysis_type, FALSE)
  )
  # table_fn() and the explicitly display-mapped helpers below already return
  # user-facing names.  Keep their sheet names so the final pass maps only raw
  # result objects; applying the resolver twice can corrupt a legitimate label
  # that happens to equal another variable's raw key (for example x1 -> "x2").
  display_mapped_sheets <- c("Overview", "Fit", "Validity", "Measurement")
  result_frame <- function(value) if (is.data.frame(value)) value else data.frame()
  display_multigroup_effect_table <- function(value) {
    value <- result_frame(value)
    if (!nrow(value)) return(value)
    value <- structural_canvas_display_identifier_table(value, display_name)
    for (column in intersect(c("Interaction path", "Indirect path", "Moderated path", "Downstream Path"), names(value))) {
      path <- as.character(value[[column]])
      nonmissing <- !is.na(path)
      path[nonmissing] <- structural_canvas_display_path(path[nonmissing], display_name)
      value[[column]] <- path
    }
    value
  }
  display_pls_effect_table <- function(value) {
    value <- result_frame(value)
    if (!nrow(value)) return(value)
    value <- structural_canvas_display_identifier_table(value, display_name)
    if ("Mediators" %in% names(value)) {
      mediators <- as.character(value$Mediators)
      nonmissing <- !is.na(mediators) & nzchar(trimws(mediators))
      mediators[nonmissing] <- structural_canvas_display_path(mediators[nonmissing], display_name)
      value$Mediators <- mediators
    }
    value
  }
  metadata_rows <- function(value, source, prefix = "") {
    if (is.null(value) || !length(value)) return(data.frame())
    if (is.data.frame(value)) {
      return(data.frame(
        Source = source,
        Item = if (nzchar(prefix)) prefix else "Table",
        Value = paste0(nrow(value), " row(s); columns: ", paste(names(value), collapse = ", ")),
        stringsAsFactors = FALSE, check.names = FALSE
      ))
    }
    if (is.list(value)) {
      rows <- lapply(names(value), function(name) {
        item_prefix <- if (nzchar(prefix)) paste(prefix, name, sep = ".") else name
        metadata_rows(value[[name]], source, item_prefix)
      })
      rows <- Filter(function(row) is.data.frame(row) && nrow(row), rows)
      return(if (length(rows)) do.call(rbind, rows) else data.frame())
    }
    text <- as.character(value)
    text <- text[!is.na(text)]
    data.frame(
      Source = source,
      Item = if (nzchar(prefix)) prefix else "Value",
      Value = paste(text, collapse = ", "),
      stringsAsFactors = FALSE, check.names = FALSE
    )
  }
  if (analysis_type %in% c("cbsem", "sem")) {
    structural_tables <- list(
      Structural_Paths = table_fn("structural"),
      Structural_Path_CI = table_fn("structural_ci"),
      Structural_Effects = table_fn("structural_effects"),
      Structural_Effect_CI = table_fn("structural_effect_ci"),
      Specific_Indirect = table_fn("structural_specific_indirect")
    )
    for (name in names(structural_tables)) {
      table <- structural_tables[[name]]
      if (is.data.frame(table) && nrow(table)) {
        sheets[[name]] <- table
        display_mapped_sheets <- c(display_mapped_sheets, name)
      }
    }
    moderation_jn <- structural_canvas_moderation_jn_table(bundle, display_name = display_name)
    if (is.data.frame(moderation_jn) && nrow(moderation_jn)) {
      sheets$Johnson_Neyman <- moderation_jn
      display_mapped_sheets <- c(display_mapped_sheets, "Johnson_Neyman")
    }
    if (is.data.frame(bundle$effect_bootstrap_result) && nrow(bundle$effect_bootstrap_result)) {
      sheets$Effect_Bootstrap_Diagnostics <- structural_canvas_display_identifier_table(bundle$effect_bootstrap_result, display_name)
      display_mapped_sheets <- c(display_mapped_sheets, "Effect_Bootstrap_Diagnostics")
    }
  }
  if (is.list(bundle$invariance_result)) {
    invariance_result <- bundle$invariance_result
    if (exists("structural_canvas_enforce_product_factor_joint_gate", mode = "function")) {
      invariance_result <- structural_canvas_enforce_product_factor_joint_gate(invariance_result)
    }
    if (identical(invariance_result$type %||% "", "pls_micom")) {
      pls_mga <- invariance_result$pls_mga %||% list()
      pls_modmed_mga <- invariance_result$pls_modmed_mga %||%
        pls_mga$pls_modmed_mga %||% list()
      bind_group_modmed <- function(field) {
        results <- pls_modmed_mga$group_results %||% list()
        rows <- lapply(names(results), function(group_name) {
          table <- result_frame(results[[group_name]][[field]])
          if (!nrow(table)) return(NULL)
          data.frame(Group = group_name, table, check.names = FALSE, stringsAsFactors = FALSE)
        })
        rows <- Filter(Negate(is.null), rows)
        if (length(rows)) do.call(rbind, rows) else data.frame()
      }
      pls_multigroup_sheets <- list(
        PLS_MICOM_Step1 = invariance_result$configural_audit,
        PLS_MICOM_Groups = invariance_result$group_diagnostics %||% pls_mga$group_diagnostics,
        PLS_MICOM_Steps2_3 = invariance_result$table,
        PLS_MICOM_Pair_Gate = invariance_result$pairwise_gate,
        PLS_MGA_Group_Effects = pls_mga$group_effects %||% invariance_result$group_effects,
        PLS_MGA_Pair_Differences = pls_mga$pairwise_differences %||% invariance_result$pairwise_effect_differences,
        PLS_MGA_Group_Validity = pls_mga$validity_gate$groups,
        PLS_MGA_Pair_Validity = pls_mga$validity_gate$pairs,
        PLS_Path_Permutation_Sens = invariance_result$permutation_path_sensitivity,
        PLS_MG_Interaction = bind_group_modmed("interaction_effects"),
        PLS_MG_Simple_Slopes = bind_group_modmed("simple_slopes"),
        PLS_MG_ModMed_Index = bind_group_modmed("moderated_mediation"),
        PLS_MG_Conditional_Indirect = bind_group_modmed("conditional_indirect"),
        PLS_MG_ModMed_Differences = pls_modmed_mga$pairwise_differences,
        PLS_MG_ModMed_Validity = pls_modmed_mga$pairwise_validity
      )
      for (name in names(pls_multigroup_sheets)) {
        value <- result_frame(pls_multigroup_sheets[[name]])
        if (!nrow(value)) next
        if (name %in% c(
          "PLS_MICOM_Steps2_3", "PLS_MGA_Group_Effects", "PLS_MGA_Pair_Differences",
          "PLS_Path_Permutation_Sens", "PLS_MG_Interaction", "PLS_MG_Simple_Slopes",
          "PLS_MG_ModMed_Index",
          "PLS_MG_Conditional_Indirect", "PLS_MG_ModMed_Differences"
        )) {
          value <- display_pls_effect_table(value)
          display_mapped_sheets <- c(display_mapped_sheets, name)
        }
        sheets[[name]] <- value
      }
      pls_selected_paths <- result_frame(
        invariance_result$selected_path_registry %||% pls_mga$selected_path_registry %||% data.frame()
      )
      if (nrow(pls_selected_paths)) {
        sheets$PLS_MG_Selected_Paths <- pls_selected_paths
        display_mapped_sheets <- c(display_mapped_sheets, "PLS_MG_Selected_Paths")
      }
      micom_policy <- metadata_rows(list(
        estimator = invariance_result$estimator,
        estimand = invariance_result$estimand,
        method_scope = invariance_result$method_scope,
        configural_invariance_policy = invariance_result$configural_invariance_policy,
        measurement_gate = invariance_result$measurement_gate,
        multiple_testing = invariance_result$multiple_testing,
        missing_data_policy = invariance_result$missing_data_policy,
        stage3_score_source = invariance_result$stage3_score_source,
        permutations_requested = invariance_result$permutations_requested,
        seed = invariance_result$seed,
        path_scope = invariance_result$path_scope %||% pls_mga$path_scope %||% "all",
        requested_path_ids = invariance_result$requested_path_ids %||% pls_mga$requested_path_ids,
        direct_path_selection_policy = invariance_result$direct_path_selection_policy %||% pls_mga$direct_path_selection_policy,
        pls_mga_bootstrap_replicates = pls_mga$bootstrap_reps_requested,
        pls_mga_seed = pls_mga$seed,
        pls_mga_group_seeds = pls_mga$group_seeds,
        pls_mga_missing_policy = pls_mga$missing_policy,
        pls_mga_estimand_basis = pls_mga$estimand_basis,
        pls_mga_micom_gate = pls_mga$micom_gate,
        pls_mga_validity_gate = pls_mga$validity_gate,
        pls_mga_omnibus_status = pls_mga$omnibus_status,
        pls_mga_metadata = pls_mga$metadata,
        pls_mga_status = pls_mga$status,
        pls_mga_reason = pls_mga$reason,
        pls_modmed_mga_seed = pls_modmed_mga$seed,
        pls_modmed_mga_micom_gate = pls_modmed_mga$micom_gate,
        pls_modmed_mga_metadata = pls_modmed_mga$metadata,
        pls_modmed_mga_omnibus_status = pls_modmed_mga$omnibus_status
      ), "PLS multi-group audit")
      if (nrow(micom_policy)) sheets$PLS_MG_Method_Audit <- micom_policy
    } else if (identical(invariance_result$type %||% "", "structural_path_comparison")) {
      measurement <- invariance_result$measurement_invariance %||% list()
      gate <- structural_canvas_normalize_metric_invariance_gate(
        invariance_result$measurement_gate %||% list(), measurement
      )
      gate_metrics <- gate$metrics %||% list()
      gate_criteria <- gate$criteria %||% list()
      gate_metric_value <- function(value) {
        value <- suppressWarnings(as.numeric(value %||% NA_real_))
        if (length(value) && is.finite(value[[1L]])) format_decimal3(value[[1L]]) else "Not available"
      }
      gate_criteria_text <- paste0(
        "Metric model converged and admissible; DeltaCFI >= ",
        sprintf("%.3f", gate_criteria$delta_cfi_min %||% -.010),
        "; DeltaRMSEA <= ", sprintf("%.3f", gate_criteria$delta_rmsea_max %||% .015),
        "; DeltaSRMR <= ", sprintf("%.3f", gate_criteria$delta_srmr_max %||% .030), "."
      )
      bootstrap_execution <- list(
        requested = isTRUE(bundle$multigroup_moderation_bootstrap_requested),
        pending = isTRUE(bundle$multigroup_moderation_bootstrap_pending),
        canceled = isTRUE(bundle$multigroup_moderation_bootstrap_canceled),
        error = trimws(paste(as.character(bundle$multigroup_moderation_bootstrap_error %||% ""), collapse = " ")),
        blocked_reason = trimws(paste(as.character(bundle$multigroup_moderation_bootstrap_blocked_reason %||% ""), collapse = " "))
      )
      bootstrap_state <- if (
        exists("structural_canvas_structural_group_comparison_export", mode = "function") &&
          exists("structural_canvas_multigroup_latent_moderation_bootstrap_state", mode = "function")
      ) {
        structural_canvas_multigroup_latent_moderation_bootstrap_state(
          structural_canvas_structural_group_comparison_export(bundle)
        )
      } else {
        diagnostics <- result_frame(invariance_result$moderated_mediation_bootstrap_diagnostics)
        diagnostic_usable <- nrow(diagnostics) > 0L &&
          "Inference usable" %in% names(diagnostics) &&
          any(as.logical(diagnostics[["Inference usable"]]), na.rm = TRUE)
        recorded <- bootstrap_execution$requested || nrow(diagnostics) > 0L
        state <- if (bootstrap_execution$pending) {
          "pending"
        } else if (bootstrap_execution$canceled) {
          "canceled"
        } else if (nzchar(bootstrap_execution$error)) {
          "failed"
        } else if (nzchar(bootstrap_execution$blocked_reason)) {
          "blocked"
        } else if (diagnostic_usable) {
          "recorded_usable"
        } else if (recorded) {
          "recorded_unusable"
        } else {
          "not_recorded"
        }
        reason <- switch(
          state,
          pending = "The stratified bootstrap is still running.",
          canceled = "The stratified bootstrap was canceled by the user.",
          failed = paste0("The stratified bootstrap failed: ", bootstrap_execution$error),
          blocked = paste0("The stratified bootstrap was not run: ", bootstrap_execution$blocked_reason),
          recorded_usable = "Diagnostics permitted bootstrap inference.",
          recorded_unusable = "Bootstrap execution was recorded, but inference was unavailable.",
          "No stratified-bootstrap execution record was available."
        )
        list(
          recorded = recorded, usable = identical(state, "recorded_usable"),
          state = state, reason = reason
        )
      }
      if (nrow(result_frame(measurement$table))) sheets$MG_Measurement_Gate <- measurement$table
      if (nrow(result_frame(invariance_result$table))) sheets$MG_Structural_Models <- invariance_result$table
      if (nrow(result_frame(invariance_result$group_diagnostics))) sheets$MG_Group_Diagnostics <- invariance_result$group_diagnostics
      if (nrow(result_frame(invariance_result$path_estimates))) sheets$MG_Group_Paths <- invariance_result$path_estimates
      if (nrow(result_frame(invariance_result$formal_path_tests))) sheets$MG_Formal_Path_Tests <- invariance_result$formal_path_tests
      if (nrow(result_frame(invariance_result$path_differences))) sheets$MG_Pairwise_Differences <- invariance_result$path_differences
      selected_path_registry <- result_frame(invariance_result$selected_path_registry)
      if (nrow(selected_path_registry)) {
        sheets$MG_Selected_Paths <- selected_path_registry
        display_mapped_sheets <- c(display_mapped_sheets, "MG_Selected_Paths")
      }
      latent_moderation_sheets <- list(
        MG_Interaction_Estimates = invariance_result$interaction_group_estimates,
        MG_Interaction_Omnibus = invariance_result$interaction_omnibus_tests,
        MG_Interaction_Pairwise = invariance_result$interaction_pairwise_differences,
        MG_ModMed_Indices = invariance_result$moderated_mediation_group_indices,
        MG_ModMed_Delta_Tests = invariance_result$moderated_mediation_delta_tests,
        MG_ModMed_Differences = invariance_result$moderated_mediation_pairwise_differences,
        MG_ModMed_Boot_Diagnostics = invariance_result$moderated_mediation_bootstrap_diagnostics
      )
      for (name in names(latent_moderation_sheets)) {
        value <- display_multigroup_effect_table(latent_moderation_sheets[[name]])
        if (!nrow(value)) next
        sheets[[name]] <- value
        display_mapped_sheets <- c(display_mapped_sheets, name)
      }
      product_indicator_policy_rows <- metadata_rows(
        invariance_result$product_indicator_policy %||% NULL, "Policy"
      )
      if (nrow(product_indicator_policy_rows)) {
        sheets$MG_Product_Indicator_Policy <- product_indicator_policy_rows
      }
      product_indicator_audit <- result_frame(
        invariance_result$product_indicator_audit %||% data.frame()
      )
      if (nrow(product_indicator_audit)) {
        sheets$MG_Product_Indicator_Audit <- product_indicator_audit
        # Preserve the raw internal product/source names in this reproducibility
        # sheet. User-label mapping remains confined to interpretation tables.
        display_mapped_sheets <- c(display_mapped_sheets, "MG_Product_Indicator_Audit")
      }
      sheets$MG_Specification_Policy <- data.frame(
        Item = c(
          "Policy", "Structural path scope", "Requested structural edge IDs", "Selected lavaan paths",
          "Unselected group.partial regressions", "Expected selected-path Delta df", "Observed selected-path Delta df",
          "Selected-path Delta df matched", "Direct-path selection policy",
          "Measurement gate passed", "Measurement gate reason code", "Measurement gate reason",
          "Measurement gate DeltaCFI", "Measurement gate DeltaRMSEA", "Measurement gate DeltaSRMR",
          "Measurement gate converged", "Measurement gate admissible",
          "Metric gate criteria", "Path-equality estimand",
          "Free-path model group.equal", "Equal-path model group.equal",
          "Product-factor joint gate passed", "Product-factor joint gate reason",
          "Interaction loading constraints", "Unsupported moderated-mediation paths",
          "Multi-group bootstrap requested", "Multi-group bootstrap pending",
          "Multi-group bootstrap canceled", "Multi-group bootstrap error",
          "Multi-group bootstrap blocked reason", "Multi-group bootstrap recorded",
          "Multi-group bootstrap inference usable", "Multi-group bootstrap state",
          "Multi-group bootstrap state reason",
          "Partial invariance support", "Labelled parameters audited",
          "Repeated equality labels", "Explicit parameter constraints"
        ),
        Value = c(
          as.character(invariance_result$specification_policy %||% ""),
          as.character(invariance_result$path_scope %||% "all"),
          paste(invariance_result$requested_path_ids %||% character(0), collapse = ", "),
          paste(invariance_result$selected_path_registry$lavaan_term %||% character(0), collapse = "; "),
          paste(invariance_result$unselected_group_partial %||% character(0), collapse = "; "),
          as.character(invariance_result$constraint_df_audit$expected_delta_df %||% NA_real_),
          as.character(invariance_result$constraint_df_audit$actual_delta_df %||% NA_real_),
          as.character(invariance_result$constraint_df_audit$matched %||% NA),
          as.character(invariance_result$comparison_policy$direct_path_selection_policy %||% "Not recorded"),
          as.character(isTRUE(gate$passed)),
          as.character(gate$reason_code %||% "not_recorded"),
          structural_canvas_metric_invariance_gate_reason(gate, "en"),
          gate_metric_value(gate_metrics$delta_cfi),
          gate_metric_value(gate_metrics$delta_rmsea),
          gate_metric_value(gate_metrics$delta_srmr),
          as.character(isTRUE(gate_metrics$converged)),
          as.character(isTRUE(gate_metrics$admissible)),
          gate_criteria_text,
          as.character(invariance_result$comparison_policy$estimand_statement %||% "All structural equality tests target unstandardized regression coefficient B; standardized beta is descriptive only."),
          paste(invariance_result$comparison_policy$free_model_group_equal %||% "", collapse = ", "),
          paste(invariance_result$comparison_policy$equal_model_group_equal %||% "", collapse = ", "),
          if (is.list(invariance_result$product_factor_joint_gate)) {
            as.character(isTRUE(invariance_result$product_factor_joint_gate$passed))
          } else {
            "Not applicable"
          },
          if (is.list(invariance_result$product_factor_joint_gate)) {
            as.character(invariance_result$product_factor_joint_gate$reason %||% "Not recorded")
          } else {
            "Not applicable"
          },
          if (is.list(invariance_result$product_factor_joint_gate)) {
            as.character(invariance_result$product_factor_joint_gate$loading_constraints %||% "Not recorded")
          } else {
            "Not applicable"
          },
          if (length(invariance_result$moderated_mediation_unsupported_paths %||% character(0))) {
            paste(invariance_result$moderated_mediation_unsupported_paths, collapse = "; ")
          } else {
            "None"
          },
          as.character(bootstrap_execution$requested),
          as.character(bootstrap_execution$pending),
          as.character(bootstrap_execution$canceled),
          if (nzchar(bootstrap_execution$error)) bootstrap_execution$error else "None",
          if (nzchar(bootstrap_execution$blocked_reason)) bootstrap_execution$blocked_reason else "None",
          as.character(isTRUE(bootstrap_state$recorded)),
          as.character(isTRUE(bootstrap_state$usable)),
          as.character(bootstrap_state$state %||% "not_recorded"),
          as.character(bootstrap_state$reason %||% "Not recorded"),
          as.character(invariance_result$comparison_policy$partial_invariance_status %||% "Not implemented"),
          as.character(invariance_result$constraint_audit$labelled_parameters %||% 0L),
          paste(invariance_result$constraint_audit$repeated_labels %||% character(0), collapse = ", "),
          paste(invariance_result$constraint_audit$explicit_constraints %||% character(0), collapse = "; ")
        ),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    } else if (nrow(result_frame(invariance_result$table))) {
      sheets$Invariance <- invariance_result$table
      sheets$Invariance_Groups <- invariance_result$group_diagnostics
      if (!is.null(invariance_result$group_reliability) && nrow(invariance_result$group_reliability)) {
        sheets$Invariance_Reliability <- invariance_result$group_reliability
      }
      if (!is.null(invariance_result$group_htmt) && nrow(invariance_result$group_htmt)) {
        sheets$Invariance_HTMT <- invariance_result$group_htmt
      }
      group_residuals <- structural_canvas_display_residual_diagnostics(
        invariance_result$group_residuals %||% list(),
        display_name
      )
      if (isTRUE(group_residuals$available)) {
        if (nrow(group_residuals$group_summary %||% data.frame())) {
          sheets$Invariance_Residual_Summary <- group_residuals$group_summary
          display_mapped_sheets <- c(display_mapped_sheets, "Invariance_Residual_Summary")
        }
        if (nrow(group_residuals$group_pairs %||% data.frame())) {
          sheets$Invariance_Residual_Pairs <- group_residuals$group_pairs
          display_mapped_sheets <- c(display_mapped_sheets, "Invariance_Residual_Pairs")
        }
      }
      score_tables <- invariance_result$score_diagnostics %||% list()
      for (stage in names(score_tables)) if (nrow(score_tables[[stage]])) sheets[[paste0("Inv_Score_", stage)]] <- score_tables[[stage]]
    }
  }
  is_pls <- identical(analysis_type, "plssem") ||
    toupper(as.character(bundle$estimator %||% "")) %in% c("PLS", "PLSC")
  if (is_pls) {
    pls_tables <- list(
      PLS_Fit_Guide = table_fn("fit_guide"),
      PLS_Bootstrap_Effects = table_fn("fit_bootstrap"),
      PLS_Direct_Effects = table_fn("pls_direct_effects"),
      PLS_Specific_Indirect = table_fn("pls_specific_indirect"),
      PLS_Total_Indirect = table_fn("pls_total_indirect"),
      PLS_Total_Effects = table_fn("pls_total_effect"),
      PLS_Moderation = structural_canvas_pls_moderation_result_table(
        bundle, "pls_moderation", display_name, format_values = FALSE
      ),
      PLS_Simple_Slopes = structural_canvas_pls_moderation_result_table(
        bundle, "pls_simple_slopes", display_name, format_values = FALSE
      ),
      PLS_ModMed_Index = structural_canvas_pls_moderation_result_table(
        bundle, "pls_moderated_mediation", display_name, format_values = FALSE
      ),
      PLS_Conditional_Indirect = structural_canvas_pls_moderation_result_table(
        bundle, "pls_conditional_indirect", display_name, format_values = FALSE
      ),
      PLS_HTMT = table_fn("pls_htmt"),
      PLS_Measurement_Guide = table_fn("measurement_guide"),
      PLS_Measurement_Bootstrap = table_fn("measurement_bootstrap")
    )
    for (name in names(pls_tables)) {
      value <- display_pls_effect_table(pls_tables[[name]])
      if (!is.data.frame(value) || (!nrow(value) && !ncol(value))) next
      sheets[[name]] <- value
      display_mapped_sheets <- c(display_mapped_sheets, name)
    }

    audit <- tryCatch(
      structural_canvas_audit_manifest(bundle, "plssem"),
      error = function(error) list(audit_error = conditionMessage(error))
    )
    record <- metadata_rows(list(
      analysis = audit$analysis %||% list(
        type = "plssem", estimator = bundle$estimator %||% "not recorded",
        n_analysis = if (is.data.frame(bundle$analysis_data)) nrow(bundle$analysis_data) else NA_integer_
      ),
      estimator_selection = audit$decision$pls_estimator_selection,
      structural_multiplicity = audit$decision$structural_multiplicity,
      pls_mga_multiplicity = audit$decision$pls_mga_multiplicity,
      model_specification_sha256 = audit$model$specification_sha256,
      data_fingerprints = audit$data_fingerprints,
      pls_bootstrap = audit$resampling$pls,
      pls_latent_moderation = audit$resampling$pls$latent_moderation_and_moderated_mediation,
      micom = audit$resampling$micom,
      pls_multi_group = audit$resampling$pls_multi_group,
      pls_multi_group_moderation = audit$resampling$pls_multi_group$moderated_mediation,
      pls_predict = audit$resampling$pls_predict,
      audit_error = audit$audit_error
    ), "PLS analysis record")
    if (nrow(record)) sheets$PLS_Analysis_Record <- record

    estimator <- toupper(as.character(bundle$estimator %||% "PLS"))
    moderation_definitions <- bundle$diagnostics$moderation_definitions %||%
      bundle$fit$statedu_moderation_definitions %||% list()
    moderation_methods <- unique(vapply(
      moderation_definitions,
      function(definition) as.character(definition$method %||% "two_stage"),
      character(1)
    ))
    auto_main_effects <- any(vapply(
      moderation_definitions,
      function(definition) isTRUE(definition$moderator_main_effect_auto_added),
      logical(1)
    ))
    sheets$PLS_Notes <- data.frame(
      Topic = c(
        "Estimand", "PLSc scope", "Model fit", "Missing data", "Bootstrap gate",
        "Latent moderation method", "Strong hierarchy", "Conditional effects",
        "PLSc interaction scope", "MICOM scope", "PLS-MGA scope", "Multiplicity",
        "Omnibus limitation"
      ),
      Note = c(
        "Structural, indirect, and total effects are effects among fitted construct scores; causal interpretation requires a defensible design and assumptions.",
        if (identical(estimator, "PLSC")) "PLSc disattenuation applies only to the recorded common-factor constructs; composite constructs remain uncorrected." else "PLSc common-factor correction was not requested.",
        "PLS model-fit indices are descriptive diagnostics and are not covariance-model exact-fit acceptance tests.",
        as.character(bundle$missing_diagnostics$policy %||% structural_canvas_pls_missing_policy()),
        "Bootstrap CI and p values are displayed only when at least 80% of whole requested draws satisfy the recorded finite-shape, convergence, and admissibility contract.",
        if (length(moderation_methods)) paste0("Declared latent-interaction procedure(s): ", paste(moderation_methods, collapse = ", "), ". The full measurement, interaction, and structural model is re-estimated in every bootstrap draw.") else "No latent interaction was specified.",
        if (length(moderation_definitions)) paste0("The predictor and moderator main effects are retained under the strong-hierarchy principle; moderator main-effect auto-addition recorded: ", if (auto_main_effects) "yes." else "no.") else "Strong-hierarchy handling is not applicable because no latent interaction was specified.",
        "Simple slopes and conditional indirect effects are evaluated at the moderator mean and mean +/- 1 SD; the unstandardized index of moderated mediation and its bootstrap CI are the primary moderated-mediation results.",
        if (identical(estimator, "PLSC") && length(moderation_definitions)) "PLSc correction does not apply to the generated interaction construct; interpret it as an uncorrected construct-score interaction, not a common-factor interaction." else "No PLSc latent-interaction correction claim is made.",
        "MICOM evaluates PLS composite-score invariance pair by pair; it does not establish PLSc/common-factor measurement invariance.",
        "Only MICOM-admitted pairs receive PLS-MGA interval and p-value inference; point estimates remain descriptive for blocked pairs.",
        "Single-group interaction, simple-slope, moderated-mediation, and conditional-indirect families use BH adjustment; MICOM-admitted PLS-MGA pairwise families additionally report Holm adjustment.",
        "This release reports pairwise PLS-MGA comparisons and does not provide an omnibus three-or-more-group permutation test."
      ),
      stringsAsFactors = FALSE, check.names = FALSE
    )

    raw_sheet_names <- setdiff(names(sheets), unique(display_mapped_sheets))
    sheets[raw_sheet_names] <- lapply(
      sheets[raw_sheet_names], structural_canvas_display_identifier_table,
      display_name = display_name
    )
    return(c(list(Contents = structural_canvas_workbook_contents(c("Contents", names(sheets)))), sheets))
  }
  if (!is.null(bundle$reliability_bootstrap_result) && nrow(bundle$reliability_bootstrap_result)) sheets$Reliability_CI <- bundle$reliability_bootstrap_result
  if (!is.null(bundle$bollen_stine_result) && nrow(bundle$bollen_stine_result)) {
    sheets$Bollen_Stine <- bundle$bollen_stine_result
    sheets$Bollen_Stine$`Model context` <- if (isTRUE(bundle$modified_from_baseline)) "Exploratory modified model" else "Prespecified/original model"
  }
  if (!is.null(bundle$htmt_bootstrap_result) && nrow(bundle$htmt_bootstrap_result)) sheets$HTMT_CI <- bundle$htmt_bootstrap_result
  higher_htmt <- structural_canvas_higher_htmt_result(bundle)
  if (isTRUE(higher_htmt$available)) {
    sheets$Higher_HTMT <- higher_htmt$result$pairs
    sheets$Higher_HTMT_Raw <- higher_htmt$raw_result$pairs
    sheets$Higher_Validity <- structural_canvas_higher_validity_estimates(bundle)
    sheets$Higher_HTMT_Indicators <- higher_htmt$mapping
    sheets$Higher_HTMT_Method <- data.frame(Method = c(structural_canvas_higher_htmt_note(higher_htmt, TRUE), structural_canvas_higher_htmt_note(higher_htmt), structural_canvas_higher_validity_note(bundle, sheets$Higher_Validity)), check.names = FALSE)
    higher_htmt_ci <- attr(bundle$htmt_bootstrap_result, "higher_order")
    if (is.data.frame(higher_htmt_ci)) sheets$Higher_HTMT_CI <- higher_htmt_ci
    higher_raw_ci <- attr(bundle$htmt_bootstrap_result, "higher_order_raw")
    if (is.data.frame(higher_raw_ci)) sheets$Higher_HTMT_Raw_CI <- higher_raw_ci
  }
  higher_order <- structural_canvas_higher_order_results(bundle$snapshot %||% list(), bundle$fit)
  if (isTRUE(higher_order$available) && nrow(higher_order$table %||% data.frame())) {
    higher_table <- higher_order$table
    names(higher_table) <- c(
      "Higher-order factor", "Lower-order factor", "B", "B 95% CI lower", "B 95% CI upper",
      "SE", "z", "p", "Beta", "Beta 95% CI lower", "Beta 95% CI upper",
      "R2", "R2 95% CI lower", "R2 95% CI upper", "Standardized residual variance"
    )
    sheets$Higher_Order_Loadings <- higher_table
    omega_h <- structural_canvas_omega_h(bundle$snapshot %||% list(), bundle$fit)
    if (isTRUE(omega_h$available)) {
      sheets$Higher_Order_Omega <- data.frame(
        `Higher-order factor` = omega_h$higher_order_factor,
        Indicators = omega_h$indicators,
        `Hierarchical omega` = omega_h$omega_h,
        Guidance = structural_canvas_omega_h_guidance(omega_h$omega_h),
        Note = "Model- and unit-weighted-score-conditional; not evidence of unidimensionality or equivalence to a bifactor model.",
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
  }
  if (length(bundle$covariates %||% character(0))) {
    covariate_effects <- structural_canvas_covariate_effect_table(bundle$fit, bundle$covariates, display_name)
    if (nrow(covariate_effects)) {
      sheets$Covariate_Effects <- covariate_effects
      display_mapped_sheets <- c(display_mapped_sheets, "Covariate_Effects")
    }
    if (nrow(bundle$covariate_fit_comparison %||% data.frame())) sheets$Covariate_Fit_Comparison <- bundle$covariate_fit_comparison
  }
  common_method <- bundle$common_method_result %||% NULL
  if (!is.null(common_method)) {
    common_summary <- structural_canvas_common_method_display_table(common_method, format_values = FALSE)
    if (nrow(common_summary)) sheets$Common_Method <- common_summary
    if (nrow(common_method$fit %||% data.frame())) sheets$Common_Method_Fit <- common_method$fit
    if (nrow(common_method$comparison %||% data.frame())) sheets$Common_Method_Comparison <- common_method$comparison
    if (nrow(common_method$loading_change %||% data.frame())) sheets$Common_Method_Loadings <- common_method$loading_change
  }
  if (!is.null(bundle$mi_history) && nrow(bundle$mi_history)) sheets$MI_History <- bundle$mi_history[, setdiff(names(bundle$mi_history), "Signature"), drop = FALSE]
  if (!is.null(bundle$holdout_comparison)) {
    sheets$MI_Holdout_Fit <- bundle$holdout_comparison$table
    sheets$MI_Holdout_Change <- bundle$holdout_comparison$changes
  }
  if (isTRUE(bundle$modified_from_baseline) && !is.null(bundle$baseline_fit)) sheets$Model_Difference <- structural_canvas_model_difference_report(bundle)
  residuals <- structural_canvas_display_residual_diagnostics(
    structural_canvas_residual_diagnostics(bundle$fit),
    display_name
  )
  matrix_sheet <- function(value) {
    value <- as.matrix(value)
    data.frame(Indicator = rownames(value), value, check.names = FALSE)
  }
  if (isTRUE(residuals$available)) {
    sheets$Residual_Z <- matrix_sheet(residuals$standardized)
    sheets$Residual_Correlation <- matrix_sheet(residuals$correlation)
    display_mapped_sheets <- c(display_mapped_sheets, "Residual_Z", "Residual_Correlation")
    if (nrow(residuals$largest)) {
      sheets$Large_Residuals <- residuals$largest
      display_mapped_sheets <- c(display_mapped_sheets, "Large_Residuals")
    }
  }
  latent_intervals <- structural_canvas_latent_correlation_intervals(bundle$fit, level = .95)
  if (nrow(latent_intervals)) sheets$Latent_Correlation_CI <- latent_intervals
  if (!is.null(bundle$mi) && nrow(bundle$mi)) {
    mi_columns <- intersect(c("step", "skipped_inadmissible", "skipped_details", "lhs", "op", "rhs", "mi", "MI p", "BH-adjusted p", "Multiplicity family size", "epc", "sepc.lv", "sepc.all", "Reason", "cfi_after", "tli_after", "rmsea_after", "srmr_after"), names(bundle$mi))
    sheets$MI_Candidates <- bundle$mi[, mi_columns, drop = FALSE]
  }
  sheets$Model_Syntax <- data.frame(Line = strsplit(as.character(bundle$syntax %||% ""), "\n", fixed = TRUE)[[1L]], check.names = FALSE)
  sheets$Analysis_Record <- data.frame(Record = strsplit(structural_canvas_reproducibility_record(bundle), "\n", fixed = TRUE)[[1L]], check.names = FALSE)
  sheets$Fit_Numeric <- structural_canvas_export_fit_estimates(bundle)
  sheets$Admissibility_Diagnostics <- structural_canvas_export_admissibility(bundle)
  sheets$RMSEA_Tests <- structural_canvas_rmsea_hypothesis_tests(bundle)
  information_criteria <- structural_canvas_information_criteria(bundle)
  if (any(is.finite(information_criteria$AIC)) || any(is.finite(information_criteria$BIC))) sheets$Information_Criteria <- information_criteria
  sheets$Parameter_Estimates <- structural_canvas_export_parameter_estimates(bundle$fit)
  sheets$Latent_Correlations <- structural_canvas_export_latent_correlations(bundle$fit)
  sheets$Reliability_Validity_Numeric <- structural_canvas_export_reliability_validity(bundle)
  sample_statistics <- structural_canvas_export_sample_statistics(bundle$fit)
  if (nrow(sample_statistics$Descriptives)) sheets$Sample_Descriptives <- sample_statistics$Descriptives
  if (nrow(sample_statistics$Covariance)) sheets$Sample_Covariance <- sample_statistics$Covariance
  if (nrow(sample_statistics$Thresholds)) sheets$Thresholds <- sample_statistics$Thresholds
  sheets$Notes <- structural_canvas_export_notes(bundle)
  raw_sheet_names <- setdiff(names(sheets), unique(display_mapped_sheets))
  sheets[raw_sheet_names] <- lapply(
    sheets[raw_sheet_names], structural_canvas_display_identifier_table,
    display_name = display_name
  )
  if (is.data.frame(sheets$Latent_Correlations) && ncol(sheets$Latent_Correlations) > 1L) {
    names(sheets$Latent_Correlations)[-1L] <- as.character(display_name(names(sheets$Latent_Correlations)[-1L]))
  }
  c(list(Contents = structural_canvas_workbook_contents(c("Contents", names(sheets)))), sheets)
}
