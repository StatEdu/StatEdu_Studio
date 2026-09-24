# App bootstrap helpers for StatEdu Studio.
# All statistical analysis dependencies must be CRAN packages.

required_packages <- c(
  "shiny",
  "DT",
  "lmtest",
  "sandwich",
  "nortest",
  "car",
  "boot",
  "jsonlite",
  "haven",
  "foreign",
  "readr",
  "readxl",
  "cellranger",
  "htmltools",
  "markdown",
  "openxlsx",
  "officer",
  "flextable",
  "xml2",
  "rvest",
  "callr",
  "ggplot2",
  "glmnet",
  "agricolae",
  "psych",
  "polycor",
  "longpower",
  "WebPower",
  "TOSTER",
  "MASS",
  "nnet",
  "ordinal",
  "geepack",
  "lme4",
  "lmerTest",
  "mmrm",
  "plm",
  "mice",
  "survey",
  "VGAM",
  "svyVGAM",
  "survival",
  "cmprsk",
  "lavaan",
  "seminr"
)

# Release-validation packages are bundled with the Electron R runtime so the
# installer gate can reproduce external numerical oracles without relying on a
# user's R library. They are intentionally excluded from required_packages:
# the interactive application does not need to attach or load them at startup.
bundled_validation_packages <- c(
  cSEM = "0.6.1"
)

# Exact versions whose package internals are part of a bundled performance or
# numerical contract. These remain ordinary application dependencies; the pin
# is enforced by packaging/preflight validators rather than app startup.
bundled_runtime_package_versions <- c(
  lavaan = "0.7-2",
  mmrm = "0.3.18"
)

startup_packages <- c("shiny", "DT")

ensure_required_packages <- function(packages = required_packages) {
  available_packages <- .packages(all.available = TRUE)
  missing_packages <- setdiff(packages, available_packages)
  if (length(missing_packages) > 0) {
    stop(
      "Install required packages first: install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    )
  }
  invisible(TRUE)
}

load_app_packages <- function(
  packages = required_packages,
  attach_packages = startup_packages,
  check = !identical(tolower(Sys.getenv("STATEDU_NO_PACKAGE_INSTALL", "false")), "true")
) {
  if (isTRUE(check)) {
    ensure_required_packages(packages)
  }
  for (package in attach_packages) {
    suppressPackageStartupMessages(library(package, character.only = TRUE))
  }
  invisible(TRUE)
}

app_module_files <- c(
  "utils.R",
  "labels.R",
  "update_check.R",
  "settings_io.R",
  "settings_dialogs.R",
  "data_io.R",
  "data_roles.R",
  "data_category_labels.R",
  "codebook_io.R",
  "data_regression_setup.R",
  "analysis_reliability.R",
  "analysis_interrater_agreement.R",
  "analysis_frequencies.R",
  "analysis_crosstabs.R",
  "analysis_correlation.R",
  "analysis_factor_analysis.R",
  "analysis_pca.R",
  "analysis_paired.R",
  "analysis_paired_rm.R",
  "analysis_one_group_rm_anova.R",
  "analysis_mixed_rm_anova.R",
  "analysis_nonparametric_paired.R",
  "analysis_ttest_anova.R",
  "analysis_ancova.R",
  "analysis_regression.R",
  "analysis_logistic.R",
  "analysis_generalized.R",
  "analysis_longitudinal.R",
  "analysis_survival.R",
  "analysis_meta.R",
  "sample_size.R",
  "sample_size_ui.R",
  "data_editor_recode.R",
  "data_editor_likert.R",
  "data_editor_missing.R",
  "data_editor_wide_long.R",
  "data_editor_merge.R",
  "data_editor_id_aggregate.R",
  "data_editor_transform.R",
  "data_editor_rename.R",
  "analysis_scope.R",
  "analysis_scope_conditions.R",
  "data_editor_ui.R",
  "calculator_hint8.R",
  "calculator_metabolic.R",
  "calculator_metabolic_severity.R",
  "calculator_frs.R",
  "calculator_eq5d.R",
  "calculator_ascvd10.R",
  "analysis_penalized.R",
  "setup_penalized_menu.R",
  "bootstrap_manager.R",
  "server_client.R",
  "server_data_state.R",
  "server_state.R",
  "server_settings.R",
  "server_selection.R",
  "server_codebook.R",
  "server_setup.R",
  "server_workflow.R",
  "analysis_data_viewer.R",
  "server_reliability.R",
  "server_interrater_agreement.R",
  "analysis_ipa.R",
  "server_ipa.R",
  "server_frequencies.R",
  "server_crosstabs.R",
  "server_logistic.R",
  "server_generalized.R",
  "server_longitudinal.R",
  "server_survival.R",
  "server_paired.R",
  "server_paired_rm.R",
  "server_one_group_rm_anova.R",
  "server_mixed_rm_anova.R",
  "server_ttest_anova.R",
  "server_ancova.R",
  "server_nonparametric.R",
  "server_nonparametric_paired.R",
  "server_correlation.R",
  "server_factor_analysis.R",
  "server_pca.R",
  "server_meta.R",
  "server_analysis.R",
  "server_data_outputs.R",
  "ui_helpers.R",
  "data_ui_tables.R",
  "data_ui_steps.R",
  "data_ui.R",
  "diagnostic_plots.R",
  "result_formatting.R",
  "result_labels.R",
  "result_model_summary.R",
  "result_coefficients.R",
  "setup_analysis_ui.R",
  "analysis_complex_logistic.R",
  "analysis_complex_logistic_diagnostics.R",
  "setup_complex_sample_ui.R",
  "setup_ui.R",
  "setup_interrater_agreement_ui.R",
  "setup_custom_model_canvas_snapshot.R",
  "setup_custom_model_canvas_structural_core.R",
  "setup_custom_model_canvas_structural_diagnostics.R",
  "setup_custom_model_canvas_structural_common_method.R",
  "setup_custom_model_canvas_structural_identification_diagnostics.R",
  "setup_custom_model_canvas_structural_residual_constraints.R",
  "setup_custom_model_canvas_scores.R",
  "setup_custom_model_canvas_scores_ui.R",
  "setup_custom_model_canvas_structural_distribution_diagnostics.R",
  "setup_custom_model_canvas_structural_local_fit_diagnostics.R",
  "setup_custom_model_canvas_structural_risk_diagnostics.R",
  "setup_custom_model_canvas_structural_higher_order.R",
  "setup_custom_model_canvas_structural_validity.R",
  "setup_custom_model_canvas_structural_higher_htmt.R",
  "setup_custom_model_canvas_structural_reliability.R",
  "setup_custom_model_canvas_structural_evaluation.R",
  "setup_custom_model_canvas_structural_model_comparison.R",
  "setup_custom_model_canvas_structural_mi_evaluation.R",
  "setup_custom_model_canvas_structural_holdout_evaluation.R",
  "setup_custom_model_canvas_structural_invariance_evaluation.R",
  "setup_custom_model_canvas_structural_invariance_execute.R",
  "setup_custom_model_canvas_structural_pls_engine.R",
  "setup_custom_model_canvas_structural_pls_mga_engine.R",
  "setup_custom_model_canvas_structural_pls_modmed_engine.R",
  "setup_custom_model_canvas_structural_lavaan_syntax.R",
  "setup_custom_model_canvas_structural_engine.R",
  "setup_custom_model_canvas_structural_bootstrap.R",
  "setup_custom_model_canvas_structural_reliability_bootstrap.R",
  "setup_custom_model_canvas_structural_reliability_bootstrap_execute.R",
  "setup_custom_model_canvas_structural_htmt_bootstrap_execute.R",
  "setup_custom_model_canvas_structural_bollen_stine_bootstrap.R",
  "setup_custom_model_canvas_structural_bollen_stine_bootstrap_execute.R",
  "setup_custom_model_canvas_structural_cfa_bootstrap_job.R",
  "setup_custom_model_canvas_exports.R",
  "setup_custom_model_canvas_export_fit_tables.R",
  "setup_custom_model_canvas_export_report.R",
  "setup_custom_model_canvas_export_workbook.R",
  "setup_custom_model_canvas_i18n.R",
  "setup_custom_model_canvas_variables.R",
  "setup_custom_model_canvas_components.R",
  "setup_custom_model_canvas_options.R",
  "setup_custom_model_canvas_toolbar.R",
  "setup_custom_model_canvas_result_snapshot.R",
  "setup_custom_model_canvas_structural_components.R",
  "setup_custom_model_canvas_structural_toolbar_icons.R",
  "setup_custom_model_canvas_structural_toolbar_components.R",
  "setup_custom_model_canvas_structural_options.R",
  "setup_custom_model_canvas_structural_summary_tables.R",
  "setup_custom_model_canvas_structural_measurement_tables.R",
  "setup_custom_model_canvas_structural_validity_tables.R",
  "setup_custom_model_canvas_structural_mi_tables.R",
  "setup_custom_model_canvas_structural_tables.R",
  "setup_custom_model_canvas_structural_render_tables.R",
  "setup_custom_model_canvas_structural_render_data_diagnostics.R",
  "setup_custom_model_canvas_structural_render_invariance.R",
  "setup_custom_model_canvas_structural_render_heywood.R",
  "setup_custom_model_canvas_structural_render_fit_core.R",
  "setup_custom_model_canvas_structural_render_fit_summary.R",
  "setup_custom_model_canvas_structural_render_fit.R",
  "setup_custom_model_canvas_structural_render_htmt.R",
  "setup_custom_model_canvas_structural_render_factor_scores.R",
  "setup_custom_model_canvas_structural_render_reliability_bootstrap.R",
  "setup_custom_model_canvas_structural_render_latent_correlations.R",
  "setup_custom_model_canvas_structural_render_validity_notes.R",
  "setup_custom_model_canvas_structural_render_validity.R",
  "setup_custom_model_canvas_structural_render_local_fit.R",
  "setup_custom_model_canvas_structural_render_moderation.R",
  "setup_custom_model_canvas_structural_render_mi.R",
  "setup_custom_model_canvas_structural_render.R",
  "setup_custom_model_canvas_structural_execute_settings.R",
  "setup_custom_model_canvas_structural_execute_notifications.R",
  "setup_custom_model_canvas_structural_execute.R",
  "setup_custom_model_canvas_structural_events_estimator.R",
  "setup_custom_model_canvas_structural_events_heywood.R",
  "setup_custom_model_canvas_structural_events.R",
  "setup_custom_model_canvas_structural_handlers.R",
  "setup_custom_model_canvas_ui.R",
  "setup_complex_sample_custom_model_ui.R",
  "setup_meta_ui.R",
  "analysis_menu_ui.R",
  "setup_reliability_ui.R",
  "setup_frequencies_ui.R",
  "setup_crosstabs_ui.R",
  "setup_paired_ui.R",
  "setup_paired_rm_ui.R",
  "setup_one_group_rm_anova_ui.R",
  "setup_mixed_rm_anova_ui.R",
  "setup_ttest_anova_ui.R",
  "setup_ancova_ui.R",
  "setup_nonparametric_ui.R",
  "setup_nonparametric_paired_ui.R",
  "setup_correlation_ui.R",
  "setup_factor_analysis_ui.R",
  "setup_pca_ui.R",
  "setup_regression_ui.R",
  "analysis_syntax.R",
  "analysis_commands.R",
  "setup_mediation_moderation_ui.R",
  "setup_hierarchical_ui.R",
  "setup_logistic_ui.R",
  "setup_longitudinal_ui.R",
  "setup_generalized_ui.R",
  "setup_survival_ui.R",
  "result_table_ui.R",
  "result_penalized_ui.R",
  "result_panels_ui.R",
  "result_reliability_ui.R",
  "result_interrater_agreement_ui.R",
  "result_frequencies_ui.R",
  "result_crosstabs_ui.R",
  "result_logistic_ui.R",
  "result_longitudinal_ui.R",
  "result_generalized_ui.R",
  "result_survival_ui.R",
  "result_paired_ui.R",
  "result_paired_rm_ui.R",
  "result_one_group_rm_anova_ui.R",
  "result_mixed_rm_anova_ui.R",
  "result_nonparametric_paired_ui.R",
  "result_ancova_ui.R",
  "result_correlation_ui.R",
  "result_factor_analysis_ui.R",
  "result_pca_ui.R",
  "result_saved_ui.R",
  "result_document_model.R",
  "result_hwpx_native.R",
  "result_bootstrap_ui.R",
  "result_ui.R",
  "result_export.R",
  "result_export_files.R",
  "app_misc_ui.R",
  "app_server.R"
)

latent_mplus_enabled <- function() FALSE
latent_mplus_head_tags <- function(version) NULL
latent_menu_tab <- function() NULL
latent_mplus_panel_content <- function(...) NULL
register_latent_mplus_server <- function(...) invisible(FALSE)

optional_app_module_files <- c(
  latent_mplus = "latent_mplus_module.R"
)

utf8_app_module_files <- c(
  "analysis_ipa.R",
  "server_ipa.R",
  "utils.R",
  "labels.R",
  "codebook_io.R",
  "server_codebook.R",
  "data_editor_merge.R",
  "data_editor_id_aggregate.R",
  "data_editor_ui.R",
  "server_data_outputs.R",
  "ui_helpers.R",
  "data_ui_steps.R",
  "data_ui.R",
  "analysis_survival.R",
  "analysis_meta.R",
  "analysis_menu_ui.R",
  "setup_meta_ui.R",
  "server_meta.R",
  "setup_custom_model_canvas_snapshot.R",
  "setup_custom_model_canvas_structural_core.R",
  "setup_custom_model_canvas_structural_diagnostics.R",
  "setup_custom_model_canvas_structural_common_method.R",
  "setup_custom_model_canvas_structural_identification_diagnostics.R",
  "setup_custom_model_canvas_structural_residual_constraints.R",
  "setup_custom_model_canvas_scores.R",
  "setup_custom_model_canvas_scores_ui.R",
  "setup_custom_model_canvas_structural_distribution_diagnostics.R",
  "setup_custom_model_canvas_structural_local_fit_diagnostics.R",
  "setup_custom_model_canvas_structural_risk_diagnostics.R",
  "setup_custom_model_canvas_structural_higher_order.R",
  "setup_custom_model_canvas_structural_validity.R",
  "setup_custom_model_canvas_structural_higher_htmt.R",
  "setup_custom_model_canvas_structural_reliability.R",
  "setup_custom_model_canvas_structural_evaluation.R",
  "setup_custom_model_canvas_structural_model_comparison.R",
  "setup_custom_model_canvas_structural_mi_evaluation.R",
  "setup_custom_model_canvas_structural_holdout_evaluation.R",
  "setup_custom_model_canvas_structural_invariance_evaluation.R",
  "setup_custom_model_canvas_structural_invariance_execute.R",
  "setup_custom_model_canvas_structural_pls_engine.R",
  "setup_custom_model_canvas_structural_pls_mga_engine.R",
  "setup_custom_model_canvas_structural_pls_modmed_engine.R",
  "setup_custom_model_canvas_structural_lavaan_syntax.R",
  "setup_custom_model_canvas_structural_engine.R",
  "setup_custom_model_canvas_structural_bootstrap.R",
  "setup_custom_model_canvas_structural_reliability_bootstrap.R",
  "setup_custom_model_canvas_structural_reliability_bootstrap_execute.R",
  "setup_custom_model_canvas_structural_htmt_bootstrap_execute.R",
  "setup_custom_model_canvas_structural_bollen_stine_bootstrap.R",
  "setup_custom_model_canvas_structural_bollen_stine_bootstrap_execute.R",
  "setup_custom_model_canvas_structural_cfa_bootstrap_job.R",
  "setup_custom_model_canvas_exports.R",
  "setup_custom_model_canvas_export_fit_tables.R",
  "setup_custom_model_canvas_export_report.R",
  "setup_custom_model_canvas_export_workbook.R",
  "setup_custom_model_canvas_i18n.R",
  "setup_custom_model_canvas_variables.R",
  "setup_custom_model_canvas_components.R",
  "setup_custom_model_canvas_options.R",
  "setup_custom_model_canvas_toolbar.R",
  "setup_custom_model_canvas_result_snapshot.R",
  "setup_custom_model_canvas_structural_components.R",
  "setup_custom_model_canvas_structural_toolbar_icons.R",
  "setup_custom_model_canvas_structural_toolbar_components.R",
  "setup_custom_model_canvas_structural_options.R",
  "setup_custom_model_canvas_structural_summary_tables.R",
  "setup_custom_model_canvas_structural_measurement_tables.R",
  "setup_custom_model_canvas_structural_validity_tables.R",
  "setup_custom_model_canvas_structural_mi_tables.R",
  "setup_custom_model_canvas_structural_tables.R",
  "setup_custom_model_canvas_structural_render_tables.R",
  "setup_custom_model_canvas_structural_render_data_diagnostics.R",
  "setup_custom_model_canvas_structural_render_invariance.R",
  "setup_custom_model_canvas_structural_render_heywood.R",
  "setup_custom_model_canvas_structural_render_fit_core.R",
  "setup_custom_model_canvas_structural_render_fit_summary.R",
  "setup_custom_model_canvas_structural_render_fit.R",
  "setup_custom_model_canvas_structural_render_htmt.R",
  "setup_custom_model_canvas_structural_render_factor_scores.R",
  "setup_custom_model_canvas_structural_render_reliability_bootstrap.R",
  "setup_custom_model_canvas_structural_render_latent_correlations.R",
  "setup_custom_model_canvas_structural_render_validity_notes.R",
  "setup_custom_model_canvas_structural_render_validity.R",
  "setup_custom_model_canvas_structural_render_local_fit.R",
  "setup_custom_model_canvas_structural_render_moderation.R",
  "setup_custom_model_canvas_structural_render_mi.R",
  "setup_custom_model_canvas_structural_render.R",
  "setup_custom_model_canvas_structural_execute_settings.R",
  "setup_custom_model_canvas_structural_execute_notifications.R",
  "setup_custom_model_canvas_structural_execute.R",
  "setup_custom_model_canvas_structural_events_estimator.R",
  "setup_custom_model_canvas_structural_events_heywood.R",
  "setup_custom_model_canvas_structural_events.R",
  "setup_custom_model_canvas_structural_handlers.R",
  "setup_custom_model_canvas_ui.R",
  "setup_complex_sample_custom_model_ui.R",
  "setup_mediation_moderation_ui.R",
  "setup_survival_ui.R",
  "result_survival_ui.R",
  "server_survival.R",
  "app_misc_ui.R"
)

source_app_modules_individually <- function(files, dir, latent_module_file) {
  for (file in files) {
    if (file %in% utf8_app_module_files) {
      source(file.path(dir, file), local = FALSE, encoding = "UTF-8")
    } else {
      source(file.path(dir, file), local = FALSE)
    }
  }
  if (file.exists(latent_module_file)) {
    source(latent_module_file, local = FALSE)
  }
  invisible(TRUE)
}

app_module_cache_manifest <- function(paths) {
  info <- file.info(paths, extra_cols = FALSE)
  data.frame(
    path = normalizePath(paths, winslash = "/", mustWork = FALSE),
    size = as.numeric(info$size),
    mtime = as.numeric(info$mtime),
    stringsAsFactors = FALSE
  )
}

app_module_cache_paths <- function() {
  cache_dir <- trimws(Sys.getenv("STATEDU_MODULE_CACHE_DIR", ""))
  if (!nzchar(cache_dir)) {
    cache_dir <- tools::R_user_dir("StatEduStudio", which = "cache")
  }
  version_tag <- paste(R.version$major, strsplit(R.version$minor, ".", fixed = TRUE)[[1]][[1]], sep = ".")
  list(
    source = file.path(cache_dir, paste0("app-modules-r", version_tag, ".R")),
    manifest = file.path(cache_dir, paste0("app-modules-r", version_tag, ".rds")),
    expressions = file.path(cache_dir, paste0("app-expressions-r", version_tag, ".rds"))
  )
}

source_cached_app_modules <- function(cache_paths, manifest) {
  # Preserve source references and verbose source output when explicitly requested.
  if (!identical(getOption("keep.source", FALSE), FALSE) ||
      !identical(getOption("verbose", FALSE), FALSE)) {
    source(cache_paths$source, local = FALSE, encoding = "UTF-8")
    return(invisible(TRUE))
  }
  cached <- suppressWarnings(tryCatch(readRDS(cache_paths$expressions), error = function(error) NULL))
  valid <- is.list(cached) && identical(cached$version, R.version.string) &&
    identical(cached$manifest, manifest) && is.expression(cached$expressions)
  expressions <- if (valid) cached$expressions else
    parse(file = cache_paths$source, encoding = "UTF-8", keep.source = FALSE)
  eval(expressions, envir = .GlobalEnv)
  if (!valid) {
    # The optional serialized cache must not prevent loading on a read-only disk.
    suppressWarnings(tryCatch({
      temporary <- tempfile("app-expressions-", tmpdir = dirname(cache_paths$expressions))
      on.exit(if (file.exists(temporary)) unlink(temporary), add = TRUE)
      connection <- gzfile(temporary, open = "wb", compression = 1L)
      tryCatch(
        saveRDS(list(version = R.version.string, manifest = manifest, expressions = expressions), connection),
        finally = close(connection)
      )
      file.copy(temporary, cache_paths$expressions, overwrite = TRUE)
    }, error = function(error) NULL))
  }
  invisible(TRUE)
}

write_combined_app_module_cache <- function(paths, target) {
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("app-modules-", tmpdir = dirname(target), fileext = ".R")
  connection <- file(temporary, open = "wb")
  connection_open <- TRUE
  on.exit({
    if (isTRUE(connection_open)) close(connection)
    if (file.exists(temporary)) unlink(temporary, force = TRUE)
  }, add = TRUE)
  for (path in paths) {
    writeBin(charToRaw(paste0("\n# source: ", normalizePath(path, winslash = "/", mustWork = FALSE), "\n")), connection)
    writeBin(readBin(path, what = "raw", n = file.info(path, extra_cols = FALSE)$size), connection)
    writeBin(charToRaw("\n"), connection)
  }
  close(connection)
  connection_open <- FALSE
  if (!file.copy(temporary, target, overwrite = TRUE)) {
    stop("Could not create the combined app-module cache.", call. = FALSE)
  }
  invisible(target)
}

source_app_modules <- function(files = app_module_files, dir = "R") {
  latent_module_file <- file.path(dir, optional_app_module_files[["latent_mplus"]])
  use_cache <- identical(files, app_module_files) &&
    !identical(tolower(Sys.getenv("STATEDU_MODULE_CACHE", "true")), "false") &&
    !identical(Sys.getenv("STATEDU_MODULE_CACHE", "true"), "0")
  if (!isTRUE(use_cache)) {
    return(source_app_modules_individually(files, dir, latent_module_file))
  }

  paths <- file.path(dir, files)
  if (file.exists(latent_module_file)) paths <- c(paths, latent_module_file)
  if (!all(file.exists(paths))) {
    return(source_app_modules_individually(files, dir, latent_module_file))
  }

  cache_paths <- app_module_cache_paths()
  manifest <- app_module_cache_manifest(paths)
  cached_manifest <- suppressWarnings(tryCatch(readRDS(cache_paths$manifest), error = function(error) NULL))
  cache_valid <- file.exists(cache_paths$source) && identical(cached_manifest, manifest)
  if (!isTRUE(cache_valid)) {
    cache_valid <- isTRUE(tryCatch({
      write_combined_app_module_cache(paths, cache_paths$source)
      saveRDS(manifest, cache_paths$manifest)
      TRUE
    }, error = function(error) FALSE))
  }

  if (isTRUE(cache_valid)) {
    loaded <- isTRUE(tryCatch({
      source_cached_app_modules(cache_paths, manifest)
      TRUE
    }, error = function(error) {
      warning("Combined app-module cache failed; using individual source files: ", conditionMessage(error), call. = FALSE)
      FALSE
    }))
    if (isTRUE(loaded)) return(invisible(TRUE))
  }

  source_app_modules_individually(files, dir, latent_module_file)
}

read_app_config <- function(version_file = NULL) {
  if(is.null(version_file)) {
    public <- tolower(Sys.getenv("STATEDU_PUBLIC_RELEASE", "")) %in% c("1","true","yes","on","public")
    edition <- tolower(Sys.getenv("STATEDU_EDITION", "development"))
    development <- !public && edition %in% c("", "development")
    version_file <- if(development && file.exists("VERSION_DEV")) "VERSION_DEV" else "VERSION"
  }
  list(
    version = trimws(readLines(version_file, warn = FALSE)[1])
  )
}
