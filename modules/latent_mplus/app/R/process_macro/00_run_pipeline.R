`%||%` <- function(x, y) if (is.null(x)) y else x

.pm_now_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
.pm_msg <- function(level = "INFO", ...) cat(sprintf("[%s] [%s] %s\n", .pm_now_txt(), level, paste0(...)))
.pm_info <- function(...) .pm_msg("INFO", ...)
.pm_ok <- function(...) .pm_msg("OK", ...)

.pm_norm_path <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  gsub("/+", "/", x)
}

.pm_safe_source <- function(path, envir = parent.frame()) {
  if (!file.exists(path)) stop("Required script not found: ", path, call. = FALSE)
  sys.source(path, envir = envir)
}

.pm_step_registry <- function(project_root, analysis_id = "process_macro") {
  analysis_dir <- file.path(project_root, "R", analysis_id)
  list(
    settings = list(file = file.path(analysis_dir, "01_settings.R"), label = "SETTINGS"),
    prep = list(file = file.path(analysis_dir, "02_prep.R"), label = "PREP"),
    model_run = list(file = file.path(analysis_dir, "03_model_run.R"), label = "MODEL_RUN"),
    tables = list(file = file.path(analysis_dir, "04_tables.R"), label = "TABLES"),
    figures = list(file = file.path(analysis_dir, "05_figures.R"), label = "FIGURES"),
    finalize = list(file = file.path(analysis_dir, "09_finalize.R"), label = "FINALIZE")
  )
}

.pm_step_order <- function() c("settings", "prep", "model_run", "tables", "figures", "finalize")

.pm_load_common <- function(project_root, env) {
  common_dir <- file.path(project_root, "R", "common")
  files <- c("01_path.R", "02_log.R", "03_utils.R", "04_io.R", "05_dict.R", "06_survey.R", "07_mplus.R", "08_table.R", "09_figure.R")
  for (f in files) .pm_safe_source(file.path(common_dir, f), envir = env)
}

run_pipeline <- function(from_step = "settings",
                         to_step = "finalize",
                         project_root = getwd(),
                         dataset_id = "KSWL",
                         analysis_id = "process_macro") {
  t0 <- Sys.time()
  step_order <- .pm_step_order()
  i_from <- match(from_step, step_order)
  i_to <- match(to_step, step_order)
  if (is.na(i_from) || is.na(i_to) || i_from > i_to) stop("Invalid step range.", call. = FALSE)

  project_root <- .pm_norm_path(project_root)
  steps_to_run <- step_order[i_from:i_to]
  registry <- .pm_step_registry(project_root, analysis_id = analysis_id)

  run_env <- new.env(parent = globalenv())
  run_env$PROJECT_ROOT <- project_root
  run_env$DATASET_ID <- dataset_id
  run_env$ANALYSIS_ID <- analysis_id

  .pm_load_common(project_root, run_env)

  .pm_info("============================================================")
  .pm_info("PROCESS MACRO PIPELINE START")
  .pm_info("PROJECT_ROOT = ", project_root)
  .pm_info("DATASET_ID   = ", dataset_id)
  .pm_info("ANALYSIS_ID  = ", analysis_id)
  .pm_info("STEPS        = ", paste(steps_to_run, collapse = " -> "))
  .pm_info("============================================================")

  for (st in steps_to_run) .pm_safe_source(registry[[st]]$file, envir = run_env)

  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  .pm_ok("Pipeline completed (", elapsed, " sec)")
  invisible(list(ok = TRUE, elapsed_sec = elapsed, steps = steps_to_run, dir_output = run_env$DIR_OUTPUT))
}

run_all <- function(project_root = getwd(), dataset_id = "KSWL", analysis_id = "process_macro") {
  run_pipeline(project_root = project_root, dataset_id = dataset_id, analysis_id = analysis_id)
}

run_after_mixture <- function(project_root = getwd(),
                              dataset_id = "KSWL",
                              from_step = "settings",
                              to_step = "finalize") {
  run_pipeline(
    from_step = from_step,
    to_step = to_step,
    project_root = project_root,
    dataset_id = dataset_id,
    analysis_id = "process_macro"
  )
}

cat("\n============================================================\n")
cat("process_macro/00_run_pipeline.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Available functions:\n")
cat(" - run_pipeline()\n")
cat(" - run_all()\n")
cat(" - run_after_mixture()\n")
cat("============================================================\n\n")
