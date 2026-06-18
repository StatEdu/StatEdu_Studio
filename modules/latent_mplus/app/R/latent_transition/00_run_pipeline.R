`%||%` <- function(x, y) if (is.null(x)) y else x

.lt_now_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
.lt_msg <- function(level = "INFO", ...) cat(sprintf("[%s] [%s] %s\n", .lt_now_txt(), level, paste0(...)))
.lt_info <- function(...) .lt_msg("INFO", ...)
.lt_ok <- function(...) .lt_msg("OK", ...)

.lt_norm_path <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  gsub("/+", "/", x)
}

.lt_safe_source <- function(path, envir = parent.frame()) {
  if (!file.exists(path)) stop("Required script not found: ", path, call. = FALSE)
  sys.source(path, envir = envir)
}

.lt_step_registry <- function(project_root, analysis_id = "latent_transition") {
  analysis_dir <- file.path(project_root, "R", analysis_id)
  list(
    settings = list(file = file.path(analysis_dir, "01_settings.R"), label = "SETTINGS"),
    prep = list(file = file.path(analysis_dir, "02_prep.R"), label = "PREP"),
    estimation_build = list(file = file.path(analysis_dir, "04a_estimation_build.R"), label = "ESTIMATION_BUILD"),
    estimation_run = list(file = file.path(analysis_dir, "04b_estimation_run.R"), label = "ESTIMATION_RUN"),
    estimation_collect = list(file = file.path(analysis_dir, "04c_estimation_collect.R"), label = "ESTIMATION_COLLECT"),
    tables = list(file = file.path(analysis_dir, "05_tables.R"), label = "TABLES"),
    figures = list(file = file.path(analysis_dir, "07_figures.R"), label = "FIGURES"),
    finalize = list(file = file.path(analysis_dir, "09_finalize.R"), label = "FINALIZE")
  )
}

.lt_step_order <- function() c("settings", "prep", "estimation_build", "estimation_run", "estimation_collect", "tables", "figures", "finalize")

.lt_load_common <- function(project_root, env) {
  common_dir <- file.path(project_root, "R", "common")
  files <- c("01_path.R", "02_log.R", "03_utils.R", "04_io.R", "05_dict.R", "06_survey.R", "07_mplus.R", "08_table.R", "09_figure.R", "11_longitudinal.R", "12_lta.R")
  for (f in files) .lt_safe_source(file.path(common_dir, f), envir = env)
}

run_pipeline <- function(from_step = "settings",
                         to_step = "finalize",
                         project_root = getwd(),
                         dataset_id = "KSWL",
                         analysis_id = "latent_transition",
                         run_mplus = TRUE,
                         auto_run_bat = FALSE) {
  t0 <- Sys.time()
  step_order <- .lt_step_order()
  i_from <- match(from_step, step_order)
  i_to <- match(to_step, step_order)
  if (is.na(i_from) || is.na(i_to) || i_from > i_to) stop("Invalid step range.", call. = FALSE)
  project_root <- .lt_norm_path(project_root)
  steps_to_run <- step_order[i_from:i_to]
  if (!isTRUE(run_mplus)) steps_to_run <- setdiff(steps_to_run, "estimation_run")
  registry <- .lt_step_registry(project_root, analysis_id = analysis_id)
  run_env <- new.env(parent = globalenv())
  run_env$PROJECT_ROOT <- project_root
  run_env$DATASET_ID <- dataset_id
  run_env$ANALYSIS_ID <- analysis_id
  .lt_load_common(project_root, run_env)
  .lt_info("============================================================")
  .lt_info("LATENT TRANSITION PIPELINE START")
  .lt_info("PROJECT_ROOT = ", project_root)
  .lt_info("DATASET_ID   = ", dataset_id)
  .lt_info("ANALYSIS_ID  = ", analysis_id)
  .lt_info("STEPS        = ", paste(steps_to_run, collapse = " -> "))
  .lt_info("RUN_MPLUS    = ", isTRUE(run_mplus))
  .lt_info("AUTO_RUN_BAT = ", isTRUE(auto_run_bat), " (accepted for compatibility; direct Mplus execution is used)")
  .lt_info("============================================================")
  for (st in steps_to_run) .lt_safe_source(registry[[st]]$file, envir = run_env)
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  .lt_ok("Pipeline completed (", elapsed, " sec)")
  invisible(list(ok = TRUE, elapsed_sec = elapsed, steps = steps_to_run, dir_output = run_env$DIR_OUTPUT))
}

run_all <- function(project_root = getwd(), dataset_id = "KSWL", analysis_id = "latent_transition") {
  run_pipeline(project_root = project_root, dataset_id = dataset_id, analysis_id = analysis_id)
}

cat("\n============================================================\n")
cat("latent_transition/00_run_pipeline.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Available functions:\n")
cat(" - run_pipeline()\n")
cat(" - run_all()\n")
cat("============================================================\n\n")
