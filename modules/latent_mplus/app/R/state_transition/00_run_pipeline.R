`%||%` <- function(x, y) if (is.null(x)) y else x

.st_now_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

.st_msg <- function(level = "INFO", ...) {
  cat(sprintf("[%s] [%s] %s\n", .st_now_txt(), level, paste0(...)))
}

.st_info <- function(...) .st_msg("INFO", ...)
.st_ok <- function(...) .st_msg("OK", ...)
.st_err <- function(...) .st_msg("ERROR", ...)

.st_norm_path <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  gsub("/+", "/", x)
}

.st_ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.st_safe_source <- function(path, envir = parent.frame()) {
  if (!file.exists(path)) stop("Required script not found: ", path, call. = FALSE)
  sys.source(path, envir = envir)
}

.st_step_registry <- function(project_root, analysis_id = "state_transition") {
  analysis_dir <- file.path(project_root, "R", analysis_id)
  list(
    settings = list(file = file.path(analysis_dir, "01_settings.R"), label = "SETTINGS"),
    panel_ingest = list(file = file.path(analysis_dir, "02_panel_ingest.R"), label = "PANEL_INGEST"),
    prep = list(file = file.path(analysis_dir, "03_prep.R"), label = "PREP"),
    estimation_build = list(file = file.path(analysis_dir, "04a_estimation_build.R"), label = "ESTIMATION_BUILD"),
    estimation_run = list(file = file.path(analysis_dir, "04b_estimation_run.R"), label = "ESTIMATION_RUN"),
    estimation_collect = list(file = file.path(analysis_dir, "04c_estimation_collect.R"), label = "ESTIMATION_COLLECT"),
    tables = list(file = file.path(analysis_dir, "05_tables.R"), label = "TABLES"),
    figures = list(file = file.path(analysis_dir, "07_figures.R"), label = "FIGURES"),
    finalize = list(file = file.path(analysis_dir, "09_finalize.R"), label = "FINALIZE")
  )
}

.st_step_order <- function() {
  c(
    "settings",
    "panel_ingest",
    "prep",
    "estimation_build",
    "estimation_run",
    "estimation_collect",
    "tables",
    "figures",
    "finalize"
  )
}

.st_load_common <- function(project_root, env) {
  common_dir <- file.path(project_root, "R", "common")
  files <- c(
    "01_path.R",
    "02_log.R",
    "03_utils.R",
    "04_io.R",
    "05_dict.R",
    "06_survey.R",
    "07_mplus.R",
    "08_table.R",
    "09_figure.R",
    "11_longitudinal.R"
  )

  for (f in files) {
    .st_safe_source(file.path(common_dir, f), envir = env)
  }
}

run_pipeline <- function(from_step = "settings",
                         to_step = "finalize",
                         run_mplus = FALSE,
                         auto_run_bat = FALSE,
                         project_root = getwd(),
                         dataset_id = "KSWL",
                         analysis_id = "state_transition") {
  t0 <- Sys.time()
  step_order <- .st_step_order()
  i_from <- match(from_step, step_order)
  i_to <- match(to_step, step_order)

  if (is.na(i_from) || is.na(i_to)) {
    stop("Invalid step range.", call. = FALSE)
  }
  if (i_from > i_to) {
    stop("from_step must not come after to_step.", call. = FALSE)
  }

  project_root <- .st_norm_path(project_root)
  steps_to_run <- step_order[i_from:i_to]
  registry <- .st_step_registry(project_root, analysis_id = analysis_id)

  run_env <- new.env(parent = globalenv())
  run_env$PROJECT_ROOT <- project_root
  run_env$DATASET_ID <- dataset_id
  run_env$ANALYSIS_ID <- analysis_id
  run_env$RUN_MPLUS <- isTRUE(run_mplus)
  run_env$AUTO_RUN_BAT <- isTRUE(auto_run_bat)

  .st_load_common(project_root, run_env)

  .st_info("============================================================")
  .st_info("STATE TRANSITION PIPELINE START")
  .st_info("PROJECT_ROOT = ", project_root)
  .st_info("DATASET_ID   = ", dataset_id)
  .st_info("ANALYSIS_ID  = ", analysis_id)
  .st_info("STEPS        = ", paste(steps_to_run, collapse = " -> "))
  .st_info("============================================================")

  for (st in steps_to_run) {
    info <- registry[[st]]
    .st_info("[STEP] ", info$label)
    .st_safe_source(info$file, envir = run_env)
  }

  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)
  .st_ok("Pipeline completed (", elapsed, " sec)")

  invisible(list(
    ok = TRUE,
    elapsed_sec = elapsed,
    steps = steps_to_run,
    dir_output = run_env$DIR_OUTPUT
  ))
}

run_all <- function(project_root = getwd(),
                    dataset_id = "KSWL",
                    analysis_id = "state_transition") {
  run_pipeline(
    from_step = "settings",
    to_step = "finalize",
    run_mplus = FALSE,
    auto_run_bat = FALSE,
    project_root = project_root,
    dataset_id = dataset_id,
    analysis_id = analysis_id
  )
}

cat("\n============================================================\n")
cat("state_transition/00_run_pipeline.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Available functions:\n")
cat(" - run_pipeline()\n")
cat(" - run_all()\n")
cat("============================================================\n\n")
