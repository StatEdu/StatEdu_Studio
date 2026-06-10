# ============================================================
# 00_run_pipeline.R
# Pipeline runner for cross-sectional mixture analysis
# ------------------------------------------------------------
# 역할
# 1) 프로젝트 경로 / dataset / analysis 설정
# 2) common 모듈 로드
# 3) 분석 step 순서 정의
# 4) from_step ~ to_step 범위 실행
# 5) wrapper 함수 제공
#
# 핵심 원칙
# - SCI top journal submission 우선
# - mixture estimation / R3STEP / BCH는 Mplus 우선
# - R은 후처리 / 표 / 그림 / 문서화 담당
# ============================================================

# ------------------------------------------------------------
# 0. helpers
# ------------------------------------------------------------
.now_txt <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

.msg <- function(level = "INFO", ...) {
  txt <- paste0(...)
  cat(sprintf("[%s] [%s] %s\n", .now_txt(), level, txt))
}

.info <- function(...) .msg("INFO", ...)
.warn <- function(...) .msg("WARN", ...)
.ok   <- function(...) .msg("OK", ...)
.err  <- function(...) .msg("ERROR", ...)

`%||%` <- function(x, y) if (is.null(x)) y else x

.norm_path <- function(x) {
  x <- gsub("\\\\", "/", as.character(x))
  x <- gsub("/+", "/", x)
  x
}

.ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

.safe_source <- function(path, envir = parent.frame()) {
  if (!file.exists(path)) {
    stop("Required script not found: ", path, call. = FALSE)
  }
  sys.source(path, envir = envir)
}

.validate_step_name <- function(step, valid_steps, arg_name = "step") {
  if (length(step) != 1 || is.na(step) || !nzchar(step)) {
    stop(sprintf("%s must be a single non-empty string.", arg_name), call. = FALSE)
  }
  if (!step %in% valid_steps) {
    stop(
      sprintf(
        "Invalid %s: %s\nValid steps: %s",
        arg_name, step, paste(valid_steps, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ------------------------------------------------------------
# 1. step registry
# ------------------------------------------------------------
.get_step_registry <- function(project_root, analysis_id = "cross_sectional_mixture") {
  analysis_dir <- file.path(project_root, "R", analysis_id)

  list(
    settings = list(
      file = file.path(analysis_dir, "01_settings.R"),
      label = "SETTINGS"
    ),
    prep = list(
      file = file.path(analysis_dir, "02_prep.R"),
      label = "PREP"
    ),
    estimation_build = list(
      file = file.path(analysis_dir, "03a_estimation_build_inputs.R"),
      label = "ESTIMATION_BUILD"
    ),
    estimation_run = list(
      file = file.path(analysis_dir, "03b_estimation_run_mplus.R"),
      label = "ESTIMATION_RUN"
    ),
    estimation_collect = list(
      file = file.path(analysis_dir, "03c_estimation_collect.R"),
      label = "ESTIMATION_COLLECT"
    ),
    select_best_k = list(
      file = file.path(analysis_dir, "04a_select_best_k.R"),
      label = "SELECT_BEST_K"
    ),
    classify = list(
      file = file.path(analysis_dir, "04b_classify.R"),
      label = "CLASSIFY"
    ),
    r3step = list(
      file = file.path(analysis_dir, "04c_r3step.R"),
      label = "R3STEP"
    ),
    bch = list(
      file = file.path(analysis_dir, "04d_bch.R"),
      label = "BCH"
    ),
    bch_moderation = list(
      file = file.path(analysis_dir, "04e_bch_moderation.R"),
      label = "BCH_MODERATION"
    ),
    tables = list(
      file = file.path(analysis_dir, "05_tables.R"),
      label = "TABLES"
    ),
    figures = list(
      file = file.path(analysis_dir, "07_figures.R"),
      label = "FIGURES"
    ),
    export_docx = list(
      file = file.path(analysis_dir, "08_export_docx.R"),
      label = "EXPORT_DOCX"
    ),
    finalize = list(
      file = file.path(analysis_dir, "09_finalize.R"),
      label = "FINALIZE"
    )
  )
}

.get_step_order <- function() {
  c(
    "settings",
    "prep",
    "estimation_build",
    "estimation_run",
    "estimation_collect",
    "select_best_k",
    "classify",
    "r3step",
    "bch",
    "bch_moderation",
    "tables",
    "figures",
    "export_docx",
    "finalize"
  )
}

.resolve_steps_to_run <- function(from_step, to_step, step_order) {
  .validate_step_name(from_step, step_order, "from_step")
  .validate_step_name(to_step, step_order, "to_step")

  i_from <- match(from_step, step_order)
  i_to <- match(to_step, step_order)

  if (i_from > i_to) {
    stop(
      sprintf(
        "from_step (%s) must not come after to_step (%s).",
        from_step, to_step
      ),
      call. = FALSE
    )
  }

  step_order[i_from:i_to]
}

# ------------------------------------------------------------
# 2. common modules loader
# ------------------------------------------------------------
.load_common_modules <- function(project_root, env) {
  common_dir <- file.path(project_root, "R", "common")

  common_files <- c(
    "01_path.R",
    "02_log.R",
    "03_utils.R",
    "04_io.R",
    "05_dict.R",
    "06_survey.R",
    "07_mplus.R",
    "08_table.R",
    "09_figure.R",
    "10_bch_core.R"
  )

  for (f in common_files) {
    .safe_source(file.path(common_dir, f), envir = env)
  }

  optional_files <- c(
    "fig_style.R",
    "radar_helpers.R",
    "paper_bundle.R"
  )

  for (f in optional_files) {
    p <- file.path(common_dir, f)
    if (file.exists(p)) {
      .safe_source(p, envir = env)
    }
  }

  invisible(TRUE)
}

# ------------------------------------------------------------
# 3. run one step
# ------------------------------------------------------------
.run_one_step <- function(step_name, registry, env) {
  step_info <- registry[[step_name]]
  step_file <- step_info$file
  step_label <- step_info$label %||% toupper(step_name)

  .info("--------------------------------------------------------")
  .info("[STEP] ", step_label)
  .info("[FILE] ", basename(step_file))
  .info("--------------------------------------------------------")

  t0 <- Sys.time()

  result <- tryCatch(
    {
      .safe_source(step_file, envir = env)
      list(ok = TRUE, error = NULL)
    },
    error = function(e) {
      list(ok = FALSE, error = e)
    }
  )

  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 2)

  if (isTRUE(result$ok)) {
    .ok(step_name, " completed (", elapsed, " sec)")
  } else {
    .err(step_name, " failed (", elapsed, " sec)")
    .err("MESSAGE: ", conditionMessage(result$error))
    stop(result$error)
  }

  invisible(result)
}

# ------------------------------------------------------------
# 4. main pipeline
# ------------------------------------------------------------
run_pipeline <- function(from_step = "settings",
                         to_step = "finalize",
                         run_mplus = TRUE,
                         auto_run_bat = FALSE,
                         project_root = getwd(),
                         dataset_id = "KSWL",
                         analysis_id = "cross_sectional_mixture",
                         mplus_work_root = NULL) {
  t0_all <- Sys.time()

  project_root <- .norm_path(project_root)
  step_order <- .get_step_order()
  step_registry <- .get_step_registry(project_root = project_root, analysis_id = analysis_id)
  steps_to_run <- .resolve_steps_to_run(from_step, to_step, step_order)

  run_env <- new.env(parent = globalenv())

  # expose core pipeline variables
  run_env$PROJECT_ROOT <- project_root
  run_env$DATASET_ID <- dataset_id
  run_env$ANALYSIS_ID <- analysis_id
  run_env$MPLUS_WORK_ROOT <- if (!is.null(mplus_work_root) && nzchar(as.character(mplus_work_root))) .norm_path(mplus_work_root) else NULL
  run_env$RUN_MPLUS <- isTRUE(run_mplus)
  run_env$AUTO_RUN_BAT <- isTRUE(auto_run_bat)
  run_env$FROM_STEP <- from_step
  run_env$TO_STEP <- to_step

  run_env$PIPELINE_STEP_ORDER <- step_order
  run_env$PIPELINE_STEPS_TO_RUN <- steps_to_run

  # agreed directory structure
  run_env$DIR_DATASET_ROOT <- file.path(project_root, "data", dataset_id)
  run_env$DIR_OUTPUT_BASE  <- file.path(project_root, "outputs", dataset_id)
  run_env$DIR_OUTPUT       <- file.path(project_root, "outputs", dataset_id, analysis_id)

  .ensure_dir(run_env$DIR_OUTPUT_BASE)
  .ensure_dir(run_env$DIR_OUTPUT)

  .info("============================================================")
  .info("RUN_PIPELINE START")
  .info("============================================================")
  .info("PROJECT_ROOT     = ", project_root)
  .info("DATASET_ID       = ", dataset_id)
  .info("ANALYSIS_ID      = ", analysis_id)
  .info("MPLUS_WORK_ROOT  = ", run_env$MPLUS_WORK_ROOT %||% file.path(project_root, "mplus_tmp"))
  .info("FROM_STEP        = ", from_step)
  .info("TO_STEP          = ", to_step)
  .info("RUN_MPLUS        = ", isTRUE(run_mplus))
  .info("AUTO_RUN_BAT     = ", isTRUE(auto_run_bat))
  .info("------------------------------------------------------------")
  .info("Steps to run: ", paste(steps_to_run, collapse = " -> "))
  .info("------------------------------------------------------------")

  # load common modules first
  .load_common_modules(project_root = project_root, env = run_env)

  completed_steps <- character(0)

  for (st in steps_to_run) {
    .run_one_step(step_name = st, registry = step_registry, env = run_env)
    completed_steps <- c(completed_steps, st)
  }

  elapsed_all <- round(as.numeric(difftime(Sys.time(), t0_all, units = "secs")), 2)

  .info("============================================================")
  .info("RUN_PIPELINE END")
  .info("============================================================")
  .info("Completed steps: ", paste(completed_steps, collapse = ", "))
  .info("Total elapsed: ", elapsed_all, " sec")

  invisible(list(
    ok = TRUE,
    completed_steps = completed_steps,
    elapsed_sec = elapsed_all,
    project_root = project_root,
    dataset_id = dataset_id,
    analysis_id = analysis_id,
    dir_output = run_env$DIR_OUTPUT
  ))
}

# ------------------------------------------------------------
# 5. convenience wrappers
# ------------------------------------------------------------
run_all <- function(run_mplus = TRUE,
                    auto_run_bat = FALSE,
                    project_root = getwd(),
                    dataset_id = "KSWL",
                    analysis_id              = "cross_sectional_mixture") {
  run_pipeline(
    from_step                                = "settings",
    to_step                                  = "finalize",
    run_mplus                                = run_mplus,
    auto_run_bat                             = auto_run_bat,
    project_root                             = project_root,
    dataset_id                               = dataset_id,
    analysis_id                              = analysis_id
  )
}

run_estimation_only <- function(run_mplus    = TRUE,
                                auto_run_bat = FALSE,
                                project_root = getwd(),
                                dataset_id   = "KSWL",
                                analysis_id  = "cross_sectional_mixture") {
  run_pipeline(
    from_step                                = "estimation_build",
    to_step                                  = "estimation_collect",
    run_mplus                                = run_mplus,
    auto_run_bat                             = auto_run_bat,
    project_root                             = project_root,
    dataset_id                               = dataset_id,
    analysis_id                              = analysis_id
  )
}

run_three_step_only <- function(run_mplus    = TRUE,
                                auto_run_bat = FALSE,
                                project_root = getwd(),
                                dataset_id   = "KSWL",
                                analysis_id  = "cross_sectional_mixture") {
  run_pipeline(
    from_step                                = "r3step",
    to_step                                  = "bch",
    run_mplus                                = run_mplus,
    auto_run_bat                             = auto_run_bat,
    project_root                             = project_root,
    dataset_id                               = dataset_id,
    analysis_id                              = analysis_id
  )
}

run_tables_only <- function(project_root     = getwd(),
                            dataset_id       = "KSWL",
                            analysis_id      = "cross_sectional_mixture") {
  run_pipeline(
    from_step                                = "tables",
    to_step                                  = "tables",
    run_mplus                                = FALSE,
    auto_run_bat                             = FALSE,
    project_root                             = project_root,
    dataset_id                               = dataset_id,
    analysis_id                              = analysis_id
  )
}

run_figures_only <- function(project_root    = getwd(),
                             dataset_id      = "KSWL",
                             analysis_id     = "cross_sectional_mixture") {
  run_pipeline(
    from_step                                = "figures",
    to_step                                  = "figures",
    run_mplus                                = FALSE,
    auto_run_bat                             = FALSE,
    project_root                             = project_root,
    dataset_id                               = dataset_id,
    analysis_id                              = analysis_id
  )
}

run_docx_only <- function(project_root       = getwd(),
                          dataset_id         = "KSWL",
                          analysis_id        = "cross_sectional_mixture") {
  run_pipeline(
    from_step                                = "export_docx",
    to_step                                  = "export_docx",
    run_mplus                                = FALSE,
    auto_run_bat                             = FALSE,
    project_root                             = project_root,
    dataset_id                               = dataset_id,
    analysis_id                              = analysis_id
  )
}

run_finalize_only <- function(project_root   = getwd(),
                              dataset_id     = "KSWL",
                              analysis_id    = "cross_sectional_mixture") {
  run_pipeline(
    from_step                                = "finalize",
    to_step                                  = "finalize",
    run_mplus                                = FALSE,
    auto_run_bat                             = FALSE,
    project_root                             = project_root,
    dataset_id                               = dataset_id,
    analysis_id                              = analysis_id
  )
}

# ------------------------------------------------------------
# 6. load message
# ------------------------------------------------------------
cat("\n============================================================\n")
cat("00_run_pipeline.R loaded\n")
cat("------------------------------------------------------------\n")
cat("Pipeline runner registered\n")
cat("Available functions:\n")
cat(" - run_pipeline()\n")
cat(" - run_all()\n")
cat(" - run_estimation_only()\n")
cat(" - run_three_step_only()\n")
cat(" - run_tables_only()\n")
cat(" - run_figures_only()\n")
cat(" - run_docx_only()\n")
cat(" - run_finalize_only()\n")
cat("============================================================\n\n")
