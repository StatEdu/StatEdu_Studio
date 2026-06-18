T0_RUN <- Sys.time()

log_step_start("ESTIMATION_RUN", "04b_estimation_run.R")
log_info("Running Mplus latent transition model ...")

ESTIMATION_REGISTRY <- load_step_rds("ESTIMATION_REGISTRY", dir_rds = DIR_RDS, required = TRUE)
CFG <- load_step_rds("CFG", dir_rds = DIR_RDS, required = TRUE)

inp_file <- as.character(ESTIMATION_REGISTRY$inp_file[1])
mplus_exe <- as.character(ESTIMATION_REGISTRY$mplus_exe[1] %||% resolve_mplus_exe(CFG, must_exist = TRUE))
exec_res <- run_mplus_model(inp_file = inp_file, mplus_exe = mplus_exe, workdir = dirname(inp_file), wait = TRUE, quiet = TRUE)

ESTIMATION_RUN_RESULTS <- data.frame(
  model_tag = ESTIMATION_REGISTRY$model_tag[1],
  inp_file = inp_file,
  out_file = exec_res$out_file %||% ESTIMATION_REGISTRY$out_file[1],
  log_file = exec_res$log_file %||% ESTIMATION_REGISTRY$log_file[1],
  status = ifelse(isTRUE(exec_res$ok), "ok", "failed"),
  parse_ok = isTRUE(exec_res$ok),
  system_status = exec_res$status %||% NA_integer_,
  stringsAsFactors = FALSE
)

save_named_rds_list(
  list(
    ESTIMATION_REGISTRY = ESTIMATION_REGISTRY,
    ESTIMATION_RUN_RESULTS = ESTIMATION_RUN_RESULTS
  ),
  dir_rds = DIR_RDS
)

if (!isTRUE(ESTIMATION_RUN_RESULTS$parse_ok[1])) {
  stop("Mplus latent-transition run failed. Check .out/.log files.", call. = FALSE)
}

log_step_end("estimation_run", round(as.numeric(difftime(Sys.time(), T0_RUN, units = "secs")), 2), ok = TRUE)
