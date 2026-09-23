script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

if (identical(.Platform$OS.type, "windows")) {
  Sys.setenv(LC_ALL = "Korean_Korea.utf8", LANG = "Korean_Korea.utf8")
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE)))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))
statedu_apply_preferences(statedu_initial_preferences())

elapsed <- function(expression) {
  started <- proc.time()[["elapsed"]]
  value <- force(expression)
  list(value = value, seconds = unname(proc.time()[["elapsed"]] - started))
}

set.seed(20260826L)
data <- data.frame(
  x1 = stats::rnorm(90L),
  x2 = stats::rnorm(90L),
  x3 = stats::rnorm(90L),
  x4 = stats::rnorm(90L),
  x5 = stats::rnorm(90L)
)
data$y <- 0.4 + 0.9 * data$x1 - 0.55 * data$x2 + stats::rnorm(90L, sd = .75)
formula <- y ~ x1 + x2 + x3 + x4 + x5
ols <- stats::lm(formula, data = data)
results <- list(list(
  formula = formula,
  coef_table = data.frame(
    Term = names(stats::coef(ols)),
    B = as.numeric(stats::coef(ols)),
    check.names = FALSE
  )
))
common <- list(
  results = results,
  data = data,
  seed = 20260826L,
  alpha_grid = c(.25, .5, .75),
  selection_bootstrap_resamples = 500L
)

old_worker_warm <- getOption("statedu.penalized.bootstrap.worker_files_warm", NULL)
on.exit({
  if (is.null(old_worker_warm)) {
    options(statedu.penalized.bootstrap.worker_files_warm = NULL)
  } else {
    options(statedu.penalized.bootstrap.worker_files_warm = old_worker_warm)
  }
}, add = TRUE)
options(statedu.penalized.bootstrap.worker_files_warm = FALSE)
cold_workers <- penalized_selection_bootstrap_workers(NULL, 500L)
cold <- elapsed(do.call(fit_penalized_models, common))
warm_workers <- penalized_selection_bootstrap_workers(NULL, 500L)
warm <- elapsed(do.call(fit_penalized_models, common))

stopifnot(
  identical(cold_workers, 1L),
  identical(cold$value, warm$value),
  identical(cold$value$summary, warm$value$summary),
  identical(cold$value$coefficients, warm$value$coefficients),
  identical(cold$value$selection_stability, warm$value$selection_stability)
)

cat(sprintf(
  paste0(
    "Penalized adaptive bootstrap validation passed: complete result identical; ",
    "cold %d-worker %.3fs, warm %d-worker %.3fs, speedup %.2fx ",
    "(500 resamples x 2 methods).\n"
  ),
  cold_workers,
  cold$seconds,
  warm_workers,
  warm$seconds,
  cold$seconds / warm$seconds
))
