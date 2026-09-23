script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE))) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1L]])
} else {
  "scripts/benchmark_analysis_bootstrap_audit.R"
}
default_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(
  Sys.getenv("STATEDU_BENCHMARK_REPO", default_root),
  winslash = "/",
  mustWork = TRUE
)
setwd(repo_root)

if (identical(.Platform$OS.type, "windows")) {
  Sys.setenv(LC_ALL = "Korean_Korea.utf8", LANG = "Korean_Korea.utf8")
  invisible(suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE)))
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules(dir = file.path(repo_root, "R"))

measure <- function(analysis, repetitions, expression) {
  gc(FALSE)
  started <- proc.time()[["elapsed"]]
  value <- force(expression)
  elapsed <- unname(proc.time()[["elapsed"]] - started)
  if (is.null(value)) stop(sprintf("%s returned NULL.", analysis), call. = FALSE)
  data.frame(
    analysis = analysis,
    repetitions = as.integer(repetitions),
    elapsed_seconds = elapsed,
    repetitions_per_second = as.numeric(repetitions) / elapsed,
    stringsAsFactors = FALSE
  )
}

results <- list()

set.seed(20260826L)
regression_data <- data.frame(
  x1 = stats::rnorm(240L),
  x2 = stats::rnorm(240L),
  x3 = stats::rnorm(240L),
  x4 = stats::rnorm(240L)
)
regression_data$y <- 0.7 + 0.8 * regression_data$x1 - 0.5 * regression_data$x2 +
  0.3 * regression_data$x3 + stats::rexp(240L, rate = 1.2) - 1 / 1.2
results[[length(results) + 1L]] <- measure(
  "Regression coefficient bootstrap",
  5000L,
  bootstrap_coef_table(
    regression_data,
    y ~ x1 + x2 + x3 + x4,
    r = 5000L,
    seed = 20260826L,
    ci_method = "bias_corrected"
  )
)

set.seed(20260826L)
subject_effect <- stats::rnorm(120L)
rater_matrix <- cbind(
  r1 = subject_effect + stats::rnorm(120L, sd = .55),
  r2 = subject_effect + stats::rnorm(120L, sd = .60),
  r3 = subject_effect + stats::rnorm(120L, sd = .50),
  r4 = subject_effect + stats::rnorm(120L, sd = .65)
)
results[[length(results) + 1L]] <- measure(
  "Inter-rater ICC bootstrap CI",
  1000L,
  interrater_icc_bootstrap_ci(
    rater_matrix,
    model = "icc2",
    agreement = TRUE,
    average = FALSE,
    resamples = 1000L,
    seed = 20260826L
  )
)

set.seed(20260826L)
penalized_data <- data.frame(
  x1 = stats::rnorm(90L),
  x2 = stats::rnorm(90L),
  x3 = stats::rnorm(90L),
  x4 = stats::rnorm(90L),
  x5 = stats::rnorm(90L)
)
penalized_data$y <- 0.4 + 0.9 * penalized_data$x1 - 0.55 * penalized_data$x2 +
  stats::rnorm(90L, sd = .75)
penalized_formula <- y ~ x1 + x2 + x3 + x4 + x5
penalized_fit <- stats::lm(penalized_formula, data = penalized_data)
penalized_seed <- 20260826L
results[[length(results) + 1L]] <- measure(
  "Penalized selection bootstrap",
  500L,
  fit_penalized_models(
    list(list(
      formula = penalized_formula,
      coef_table = data.frame(
        Term = names(stats::coef(penalized_fit)),
        B = as.numeric(stats::coef(penalized_fit)),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    )),
    penalized_data,
    seed = penalized_seed,
    alpha_grid = c(.25, .5, .75),
    selection_bootstrap_resamples = 500L
  )
)

survival_path <- file.path(repo_root, "scripts", "fixtures", "survival_validation.csv")
survival_data <- utils::read.csv(survival_path, check.names = FALSE)
survival_data$status <- survival_data$status == 1L
survival_data$sex <- factor(survival_data$sex)
survival_fit <- survival::coxph(
  survival::Surv(time, status) ~ sex + age,
  data = survival_data,
  model = TRUE,
  x = TRUE
)
results[[length(results) + 1L]] <- measure(
  "Cox adjusted-survival bootstrap",
  2000L,
  survival_adjusted_curve(
    survival_fit,
    survival_data,
    group = "sex",
    bootstrap_reps = 2000L,
    seed = 20260826L,
    selected_times = c(100, 200, 400)
  )
)

result <- do.call(rbind, results)
result$repo <- repo_root
result$commit <- tryCatch(
  system2("git", c("-C", shQuote(repo_root), "rev-parse", "--short", "HEAD"), stdout = TRUE)[[1L]],
  error = function(error) ""
)
print(result, row.names = FALSE)

output_path <- trimws(Sys.getenv("STATEDU_BENCHMARK_OUTPUT", ""))
if (nzchar(output_path)) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, output_path)
}
