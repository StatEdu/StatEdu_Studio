if (.Platform$OS.type == "windows" && !isTRUE(l10n_info()[["UTF-8"]])) {
  validation_locale <- Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
  if (is.na(validation_locale) || !isTRUE(l10n_info()[["UTF-8"]])) {
    stop("PLS bootstrap parallel validation requires a Windows UTF-8 locale.", call. = FALSE)
  }
}

suppressPackageStartupMessages(library(seminr))
source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "setup_custom_model_canvas_structural_pls_engine.R"), encoding = "UTF-8")

set.seed(20260901L)
n <- 120L
latent_a <- stats::rnorm(n)
latent_b <- .45 * latent_a + stats::rnorm(n, sd = .85)
latent_c <- .30 * latent_a + .50 * latent_b + stats::rnorm(n, sd = .75)
data <- data.frame(
  A1 = latent_a + stats::rnorm(n, sd = .35),
  A2 = latent_a + stats::rnorm(n, sd = .40),
  A3 = latent_a + stats::rnorm(n, sd = .45),
  B1 = latent_b + stats::rnorm(n, sd = .35),
  B2 = latent_b + stats::rnorm(n, sd = .40),
  B3 = latent_b + stats::rnorm(n, sd = .45),
  C1 = latent_c + stats::rnorm(n, sd = .35),
  C2 = latent_c + stats::rnorm(n, sd = .40),
  C3 = latent_c + stats::rnorm(n, sd = .45)
)
measurement_model <- constructs(
  composite("A", multi_items("A", 1:3), weights = mode_A),
  composite("B", multi_items("B", 1:3), weights = mode_A),
  composite("C", multi_items("C", 1:3), weights = mode_A)
)
structural_model <- relationships(
  paths(from = "A", to = c("B", "C")),
  paths(from = "B", to = "C")
)
fit <- suppressMessages(estimate_pls(data, measurement_model, structural_model))

previous_workers <- getOption("statedu.pls.bootstrap.workers")
on.exit(options(statedu.pls.bootstrap.workers = previous_workers), add = TRUE)

options(statedu.pls.bootstrap.workers = 1L)
serial <- suppressMessages(structural_canvas_run_plsc_bootstrap(
  fit, nboot = 60L, seed = 24680L, apply_plsc = FALSE
))
available_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
if (length(available_cores) != 1L || !is.finite(available_cores) || available_cores < 1L) {
  available_cores <- suppressWarnings(parallel::detectCores(logical = TRUE))
}
if (length(available_cores) != 1L || !is.finite(available_cores) || available_cores < 1L) available_cores <- 1L
expected_workers <- min(2L, as.integer(available_cores))
options(statedu.pls.bootstrap.workers = expected_workers)
parallel <- suppressMessages(structural_canvas_run_plsc_bootstrap(
  fit, nboot = 60L, seed = 24680L, apply_plsc = FALSE
))

stopifnot(
  identical(serial, parallel),
  identical(serial$requested_nboot, 60L),
  identical(serial$nboot, 60L),
  identical(serial$valid_positions, seq_len(60L)),
  identical(structural_canvas_pls_bootstrap_workers(60L), expected_workers)
)

options(statedu.pls.bootstrap.workers = NULL)
stopifnot(identical(structural_canvas_pls_bootstrap_workers(24L), 1L))

cat("PLS bootstrap serial/parallel seed reproducibility validation passed.\n")
