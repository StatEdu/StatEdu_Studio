Sys.setlocale("LC_ALL", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE_DIR = file.path(tempdir(), "syntax-example-cache"))
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
folder <- "outputs/regression_syntax_example"
dir.create(folder, recursive = TRUE, showWarnings = FALSE)
path <- file.path(folder, "mtcars_regression.sav")
haven::write_sav(datasets::mtcars[, c("mpg", "wt", "hp")], path)
data <- prepare_data(read_current_data_file(list(path = path, name = basename(path)), list()))
info <- data.frame(name = names(data), var_label = c("연비", "차량 중량", "마력"),
                   measurement = "continuous", stringsAsFactors = FALSE)
spec <- list(
  VERSION = 1L, STUDIO_VERSION = trimws(readLines("VERSION", warn = FALSE)),
  DATA = basename(path), DATA_HASH = regression_syntax_data_hash(data),
  DEPENDENTS = "mpg", PREDICTORS = c("wt", "hp"),
  MEASUREMENTS = regression_syntax_measurements(info), REFERENCES = list(),
  MISSING = "LISTWISE", CI_METHOD = "bias_corrected", BOOTSTRAP = 1000L,
  SEED = 20260908L, RESIDUAL_DIAGNOSTICS = TRUE, AUTO_METHOD = FALSE,
  SHOW_SR2 = TRUE, SHOW_F2 = TRUE, SHOW_VIF = TRUE, OUTPUT_STYLE = "standard",
  BLOCK1 = c("wt", "hp"), BLOCK2 = character(0), BLOCK3 = character(0)
)
command <- regression_syntax_text(spec)
writeLines(enc2utf8(command), file.path(folder, "mtcars_regression.stcmd"), useBytes = TRUE)
parsed <- parse_regression_syntax(command)
validate_regression_syntax_context(parsed, data, info, character(0))
prepared <- prepare_regression_syntax(parsed, data, info)
stopifnot(length(prepared$results) == 1L, length(prepared$jobs) == 0L)
result <- prepared$results[[1]]
stopifnot(isTRUE(all.equal(unname(coef(result$model)), unname(coef(lm(mpg ~ wt + hp, data))), tolerance = 1e-12)))
write_analysis_results_html(prepared$results, file.path(folder, "mtcars_regression.html"),
  variable_table = info, show_sr2 = TRUE, show_f2 = TRUE, show_vif = TRUE)
cat(command, "\n\n")
print(result$coef_table)
cat("N =", result$n, "R2 =", result$r_squared, "Adjusted R2 =", result$adjusted_r_squared,
    "F =", result$f_statistic, "p =", result$f_p, "\n")
