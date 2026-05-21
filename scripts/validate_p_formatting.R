all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_p_formatting.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "app_bootstrap.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "app_bootstrap.R"))
source_app_modules(dir = file.path(repo_root, "R"))

expect_identical <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    stop(sprintf("%s: expected %s, got %s", label, expected, actual), call. = FALSE)
  }
}

message("Checking shared p-value formatting...")
expect_identical(format_p(0), "<.001", "zero p-value")
expect_identical(format_p(0.0004), "<.001", "small p-value")
expect_identical(format_p(".000"), "<.001", "rounded string p-value")
expect_identical(format_p("0.000"), "<.001", "zero string p-value")
expect_identical(format_p(0.001), ".001", "threshold p-value")
expect_identical(format_p(0.0456), ".046", "regular p-value")
message("All p-value formatting validations passed.")
