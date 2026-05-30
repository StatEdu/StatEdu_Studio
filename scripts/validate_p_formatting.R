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
expect_identical(format_p("<.001"), "<.001", "legacy less-than p-value")
expect_identical(format_p(0.001), ".001", "threshold p-value")
expect_identical(format_p(0.0456), ".046", "regular p-value")
excel_table <- data.frame(
  p = c(".000", ".179", ".1799", "<.0012", ".000 3"),
  `p adjusted` = c(0.0004, 0.001, 0.0456, NA, ""),
  check.names = FALSE
)
excel_table <- excel_normalize_p_columns(excel_table)
expect_identical(excel_table$p[[1]], "<.001", "Excel rounded zero p-value")
expect_identical(excel_table$p[[2]], ".179", "Excel regular p-value")
expect_identical(excel_table$p[[3]], ".1799", "Excel compact marker p-value")
expect_identical(excel_table$p[[4]], "<.0012", "Excel less-than marker p-value")
expect_identical(excel_table$p[[5]], "<.001 3", "Excel spaced marker p-value")
expect_identical(excel_table$`p adjusted`[[1]], "<.001", "Excel numeric small adjusted p-value")
expect_identical(excel_table$`p adjusted`[[2]], ".001", "Excel numeric threshold adjusted p-value")
expect_identical(excel_table$`p adjusted`[[3]], ".046", "Excel numeric regular adjusted p-value")
message("All p-value formatting validations passed.")
