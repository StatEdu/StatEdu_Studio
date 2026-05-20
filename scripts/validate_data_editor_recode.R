script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_data_editor_recode.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "data_io.R"))
source(file.path(repo_root, "R", "data_editor_recode.R"))

message("Checking same-variable recoding rules...")
rules <- data.frame(old = c("1", "2", ""), new = c("10", "20", ""), stringsAsFactors = FALSE)
numeric_result <- recode_same_values(c(1, 2, 3, NA), rules, keep_unmatched = TRUE)
stopifnot(identical(numeric_result, c(10, 20, 3, NA)))

missing_result <- recode_same_values(c(1, 2, 3), rules, keep_unmatched = FALSE)
stopifnot(identical(missing_result, c(10, 20, NA)))

label_rules <- data.frame(old = c("1", "2"), new = c("low", "high"), stringsAsFactors = FALSE)
label_result <- recode_same_values(c(1, 2, 3), label_rules, keep_unmatched = TRUE)
stopifnot(identical(label_result, c("low", "high", "3")))

message("All data editor recoding validations passed.")
