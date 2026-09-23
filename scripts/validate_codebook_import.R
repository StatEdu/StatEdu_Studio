if (.Platform$OS.type == "windows") {
  suppressWarnings(try(Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8"), silent = TRUE))
}

source(file.path("R", "utils.R"), encoding = "UTF-8")
source(file.path("R", "settings_io.R"))
source(file.path("R", "data_io.R"))
source(file.path("R", "data_category_labels.R"))
source(file.path("R", "result_labels.R"))
source(file.path("R", "codebook_io.R"), encoding = "UTF-8")

stopifnot(requireNamespace("openxlsx", quietly = TRUE))
stopifnot(requireNamespace("readxl", quietly = TRUE))

columns <- c(
  "\uBCC0\uC218\uBA85",
  "\uBCC0\uC218\uB77C\uBCA8",
  "\uBCC0\uC218\uC720\uD615",
  "\uAC12",
  "\uAC12\uB77C\uBCA8"
)
valid <- data.frame(
  c("group", "", "score", "comment"),
  c("Group", "", "Score", "Comment"),
  c("\uBC94\uC8FC\uD615", "", "\uC5F0\uC18D\uD615", "\uBB38\uC790\uD615"),
  c("A", "B", "", ""),
  c("Treatment", "Control", "", ""),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
names(valid) <- columns

valid_path <- tempfile(fileext = ".xlsx")
invalid_path <- tempfile(fileext = ".xlsx")
openxlsx::write.xlsx(stats::setNames(list(valid), "\uCF54\uB529\uBD81"), valid_path)

codebook <- read_codebook_excel(valid_path)
stopifnot(nrow(codebook$variables) == 3L)
stopifnot(codebook_measurement("\uBB38\uC790\uD615") == "category")
stopifnot(codebook$source_rows[["\uBCC0\uC218\uBA85"]][[2]] == "group")

data <- data.frame(
  group = c("A", "B", "A"),
  score = c(1, 2, 3),
  comment = c("good", "average", "good"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
info <- variable_summary_table(data, NULL, data)
matched <- match_codebook_to_variables(codebook, info)
stopifnot(length(matched$matched_names) == 3L)
stopifnot(matched$variables$measurement[matched$variables[["\uBCC0\uC218\uBA85"]] == "comment"] == "category")

category_table <- codebook_category_table(NULL, info, matched)
labels <- category_value_label_lookup_static(category_table)
stopifnot(identical(unname(labels$group[["A"]]), "Treatment"))
stopifnot(identical(unname(labels$group[["B"]]), "Control"))

invalid <- valid
invalid[["\uBCC0\uC218\uC720\uD615"]][[1]] <- "unsupported"
openxlsx::write.xlsx(stats::setNames(list(invalid), "\uCF54\uB529\uBD81"), invalid_path)
invalid_error <- tryCatch(
  {
    read_codebook_excel(invalid_path)
    NULL
  },
  error = function(error) error
)
stopifnot(inherits(invalid_error, "error"))

unlink(c(valid_path, invalid_path), force = TRUE)
cat("codebook-import-validation-ok\n")
