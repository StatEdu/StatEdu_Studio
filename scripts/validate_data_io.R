script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_data_io.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "data_io.R"))

ko_sex <- statedu_utf8("ec84b1ebb384")
ko_grade <- statedu_utf8("ed9599eb8584")
ko_department <- statedu_utf8("ed9884ec9eaceab7bcebacb4ebb680ec849c")
ko_male <- statedu_utf8("eb8298eca790")
ko_female <- statedu_utf8("ec97acec9e90")

korean_data <- data.frame(
  V1 = c(ko_male, ko_female),
  V2 = c(3, 2),
  V3 = c(1, 3),
  check.names = FALSE
)
names(korean_data) <- c(ko_sex, ko_grade, ko_department)
portable_data <- data.frame(
  sex = c("male", "female"),
  grade = c(3, 2),
  dept = c(1, 3),
  check.names = FALSE
)

message("Checking copied CSV reads...")
csv_path <- tempfile(pattern = "easyflow source space ", fileext = ".csv")
writeLines(c("x,y", "1,2", "3,4"), csv_path, useBytes = TRUE)
csv_data <- read_input_data(csv_path, "source.csv", csv_header = TRUE)
stopifnot(nrow(csv_data) == 2)
stopifnot(identical(names(csv_data), c("x", "y")))

message("Checking CP949 Korean CSV reads...")
cp949_path <- tempfile(pattern = "easyflow cp949 csv ", fileext = ".csv")
cp949_text <- c(
  paste(c(ko_sex, ko_grade, ko_department), collapse = ","),
  paste(c(ko_male, "3", "1"), collapse = ","),
  paste(c(ko_female, "2", "3"), collapse = ",")
)
writeBin(iconv(paste(cp949_text, collapse = "\r\n"), from = "UTF-8", to = "CP949", toRaw = TRUE)[[1]], cp949_path)
cp949_data <- read_input_data(cp949_path, "source.csv", csv_header = TRUE)
stopifnot(nrow(cp949_data) == 2)
stopifnot(identical(names(cp949_data), names(korean_data)))
stopifnot(identical(as.character(cp949_data[[ko_sex]]), korean_data[[ko_sex]]))

message("Checking Korean XLSX reads...")
xlsx_path <- tempfile(pattern = "easyflow korean xlsx ", fileext = ".xlsx")
suppressWarnings(openxlsx::write.xlsx(korean_data, xlsx_path, overwrite = TRUE))
xlsx_data <- read_input_data(xlsx_path, "source.xlsx", csv_header = TRUE)
stopifnot(nrow(xlsx_data) == 2)
stopifnot(identical(names(xlsx_data), names(korean_data)))
stopifnot(identical(as.character(xlsx_data[[ko_sex]]), korean_data[[ko_sex]]))

message("Checking XLSX sheet and start-cell reads...")
multi_xlsx_path <- tempfile(pattern = "easyflow multi sheet xlsx ", fileext = ".xlsx")
workbook <- openxlsx::createWorkbook()
openxlsx::addWorksheet(workbook, "Notes")
openxlsx::writeData(workbook, "Notes", data.frame(note = "not data"))
openxlsx::addWorksheet(workbook, "Data")
openxlsx::writeData(workbook, "Data", "title row", startCol = 1, startRow = 1, colNames = FALSE)
suppressWarnings(openxlsx::writeData(workbook, "Data", korean_data, startCol = 2, startRow = 4))
openxlsx::saveWorkbook(workbook, multi_xlsx_path, overwrite = TRUE)
stopifnot(identical(excel_sheet_names(multi_xlsx_path, "source.xlsx"), c("Notes", "Data")))
configured_xlsx_data <- read_input_data(
  multi_xlsx_path,
  "source.xlsx",
  excel_sheet = "Data",
  excel_start_cell = "B4",
  excel_col_names = TRUE
)
stopifnot(nrow(configured_xlsx_data) == 2)
stopifnot(identical(names(configured_xlsx_data), names(korean_data)))
preview_xlsx_data <- read_excel_preview(
  multi_xlsx_path,
  "source.xlsx",
  sheet = "Data",
  start_cell = "B4",
  col_names = TRUE,
  n_max = 1
)
stopifnot(nrow(preview_xlsx_data) == 1)
pending_excel_file <- list(path = multi_xlsx_path, name = "source.xlsx", excel_pending = TRUE)
stopifnot(isTRUE(valid_pending_excel_file_value(pending_excel_file)))
stopifnot(is.null(current_data_file_value(NULL, pending_excel_file)))
invalid_pending_excel_file <- list(path = "", name = "", excel_pending = TRUE)
stopifnot(!isTRUE(valid_pending_excel_file_value(invalid_pending_excel_file)))
missing_pending_excel_file <- list(path = file.path(tempdir(), "missing.xlsx"), name = "missing.xlsx", excel_pending = TRUE)
stopifnot(!isTRUE(valid_pending_excel_file_value(missing_pending_excel_file)))

message("Checking legacy XLS reads...")
xls_example <- readxl::readxl_example("datasets.xls")
if (nzchar(xls_example) && file.exists(xls_example)) {
  xls_data <- read_input_data(xls_example, "source.xls", csv_header = TRUE)
  stopifnot(nrow(xls_data) > 0)
  stopifnot(ncol(xls_data) > 0)
} else {
  warning("readxl datasets.xls example was not available; skipped XLS validation.")
}

message("Checking Stata DTA reads...")
dta_path <- tempfile(pattern = "easyflow_stata_", fileext = ".dta")
haven::write_dta(portable_data, dta_path)
dta_data <- read_input_data(dta_path, "source.dta", csv_header = TRUE)
stopifnot(nrow(dta_data) == 2)
stopifnot(identical(names(dta_data), names(portable_data)))

message("Checking SAS XPT reads...")
xpt_path <- tempfile(pattern = "easyflow_sas_xpt_", fileext = ".xpt")
haven::write_xpt(portable_data, xpt_path)
xpt_data <- read_input_data(xpt_path, "source.xpt", csv_header = TRUE)
stopifnot(nrow(xpt_data) == 2)
stopifnot(identical(names(xpt_data), names(portable_data)))

message("Checking SAS7BDAT reads...")
if (exists("write_sas", envir = asNamespace("haven"), mode = "function")) {
  sas_path <- tempfile(pattern = "easyflow_sas7bdat_", fileext = ".sas7bdat")
  haven::write_sas(portable_data, sas_path)
  sas_data <- read_input_data(sas_path, "source.sas7bdat", csv_header = TRUE)
  stopifnot(nrow(sas_data) == 2)
  stopifnot(identical(names(sas_data), names(portable_data)))
} else {
  warning("haven::write_sas is not available; skipped SAS7BDAT write/read validation.")
  sas_path <- character(0)
}

message("Checking copied DAT reads...")
dat_path <- tempfile(pattern = "easyflow source dat ", fileext = ".dat")
writeLines(c("1 2", "3 4"), dat_path, useBytes = TRUE)
dat_data <- read_input_data(dat_path, "source.dat", dat_has_names = FALSE)
stopifnot(nrow(dat_data) == 2)
stopifnot(ncol(dat_data) == 2)

message("Checking supported extension list...")
stopifnot(supported_data_file_extension("source.sav"))
stopifnot(supported_data_file_extension("source.sas7bdat"))
stopifnot(supported_data_file_extension("source.xpt"))
stopifnot(supported_data_file_extension("source.dta"))
stopifnot(supported_data_file_extension("source.xlsx"))
stopifnot(supported_data_file_extension("source.xls"))
stopifnot(!supported_data_file_extension("source.txt"))

message("Checking temp copy helper...")
copy_path <- copy_data_file_for_reading(csv_path, "source.csv")
stopifnot(file.exists(copy_path))
stopifnot(grepl("^easyflow_data_", basename(copy_path)))
unlink(copy_path)
unlink(c(csv_path, cp949_path, xlsx_path, multi_xlsx_path, dta_path, xpt_path, sas_path, dat_path))

message("All data IO validations passed.")
