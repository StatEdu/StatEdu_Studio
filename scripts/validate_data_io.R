script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_data_io.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "data_io.R"))

korean_data <- data.frame(
  "\uc131\ubcc4" = c("\ub0a8\uc790", "\uc5ec\uc790"),
  "\ud559\ub144" = c(3, 2),
  "\ud604\uc7ac\uadfc\ubb34\ubd80\uc11c" = c(1, 3),
  check.names = FALSE
)
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
  "\uc131\ubcc4,\ud559\ub144,\ud604\uc7ac\uadfc\ubb34\ubd80\uc11c",
  "\ub0a8\uc790,3,1",
  "\uc5ec\uc790,2,3"
)
writeBin(iconv(paste(cp949_text, collapse = "\r\n"), from = "UTF-8", to = "CP949", toRaw = TRUE)[[1]], cp949_path)
cp949_data <- read_input_data(cp949_path, "source.csv", csv_header = TRUE)
stopifnot(nrow(cp949_data) == 2)
stopifnot(identical(names(cp949_data), names(korean_data)))
stopifnot(identical(as.character(cp949_data[["\uc131\ubcc4"]]), korean_data[["\uc131\ubcc4"]]))

message("Checking Korean XLSX reads...")
xlsx_path <- tempfile(pattern = "easyflow korean xlsx ", fileext = ".xlsx")
openxlsx::write.xlsx(korean_data, xlsx_path, overwrite = TRUE)
xlsx_data <- read_input_data(xlsx_path, "source.xlsx", csv_header = TRUE)
stopifnot(nrow(xlsx_data) == 2)
stopifnot(identical(names(xlsx_data), names(korean_data)))
stopifnot(identical(as.character(xlsx_data[["\uc131\ubcc4"]]), korean_data[["\uc131\ubcc4"]]))

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
unlink(c(csv_path, cp949_path, xlsx_path, dta_path, xpt_path, sas_path, dat_path))

message("All data IO validations passed.")
