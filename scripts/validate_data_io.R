script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_data_io.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "data_io.R"))

message("Checking copied CSV reads...")
csv_path <- tempfile(pattern = "easyflow source 한글 space ", fileext = ".csv")
writeLines(c("x,y", "1,2", "3,4"), csv_path, useBytes = TRUE)
csv_data <- read_input_data(csv_path, "source.csv", csv_header = TRUE)
stopifnot(nrow(csv_data) == 2)
stopifnot(identical(names(csv_data), c("x", "y")))

message("Checking CP949 Korean CSV reads...")
cp949_path <- tempfile(pattern = "easyflow cp949 csv ", fileext = ".csv")
cp949_text <- c(
  "성별,학년,월활동횟수,보람",
  "남자,3,1,2",
  "여자,2,3,4"
)
writeBin(iconv(paste(cp949_text, collapse = "\r\n"), from = "UTF-8", to = "CP949", toRaw = TRUE)[[1]], cp949_path)
cp949_data <- read_input_data(cp949_path, "source.csv", csv_header = TRUE)
stopifnot(nrow(cp949_data) == 2)
stopifnot(identical(names(cp949_data), c("성별", "학년", "월활동횟수", "보람")))
stopifnot(identical(as.character(cp949_data$성별), c("남자", "여자")))

message("Checking Korean XLSX reads...")
xlsx_path <- tempfile(pattern = "easyflow korean xlsx ", fileext = ".xlsx")
openxlsx::write.xlsx(
  data.frame(
    성별 = c("남자", "여자"),
    학년 = c(3, 2),
    월활동횟수 = c(1, 3),
    check.names = FALSE
  ),
  xlsx_path,
  overwrite = TRUE
)
xlsx_data <- read_input_data(xlsx_path, "source.xlsx", csv_header = TRUE)
stopifnot(nrow(xlsx_data) == 2)
stopifnot(identical(names(xlsx_data), c("성별", "학년", "월활동횟수")))
stopifnot(identical(as.character(xlsx_data$성별), c("남자", "여자")))

message("Checking copied DAT reads...")
dat_path <- tempfile(pattern = "easyflow source dat ", fileext = ".dat")
writeLines(c("1 2", "3 4"), dat_path, useBytes = TRUE)
dat_data <- read_input_data(dat_path, "source.dat", dat_has_names = FALSE)
stopifnot(nrow(dat_data) == 2)
stopifnot(ncol(dat_data) == 2)

message("Checking temp copy helper...")
copy_path <- copy_data_file_for_reading(csv_path, "source.csv")
stopifnot(file.exists(copy_path))
stopifnot(grepl("^easyflow_data_", basename(copy_path)))
unlink(copy_path)
unlink(c(csv_path, cp949_path, xlsx_path, dat_path))

message("All data IO validations passed.")
