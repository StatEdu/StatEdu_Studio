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
unlink(c(csv_path, dat_path))

message("All data IO validations passed.")
