fixture_path <- file.path("sample", "HolzingerSwineford1939.csv")
model_path <- file.path("sample", "pls_external_benchmark.stmodel")
expected_sha256 <- "140519c3e46920b38191d4cd9415fa33ddc40633294e6d3e30af82242f7b6204"

required_files <- c(fixture_path, model_path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Required repository fixture(s) missing: ", paste(missing_files, collapse = ", "), call. = FALSE)
}

tracked_files <- suppressWarnings(system2("git", c("ls-files", "--", required_files), stdout = TRUE, stderr = TRUE))
tracked_files <- gsub("\\\\", "/", trimws(tracked_files))
untracked_files <- required_files[!gsub("\\\\", "/", required_files) %in% tracked_files]
if (length(untracked_files) > 0L) {
  stop(
    "Required repository fixture(s) exist locally but are not tracked by Git: ",
    paste(untracked_files, collapse = ", "),
    call. = FALSE
  )
}

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("digest is required for repository fixture validation.", call. = FALSE)
}
actual_sha256 <- tolower(digest::digest(file = fixture_path, algo = "sha256", serialize = FALSE))
if (!identical(actual_sha256, expected_sha256)) {
  stop(
    "Holzinger-Swineford fixture SHA-256 mismatch: expected ", expected_sha256,
    ", found ", actual_sha256, ".",
    call. = FALSE
  )
}

if (!requireNamespace("lavaan", quietly = TRUE)) {
  stop("lavaan is required for repository fixture validation.", call. = FALSE)
}
fixture <- utils::read.csv(fixture_path, check.names = FALSE, stringsAsFactors = FALSE)
reference <- as.data.frame(lavaan::HolzingerSwineford1939)
required_columns <- c("id", "sex", "ageyr", "agemo", "school", "grade", paste0("x", 1:9))
if (!identical(names(fixture), required_columns) || nrow(fixture) != 301L) {
  stop("Holzinger-Swineford fixture dimensions or column order changed.", call. = FALSE)
}
if (!isTRUE(all.equal(fixture[paste0("x", 1:9)], reference[paste0("x", 1:9)], check.attributes = FALSE))) {
  stop("Holzinger-Swineford x1-x9 values differ from lavaan::HolzingerSwineford1939.", call. = FALSE)
}

cat("Repository fixture validation passed.\n")
