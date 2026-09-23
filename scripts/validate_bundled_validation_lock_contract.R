#!/usr/bin/env Rscript

# Static and runtime contract regression for the approved recursive cSEM
# validation-package lock. Mutations are performed only in memory or in a
# temporary file; the approved lock and bundled runtime are never modified.

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source(
  file.path(repo_root, "scripts", "validate_bundled_validation_packages.R"),
  local = TRUE,
  encoding = "UTF-8"
)
source(file.path(repo_root, "R", "app_bootstrap.R"), local = TRUE, encoding = "UTF-8")

expect_failure <- function(expression, pattern) {
  message <- tryCatch(
    {
      force(expression)
      ""
    },
    error = conditionMessage
  )
  if (!nzchar(message) || !grepl(pattern, message, fixed = TRUE)) {
    stop(
      "Expected fail-closed message containing '", pattern,
      "'; observed '", message, "'.",
      call. = FALSE
    )
  }
  invisible(message)
}

expected <- bundled_validation_read_expected_contract(repo_root)
stopifnot(
  identical(names(expected), c("Package", "Version")),
  identical(nrow(expected), 72L),
  identical(attr(expected, "sha256"), bundled_validation_expected_lock_sha256()),
  identical(expected$Package, expected$Package[order(enc2utf8(expected$Package), method = "radix")])
)

runtime_library <- normalizePath(
  file.path(repo_root, "packaging", "electron", "runtime", "R-4.5.3", "library"),
  winslash = "/",
  mustWork = TRUE
)
runtime_database <- installed.packages(lib.loc = runtime_library, noCache = TRUE)
validation_roots <- names(bundled_validation_packages)
actual <- bundled_validation_closure_records(runtime_database, validation_roots)
bundled_validation_assert_root_contract(expected, validation_roots, bundled_validation_packages)
bundled_validation_assert_no_base_or_recommended(expected, runtime_database)
bundled_validation_assert_exact_contract(expected, actual)
stopifnot(identical(actual, bundled_validation_canonical_records(expected)))

missing <- actual[-1L, , drop = FALSE]
expect_failure(
  bundled_validation_assert_exact_contract(expected, missing),
  "Bundled runtime validation dependency closure is incomplete"
)

unexpected <- rbind(
  actual,
  data.frame(Package = "stateduUnexpectedDependency", Version = "1.0.0", check.names = FALSE)
)
expect_failure(
  bundled_validation_assert_exact_contract(expected, unexpected),
  "Bundled runtime validation dependency closure has unexpected package(s)"
)

version_mismatch <- actual
version_mismatch$Version[version_mismatch$Package == "cSEM"] <- "0.6.0"
expect_failure(
  bundled_validation_assert_exact_contract(expected, version_mismatch),
  "Bundled runtime validation dependency version mismatch"
)

base_injected <- rbind(
  expected,
  data.frame(Package = "stats", Version = as.character(utils::packageVersion("stats")), check.names = FALSE)
)
expect_failure(
  bundled_validation_assert_no_base_or_recommended(base_injected, runtime_database),
  "contains base/recommended package(s): stats"
)

wrong_schema <- expected
names(wrong_schema) <- c("Name", "Version")
expect_failure(
  bundled_validation_canonical_records(wrong_schema),
  "CSV schema must be exactly Package,Version"
)

unsorted <- expected[rev(seq_len(nrow(expected))), , drop = FALSE]
expect_failure(
  bundled_validation_canonical_records(unsorted, require_sorted = TRUE),
  "is not canonically sorted by Package"
)

tampered_path <- tempfile(fileext = ".csv")
on.exit(unlink(tampered_path), add = TRUE)
tampered_text <- sub('"cSEM","0.6.1"', '"cSEM","0.6.0"', bundled_validation_canonical_csv(expected), fixed = TRUE)
writeBin(charToRaw(enc2utf8(tampered_text)), tampered_path)
expect_failure(
  bundled_validation_read_expected_contract(repo_root, path = tampered_path),
  "expected lock SHA-256 mismatch"
)

cat(
  "Bundled validation expected-lock contract PASS: rows=", nrow(expected),
  "; sha256=", attr(expected, "sha256"),
  "; missing/additional/version/schema/order/hash/base-recommended mutations all failed closed.\n",
  sep = ""
)
