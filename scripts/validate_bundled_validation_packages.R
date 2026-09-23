# Validate that pinned release-validation packages are fully self-contained in
# the bundled Electron R runtime. This script never consults a user library for
# package availability.

bundled_validation_expected_lock_file <- function(repo_root) {
  file.path(repo_root, "scripts", "bundled_validation_packages.expected.csv")
}

bundled_validation_expected_lock_sha256 <- function() {
  "cf6e4175d47ea943cd6d017951357ee87b1b615f18c6f32799b046b179447674"
}

bundled_validation_expected_lock_rows <- function() 72L

bundled_validation_arg_value <- function(args, name, default = "") {
  prefix <- paste0("--", name, "=")
  match <- args[startsWith(args, prefix)]
  if (length(match) == 0L) return(default)
  sub(prefix, "", match[[1L]], fixed = TRUE)
}

bundled_validation_canonical_records <- function(records, require_sorted = FALSE) {
  if (!is.data.frame(records) || !identical(names(records), c("Package", "Version"))) {
    stop("Bundled validation expected lock CSV schema must be exactly Package,Version.", call. = FALSE)
  }
  records <- data.frame(
    Package = as.character(records$Package),
    Version = as.character(records$Version),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (anyNA(records) || any(!nzchar(records$Package)) || any(!nzchar(records$Version))) {
    stop("Bundled validation expected lock contains a missing or empty Package/Version value.", call. = FALSE)
  }
  if (any(records$Package != trimws(records$Package)) || any(records$Version != trimws(records$Version))) {
    stop("Bundled validation expected lock contains leading or trailing whitespace.", call. = FALSE)
  }
  if (any(!grepl("^[[:alpha:]][[:alnum:].]*$", records$Package))) {
    stop("Bundled validation expected lock contains an invalid R package name.", call. = FALSE)
  }
  duplicated_packages <- unique(records$Package[duplicated(records$Package)])
  if (length(duplicated_packages) > 0L) {
    stop(
      "Bundled validation expected lock contains duplicate package(s): ",
      paste(duplicated_packages, collapse = ", "),
      call. = FALSE
    )
  }
  canonical_order <- order(enc2utf8(records$Package), method = "radix")
  if (isTRUE(require_sorted) && !identical(canonical_order, seq_len(nrow(records)))) {
    stop("Bundled validation expected lock is not canonically sorted by Package.", call. = FALSE)
  }
  records <- records[canonical_order, , drop = FALSE]
  rownames(records) <- NULL
  records
}

bundled_validation_canonical_csv <- function(records) {
  records <- bundled_validation_canonical_records(records)
  csv_quote <- function(value) {
    paste0('"', gsub('"', '""', enc2utf8(value), fixed = TRUE), '"')
  }
  lines <- c(
    '"Package","Version"',
    paste(csv_quote(records$Package), csv_quote(records$Version), sep = ",")
  )
  paste0(paste(lines, collapse = "\n"), "\n")
}

bundled_validation_read_expected_contract <- function(
  repo_root,
  path = bundled_validation_expected_lock_file(repo_root)
) {
  if (!file.exists(path)) {
    stop("Bundled validation expected lock is missing: ", path, call. = FALSE)
  }
  byte_count <- file.info(path)$size
  bytes <- readBin(path, what = "raw", n = byte_count)
  if (length(bytes) >= 3L && identical(as.integer(bytes[1:3]), c(239L, 187L, 191L))) {
    stop("Bundled validation expected lock must be UTF-8 without a BOM.", call. = FALSE)
  }
  text <- rawToChar(bytes)
  if (!validUTF8(text)) {
    stop("Bundled validation expected lock is not valid UTF-8.", call. = FALSE)
  }
  connection <- textConnection(text)
  on.exit(close(connection), add = TRUE)
  records <- tryCatch(
    utils::read.csv(
      connection, header = TRUE, stringsAsFactors = FALSE,
      check.names = FALSE, colClasses = "character", na.strings = NULL
    ),
    error = function(error) {
      stop("Bundled validation expected lock CSV cannot be parsed: ", conditionMessage(error), call. = FALSE)
    }
  )
  records <- bundled_validation_canonical_records(records, require_sorted = TRUE)
  if (!identical(nrow(records), bundled_validation_expected_lock_rows())) {
    stop(
      "Bundled validation expected lock row-count mismatch: expected ",
      bundled_validation_expected_lock_rows(), ", found ", nrow(records), ".",
      call. = FALSE
    )
  }
  canonical_bytes <- charToRaw(enc2utf8(bundled_validation_canonical_csv(records)))
  if (!identical(bytes, canonical_bytes)) {
    stop(
      "Bundled validation expected lock is not canonical CSV (UTF-8, LF, exact Package/Version header, quoted fields, final newline).",
      call. = FALSE
    )
  }
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("digest is required to verify the bundled validation expected-lock SHA-256.", call. = FALSE)
  }
  actual_sha256 <- tolower(digest::digest(bytes, algo = "sha256", serialize = FALSE))
  expected_sha256 <- bundled_validation_expected_lock_sha256()
  if (!identical(actual_sha256, expected_sha256)) {
    stop(
      "Bundled validation expected lock SHA-256 mismatch: expected ", expected_sha256,
      ", found ", actual_sha256, ".",
      call. = FALSE
    )
  }
  attr(records, "path") <- normalizePath(path, winslash = "/", mustWork = TRUE)
  attr(records, "sha256") <- actual_sha256
  records
}

bundled_validation_closure_records <- function(
  package_database,
  validation_roots,
  incomplete_message = "Bundled runtime validation dependency closure is incomplete"
) {
  missing_roots <- setdiff(validation_roots, rownames(package_database))
  if (length(missing_roots) > 0L) {
    stop(incomplete_message, ": ", paste(missing_roots, collapse = ", "), call. = FALSE)
  }
  dependency_map <- tools::package_dependencies(
    validation_roots,
    db = package_database,
    which = c("Depends", "Imports", "LinkingTo"),
    recursive = TRUE
  )
  closure <- unique(c(validation_roots, unlist(dependency_map, use.names = FALSE)))
  closure <- closure[!is.na(closure) & nzchar(closure)]
  base_packages <- rownames(package_database)[
    !is.na(package_database[, "Priority"]) &
      package_database[, "Priority"] %in% c("base", "recommended")
  ]
  required_closure <- setdiff(closure, base_packages)
  missing_closure <- setdiff(required_closure, rownames(package_database))
  if (length(missing_closure) > 0L) {
    stop(incomplete_message, ": ", paste(missing_closure, collapse = ", "), call. = FALSE)
  }
  records <- data.frame(
    Package = required_closure,
    Version = unname(package_database[required_closure, "Version"]),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  bundled_validation_canonical_records(records)
}

bundled_validation_assert_no_base_or_recommended <- function(records, package_database) {
  present <- intersect(records$Package, rownames(package_database))
  priority <- package_database[present, "Priority"]
  rejected <- present[!is.na(priority) & priority %in% c("base", "recommended")]
  if (length(rejected) > 0L) {
    stop(
      "Bundled validation non-base expected lock contains base/recommended package(s): ",
      paste(rejected, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

bundled_validation_contract_difference <- function(expected, actual) {
  expected <- bundled_validation_canonical_records(expected)
  actual <- bundled_validation_canonical_records(actual)
  missing <- setdiff(expected$Package, actual$Package)
  unexpected <- setdiff(actual$Package, expected$Package)
  common <- intersect(expected$Package, actual$Package)
  expected_version <- expected$Version[match(common, expected$Package)]
  actual_version <- actual$Version[match(common, actual$Package)]
  changed <- common[expected_version != actual_version]
  version_mismatch <- data.frame(
    Package = changed,
    Expected = expected$Version[match(changed, expected$Package)],
    Actual = actual$Version[match(changed, actual$Package)],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  list(missing = missing, unexpected = unexpected, version_mismatch = version_mismatch)
}

bundled_validation_assert_exact_contract <- function(
  expected,
  actual,
  missing_message = "Bundled runtime validation dependency closure is incomplete",
  unexpected_message = "Bundled runtime validation dependency closure has unexpected package(s)",
  version_message = "Bundled runtime validation dependency version mismatch"
) {
  difference <- bundled_validation_contract_difference(expected, actual)
  failures <- character()
  if (length(difference$missing) > 0L) {
    failures <- c(failures, paste0(missing_message, ": ", paste(difference$missing, collapse = ", ")))
  }
  if (length(difference$unexpected) > 0L) {
    failures <- c(failures, paste0(unexpected_message, ": ", paste(difference$unexpected, collapse = ", ")))
  }
  if (nrow(difference$version_mismatch) > 0L) {
    details <- paste0(
      difference$version_mismatch$Package, " expected ", difference$version_mismatch$Expected,
      ", found ", difference$version_mismatch$Actual
    )
    failures <- c(failures, paste0(version_message, ": ", paste(details, collapse = "; ")))
  }
  if (length(failures) > 0L) {
    stop(paste(failures, collapse = "; "), call. = FALSE)
  }
  invisible(TRUE)
}

bundled_validation_assert_root_contract <- function(expected, validation_roots, root_versions) {
  missing <- setdiff(validation_roots, expected$Package)
  if (length(missing) > 0L) {
    stop("Bundled validation expected lock omits pinned root(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  locked_versions <- expected$Version[match(validation_roots, expected$Package)]
  requested_versions <- unname(root_versions[validation_roots])
  mismatch <- validation_roots[locked_versions != requested_versions]
  if (length(mismatch) > 0L) {
    details <- paste0(
      mismatch, " expected-lock ", locked_versions[match(mismatch, validation_roots)],
      ", root contract ", requested_versions[match(mismatch, validation_roots)]
    )
    stop("Bundled validation root/expected-lock version mismatch: ", paste(details, collapse = "; "), call. = FALSE)
  }
  invisible(TRUE)
}

bundled_validation_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  repo_root <- normalizePath(
    bundled_validation_arg_value(args, "repo-root", getwd()),
    winslash = "/", mustWork = TRUE
  )
  runtime_root <- normalizePath(
    bundled_validation_arg_value(
      args, "runtime-root",
      file.path(repo_root, "packaging", "electron", "runtime", "R-4.5.3")
    ),
    winslash = "/", mustWork = TRUE
  )
  runtime_library <- normalizePath(file.path(runtime_root, "library"), winslash = "/", mustWork = TRUE)

  source(file.path(repo_root, "R", "app_bootstrap.R"), local = TRUE)
  validation_roots <- names(bundled_validation_packages)
  expected_contract <- bundled_validation_read_expected_contract(repo_root)
  bundled_validation_assert_root_contract(
    expected_contract, validation_roots, bundled_validation_packages
  )

  runtime_database <- installed.packages(lib.loc = runtime_library, noCache = TRUE)
  actual_contract <- bundled_validation_closure_records(
    runtime_database,
    validation_roots,
    incomplete_message = "Bundled runtime validation dependency closure is incomplete"
  )
  bundled_validation_assert_no_base_or_recommended(expected_contract, runtime_database)
  bundled_validation_assert_exact_contract(
    expected_contract,
    actual_contract,
    missing_message = "Bundled runtime validation dependency closure is incomplete",
    unexpected_message = "Bundled runtime validation dependency closure has unexpected package(s)",
    version_message = "Bundled runtime validation dependency version mismatch"
  )

  for (package in validation_roots) {
    namespace_path <- loadNamespace(package, lib.loc = runtime_library)
    if (is.null(namespace_path)) {
      stop("Failed to load bundled validation namespace: ", package, call. = FALSE)
    }
  }

  package_paths <- file.path(runtime_library, actual_contract$Package)
  byte_size <- sum(vapply(package_paths, function(path) {
    files <- list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
    sum(file.info(files)$size, na.rm = TRUE)
  }, numeric(1)))

  cat(
    "Bundled validation package contract passed: ",
    paste(paste0(validation_roots, " ", bundled_validation_packages), collapse = ", "),
    "; recursive non-base closure=", nrow(actual_contract),
    "; expected-lock-sha256=", attr(expected_contract, "sha256"),
    "; size=", format(round(byte_size / 1024^2, 2), nsmall = 2), " MB.\n",
    sep = ""
  )
  invisible(TRUE)
}

if (sys.nframe() == 0L) bundled_validation_main()
