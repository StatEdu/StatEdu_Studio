# Run with the isolated R 4.5.3 arm64 that will be bundled.
# R_MAKEVARS_USER can select an extracted Fortran compiler and SDK headers.
args <- commandArgs(trailingOnly = TRUE)
value <- function(name, default = '') {
  matching <- args[startsWith(args, paste0('--', name, '='))]
  if (length(matching)) sub(paste0('--', name, '='), '', matching[[1]], fixed = TRUE) else default
}
stopifnot(Sys.info()[['sysname']] == 'Darwin', grepl('arm64|aarch64', R.version$arch), getRversion() == '4.5.3')
repo <- normalizePath(value('repo', getwd()))
source(file.path(repo, 'R/app_bootstrap.R'))
library <- normalizePath(value('library', file.path(R.home(), 'library')))
if (!startsWith(library, paste0(normalizePath(R.home()), '/'))) stop('Library must be inside the bundled R home.')
.libPaths(library)
cache <- value('cache', file.path(repo, '.macos-work/package-cache'))
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
validation <- read.csv(file.path(repo, 'scripts/bundled_validation_packages.expected.csv'), colClasses = 'character')
lock_path <- file.path(repo, 'packaging/macos/runtime-packages.lock.csv')
input_lock_path <- file.path(repo, 'packaging/macos/runtime-inputs.lock.json')
# Parse the flat archive hash entries with base R: jsonlite is not installed yet
# when provisioning a fresh framework.
archive_hashes <- list()
if (file.exists(input_lock_path)) {
  lines <- readLines(input_lock_path, warn = FALSE)
  entries <- lines[grepl('^ *"[^"]+[.](tgz|tar[.]gz)": *"[a-f0-9]{64}",? *$', lines)]
  names <- sub('^ *"([^"]+)".*$', '\\1', entries)
  hashes <- sub('^.*: *"([a-f0-9]{64})".*$', '\\1', entries)
  archive_hashes <- as.list(setNames(hashes, names))
  if (!length(archive_hashes)) stop('Package archive checksum lock is empty.')
}
verify_archive <- function(file, package) {
  if (!length(archive_hashes)) return(invisible(NULL))
  expected <- archive_hashes[[basename(file)]]
  actual <- strsplit(system2('/usr/bin/shasum', c('-a', '256', shQuote(file)), stdout = TRUE), ' ', fixed = TRUE)[[1]][[1]]
  if (is.null(expected) || !identical(actual, expected)) stop('Package archive checksum mismatch or not pinned: ', package)
}
options(timeout = 300, Ncpus = 4)
if (file.exists(lock_path)) {
  desired <- read.csv(lock_path, colClasses = 'character')
} else {
  # Bootstrap dependency closure once; the resulting complete lock must be retained.
  install.packages(unique(c(required_packages, validation$Package)), lib = library,
    repos = 'https://mac.r-project.org', type = 'binary')
  desired <- validation
}
desired <- desired[!duplicated(desired$Package), , drop = FALSE]
force_source <- unique(c('fs', strsplit(value('source'), ',', fixed = TRUE)[[1]]))
force_source <- force_source[nzchar(force_source)]
installed_version <- function(package) {
  description <- file.path(library, package, 'DESCRIPTION')
  if (!file.exists(description)) '' else read.dcf(description)[1, 'Version']
}
needed <- desired$Package[vapply(seq_len(nrow(desired)), function(i) installed_version(desired$Package[[i]]) != desired$Version[[i]], logical(1))]
needed <- union(needed, intersect(force_source, desired$Package))
metadata <- list(); files <- list(); source_files <- character()
for (package in needed) {
  version <- desired$Version[match(package, desired$Package)]
  # Prefer the exact binary. Never silently substitute a newer version.
  binary <- file.path(cache, paste0(package, '_', version, '.tgz'))
  url <- paste0('https://mac.r-project.org/bin/macosx/big-sur-arm64/contrib/4.5/', basename(binary))
  success <- !package %in% force_source && (file.exists(binary) || tryCatch({download.file(url, binary, quiet = TRUE); TRUE}, error = function(e) FALSE))
  if (success) {
    verify_archive(binary, package)
    temporary <- tempfile(); dir.create(temporary); untar(binary, exdir = temporary)
    metadata[[package]] <- read.dcf(file.path(temporary, package, 'DESCRIPTION'))
    unlink(temporary, recursive = TRUE)
    files[[package]] <- binary
  } else {
    unlink(binary)
    file <- file.path(cache, paste0(package, '_', version, '.tar.gz'))
    urls <- c(paste0('https://cran.r-project.org/src/contrib/Archive/', package, '/', basename(file)),
              paste0('https://cran.r-project.org/src/contrib/', basename(file)))
    success <- file.exists(file)
    for (url in urls) {
      if (success) break
      success <- tryCatch({download.file(url, file, quiet = TRUE); TRUE}, error = function(e) FALSE)
      if (!success) unlink(file)
    }
    if (!success) stop('Exact package source unavailable: ', package, ' ', version)
    verify_archive(file, package)
    temporary <- tempfile(); dir.create(temporary); untar(file, exdir = temporary)
    metadata[[package]] <- read.dcf(file.path(temporary, package, 'DESCRIPTION'))
    unlink(temporary, recursive = TRUE)
    files[[package]] <- file
    source_files <- c(source_files, package)
  }
  if (metadata[[package]][1, 'Version'] != version) stop('Downloaded version mismatch: ', package)

}
dependencies <- lapply(metadata, function(d) {
  fields <- intersect(colnames(d), c('Depends', 'Imports', 'LinkingTo'))
  unique(trimws(gsub(' *[(].*?[)]', '', unlist(strsplit(paste(d[1, fields], collapse = ','), ',')))))
})
while (length(needed)) {
  ready <- needed[vapply(needed, function(p) !any(dependencies[[p]] %in% needed), logical(1))]
  if (!length(ready)) stop('Package dependency cycle: ', paste(needed, collapse = ', '))
  for (package in ready) {
    install.packages(files[[package]], repos = NULL, lib = library,
      type = if (package %in% source_files) 'source' else 'binary', INSTALL_opts = '--no-multiarch')
    if (installed_version(package) != metadata[[package]][1, 'Version']) stop('Package installation failed: ', package)
  }
  needed <- setdiff(needed, ready)
}
for (i in seq_len(nrow(validation))) {
  if (installed_version(validation$Package[[i]]) != validation$Version[[i]]) stop('Validation package pin mismatch: ', validation$Package[[i]])
}
for (package in required_packages) loadNamespace(package)
database <- installed.packages(lib.loc = library, noCache = TRUE)
lock <- data.frame(Package = database[, 'Package'], Version = database[, 'Version'], row.names = NULL)
lock <- lock[order(lock$Package), ]
write.csv(lock, lock_path, row.names = FALSE)
cat('PASS: exact validation pins, required namespaces, complete package lock saved\n')
