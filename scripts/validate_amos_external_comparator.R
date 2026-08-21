source(file.path("scripts", "generate_amos_external_benchmark.R"), encoding = "UTF-8")
source(file.path("scripts", "compare_amos_external.R"), encoding = "UTF-8")

validation_dir <- tempfile("statedu-amos-benchmark-")
dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)
on.exit(unlink(validation_dir, recursive = TRUE, force = TRUE), add = TRUE)

benchmark <- generate_amos_external_benchmark(validation_dir)
stopifnot(
  file.exists(benchmark$statedu_path), file.exists(benchmark$template_path),
  file.exists(benchmark$sav_path), file.exists(benchmark$program_path),
  file.exists(benchmark$manifest_path), nrow(benchmark$results) == 30L,
  benchmark$results$Value[benchmark$results$Item == "Degrees of freedom"] == 24,
  any(grepl("Sem.BeginGroup", readLines(benchmark$program_path, warn = FALSE), fixed = TRUE))
)

amos_exact <- file.path(validation_dir, "amos_exact.csv")
file.copy(benchmark$statedu_path, amos_exact, overwrite = TRUE)
exact <- compare_amos_external(
  benchmark$statedu_path, amos_exact,
  absolute_tolerance = .0005, relative_tolerance = 1e-6
)
stopifnot(nrow(exact) == 30L, all(exact$Status == "PASS"), max(exact$AbsoluteError) == 0)

amos_rounded <- utils::read.csv(benchmark$statedu_path, check.names = FALSE, stringsAsFactors = FALSE)
amos_rounded$Value <- round(amos_rounded$Value, 3L)
amos_rounded_path <- file.path(validation_dir, "amos_rounded.csv")
utils::write.csv(amos_rounded, amos_rounded_path, row.names = FALSE, na = "")
rounded <- compare_amos_external(
  benchmark$statedu_path, amos_rounded_path,
  absolute_tolerance = .0005, relative_tolerance = 1e-6
)
stopifnot(all(rounded$Status == "PASS"))

amos_bad <- amos_rounded
amos_bad$Value[[1L]] <- amos_bad$Value[[1L]] + .01
amos_bad_path <- file.path(validation_dir, "amos_bad.csv")
utils::write.csv(amos_bad, amos_bad_path, row.names = FALSE, na = "")
bad <- compare_amos_external(
  benchmark$statedu_path, amos_bad_path,
  absolute_tolerance = .0005, relative_tolerance = 1e-6
)
stopifnot(any(bad$Status == "FAIL"))

template <- utils::read.csv(benchmark$template_path, check.names = FALSE, stringsAsFactors = FALSE)
stopifnot(nrow(template) == 30L, all(is.na(template$Value)))

cat("AMOS external CFA benchmark and comparator validation passed.\n")
