all_args <- commandArgs(FALSE)
file_arg <- all_args[grep("^--file=", all_args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "scripts/validate_calculators.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = FALSE)
if (!file.exists(file.path(repo_root, "R", "utils.R"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source(file.path(repo_root, "R", "utils.R"))
source(file.path(repo_root, "R", "calculator_metabolic.R"))
source(file.path(repo_root, "R", "calculator_hint8.R"))
source(file.path(repo_root, "R", "calculator_eq5d.R"))
source(file.path(repo_root, "R", "calculator_frs.R"))
source(file.path(repo_root, "R", "calculator_ascvd10.R"))
source(file.path(repo_root, "R", "calculator_metabolic_severity.R"))

expect_close <- function(actual, expected, tolerance = 1e-6, label = "") {
  if (!isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance, check.attributes = FALSE))) {
    stop(sprintf("%s expected %s, got %s", label, paste(expected, collapse = ", "), paste(actual, collapse = ", ")), call. = FALSE)
  }
}

expect_identical_values <- function(actual, expected, label = "") {
  if (!identical(unname(actual), unname(expected))) {
    stop(sprintf("%s expected %s, got %s", label, paste(expected, collapse = ", "), paste(actual, collapse = ", ")), call. = FALSE)
  }
}

message("Checking HINT8...")
hint8_items <- data.frame(
  lq1 = c(1, 2),
  lq2 = c(1, 3),
  lq3 = c(1, 4),
  lq4 = c(1, 1),
  lq5 = c(1, 2),
  lq6 = c(1, 3),
  lq7 = c(1, 4),
  lq8 = c(1, 1)
)
expect_close(hint8_score(hint8_items), c(1, 0.563), label = "HINT8 score")

message("Checking EQ-5D...")
eq5d_items <- data.frame(
  eq1 = c(1, 2),
  eq2 = c(1, 3),
  eq3 = c(1, 4),
  eq4 = c(1, 5),
  eq5 = c(1, 1)
)
expect_close(eq5d_score(eq5d_items, type = "5L", value_set = "KR"), c(1, 0.423), label = "EQ-5D score")

message("Checking metabolic syndrome...")
metabolic_data <- data.frame(
  sex = c(1, 2),
  wc = c(95, 81),
  glu = c(99, 100),
  DMd = c(0, 0),
  SBP = c(120, 130),
  DBP = c(80, 80),
  HPd = c(0, 0),
  HDLc = c(45, 49),
  TG = c(151, 149)
)
metabolic_out <- metabolic_result(
  metabolic_data,
  stats::setNames(names(metabolic_data), names(metabolic_data)),
  metabolic_default_references()
)
expect_identical_values(metabolic_out$metabolic_count, c(2, 4), "Metabolic count")
expect_identical_values(metabolic_out$metabolic_syndrome, c(0, 1), "Metabolic syndrome")

message("Checking FRS...")
frs_data <- data.frame(
  sex = 1,
  age = 55,
  Smok = 1,
  HDLc = 50,
  chol = 200,
  HPd = 0,
  SBP = 135,
  DM = 0
)
frs_out <- frs_result(frs_data, stats::setNames(names(frs_data), names(frs_data)))
expect_close(frs_out$frs_score, 16, label = "FRS score")
expect_close(frs_out$frs_cvd10, 25.3, label = "FRS 10-year risk")
expect_close(frs_out$frs_cvd10_group, 3, label = "FRS risk group")
expect_close(frs_out$frs_heart_age, 76, label = "FRS heart age")

message("Checking ASCVD10...")
ascvd10_data <- data.frame(
  race = c(1, 2, 3),
  sex = c(1, 2, 1),
  age = c(55, 60, 50),
  SMOK = c(1, 0, 0),
  CHOL = c(213, 240, 200),
  HDLc = c(50, 55, 50),
  HPd = c(0, 1, 0),
  SBP = c(120, 140, 120),
  DM = c(0, 1, 0),
  c_history = c(0, 0, 1),
  c_ldlc = c(100, 120, 100)
)
ascvd10_out <- ascvd10_result(ascvd10_data, stats::setNames(names(ascvd10_data), names(ascvd10_data)))
expect_close(ascvd10_out$ascvd10_score[1:2], c(10.00086, 23.01622), tolerance = 1e-5, label = "ASCVD10 score")
if (!is.na(ascvd10_out$ascvd10_score[[3]])) {
  stop("ASCVD10 history exclusion expected NA", call. = FALSE)
}

message("Checking metabolic severity...")
mbss_data <- data.frame(
  sex = c(1, 2),
  age = c(35, 50),
  glu = c(100, 100),
  SBP = c(120, 120),
  wc = c(90, 80),
  HDLc = c(50, 50),
  TG = c(150, 150)
)
mbss_out <- metabolic_severity_result(mbss_data, stats::setNames(names(mbss_data), names(mbss_data)))
expect_close(mbss_out$MBSS_overall, c(0.4974157, 0.9701299), tolerance = 1e-6, label = "MBSS overall")
expect_close(mbss_out$MBSS, c(0.6571234, 0.7581409), tolerance = 1e-6, label = "MBSS")

message("All calculator validations passed.")
