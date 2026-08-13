scripts <- c(
  "validate_cfa_canvas.R",
  "validate_cfa_bootstrap.R",
  "validate_cfa_reporting_exports.R",
  "validate_cfa_external_references.R"
)

for (script in scripts) {
  message("Running ", script, "...")
  source(file.path("scripts", script), encoding = "UTF-8")
}

cat("All CFA validations passed.\n")
