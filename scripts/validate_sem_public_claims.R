claims_read_text <- function(path) {
  if (!file.exists(path)) stop("Required public-claims file was not found: ", path, call. = FALSE)
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

claims_require <- function(condition, message) {
  if (!isTRUE(condition)) stop(message, call. = FALSE)
}

claims_require_phrase <- function(text, phrase, label) {
  claims_require(grepl(phrase, text, fixed = TRUE), paste0("Public notes must retain the ", label, " boundary."))
}

version <- trimws(readLines("VERSION", warn = FALSE, n = 1L))
notes <- claims_read_text("docs/RELEASE_1_2_3_PUBLIC_NOTES_DRAFT.md")

claims_require_phrase(notes, "Fit references are descriptive aids rather than universal acceptance rules.", "non-universal fit")
claims_require_phrase(notes, "estimated structural-model fit is not provided.", "saturated-versus-estimated fit")
claims_require_phrase(notes, "do not establish unidimensionality or equivalence to a bifactor model.", "higher-order/bifactor")
claims_require_phrase(notes, "do not by themselves establish causal effects.", "cross-sectional causality")
claims_require_phrase(notes, "only if the final external benchmark gate passes with recorded software versions and settings.", "external-program equivalence")

forbidden_claims <- c(
  "establishes causal effects", "proves causality", "causally identifies",
  "guarantees good fit", "universally acceptable fit", "universal fit cutoff",
  "establishes unidimensionality", "equivalent to a bifactor model",
  "equivalent to SmartPLS", "identical to SmartPLS", "equivalent to ADANCO", "identical to ADANCO",
  "SmartPLS/ADANCO equivalence confirmed"
)
claims_find_forbidden <- function(text) {
  lower_text <- tolower(text)
  forbidden_claims[vapply(forbidden_claims, function(value) grepl(tolower(value), lower_text, fixed = TRUE), logical(1))]
}
present_forbidden <- claims_find_forbidden(notes)
claims_require(!length(present_forbidden), paste0("Public notes contain unsupported claim(s): ", paste(present_forbidden, collapse = ", "), "."))

claims_require(grepl("Bias-corrected bootstrap defaults", notes, fixed = TRUE), "Public notes must identify the bias-corrected bootstrap default.")
claims_require(grepl("valid-replicate reporting", notes, fixed = TRUE), "Public notes must disclose valid bootstrap replicate reporting.")
claims_require(grepl("explicit correction scope", notes, fixed = TRUE), "Public notes must disclose PLSc correction scope.")
claims_require(grepl("Audit manifests", notes, fixed = TRUE), "Public notes must retain reproducibility-record scope.")

if (identical(version, "1.2.3-dev")) {
  claims_require(grepl("Status: draft; not approved for publication", notes, fixed = TRUE), "Development public notes must remain an unapproved draft.")
  required_placeholders <- c(
    "Release date: pending", "Installer SHA-256: pending", "Blockmap SHA-256: pending",
    "External PLS evidence: pending", "Final packaged manual QA: pending", "Publication approval: pending"
  )
  missing_placeholders <- required_placeholders[!vapply(required_placeholders, function(value) grepl(value, notes, fixed = TRUE), logical(1))]
  claims_require(!length(missing_placeholders), paste0("Development public notes are missing pending placeholder(s): ", paste(missing_placeholders, collapse = ", "), "."))
  synthetic_forbidden <- claims_find_forbidden(paste(notes, "This release establishes causal effects and guarantees good fit."))
  claims_require(setequal(synthetic_forbidden, c("establishes causal effects", "guarantees good fit")), "Public-claims validator did not detect the synthetic overclaim fixture.")
} else if (identical(version, "1.2.3")) {
  claims_require(!grepl("pending", notes, ignore.case = TRUE), "Final 1.2.3 public notes must contain no pending placeholders.")
  claims_require(!grepl("not approved for publication", notes, fixed = TRUE), "Final 1.2.3 public notes must record approved publication status.")
}

cat("SEM public-claims validation passed for ", version, ".\n", sep = "")
