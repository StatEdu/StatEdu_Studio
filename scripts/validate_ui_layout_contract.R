script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_ui_layout_contract.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

read_project_file <- function(path) {
  paste(readLines(file.path(repo_root, path), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

assert_contains <- function(text, pattern, label) {
  if (!grepl(pattern, text, fixed = TRUE)) {
    stop(sprintf("UI layout contract missing: %s", label), call. = FALSE)
  }
}

assert_not_contains <- function(text, pattern, label) {
  if (grepl(pattern, text, fixed = TRUE)) {
    stop(sprintf("UI layout contract should not contain: %s", label), call. = FALSE)
  }
}

css <- read_project_file("www/style.css")
data_editor_ui <- read_project_file("R/data_editor_ui.R")
wide_long_ui <- read_project_file("R/data_editor_wide_long.R")
rename_ui <- read_project_file("R/data_editor_rename.R")
recode_ui <- read_project_file("R/data_editor_recode.R")
layout_doc <- read_project_file("docs/UI_LAYOUT_CONTRACT.md")

message("Checking shared Data Editor geometry contract...")
assert_contains(css, "--se-standard-setup-width: 1176px;", "standard setup width")
assert_contains(css, "--se-standard-panel-height: 520px;", "standard panel height")
assert_contains(css, "--se-standard-options-width: 310px;", "standard options width")
assert_contains(css, "--se-standard-options-button-width: 286px;", "standard options footer button width")
assert_contains(css, ".data-editor-workspace .analysis-workspace-heading", "workspace heading width rule")
assert_contains(css, ".data-editor-workspace .recode-same-setup-grid:not(.recode-builder-grid)", "standard setup grid rule")
assert_contains(css, ".data-editor-workspace .recode-same-action-row:not(.recode-builder-action-row)", "standard action row rule")
assert_contains(css, "grid-template-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) 20px var(--se-standard-options-width) !important;", "standard three-block grid columns")

message("Checking Wide to Long menu contract...")
assert_contains(data_editor_ui, 'lazy_tab_panel("Wide to Long", "data_editor_wide_long", "lazy_data_editor_wide_long")', "Data Editor menu label")
assert_contains(wide_long_ui, 'h1("Wide to Long")', "Wide to Long page title")
assert_contains(wide_long_ui, 'analysis_workspace_heading("Wide to long", "wide_long")', "Wide to Long workspace heading")
assert_not_contains(wide_long_ui, "Wide to Long Format", "old Wide to Long Format label")
assert_contains(wide_long_ui, 'class = "analysis-action-row recode-same-action-row wide-long-action-row"', "Wide to Long action row")
assert_contains(wide_long_ui, 'actionButton("run_wide_long", "Run"', "Wide to Long run button")
assert_contains(wide_long_ui, 'actionButton("wide_long_remove_spec", "Remove"', "Wide to Long remove button")
assert_contains(wide_long_ui, 'actionButton("preview_wide_long", "Preview"', "Wide to Long preview button")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #run_wide_long", "Wide to Long run placement")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #wide_long_remove_spec", "Wide to Long remove placement")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #preview_wide_long", "Wide to Long preview placement")
assert_contains(css, ".data-editor-workspace .wide-long-set-button", "Wide to Long set button placement")

message("Checking Rename Variable menu contract...")
assert_contains(rename_ui, 'class = "analysis-action-row recode-same-action-row variable-rename-action-row"', "Rename action row")
assert_contains(rename_ui, 'actionButton("run_variable_rename", "Run"', "Rename run button")
assert_contains(rename_ui, 'actionButton("remove_variable_rename", "Remove"', "Rename remove button")
assert_contains(rename_ui, 'class = "recode-same-setup-grid variable-rename-grid"', "Rename standard grid")
assert_contains(css, ".data-editor-workspace .variable-rename-target-panel .analysis-transfer-listbox", "Rename target list height")
assert_contains(css, ".data-editor-workspace .variable-rename-action-row > #remove_variable_rename", "Rename remove placement")

message("Checking Recode Variable menu contract...")
assert_contains(recode_ui, 'class = "recode-same-setup-grid recode-builder-grid"', "Recode builder grid")
assert_contains(recode_ui, 'class = "analysis-action-row recode-same-action-row recode-builder-action-row"', "Recode action row")
assert_contains(recode_ui, 'actionButton("apply_recode_same", "Apply"', "Recode apply button")
assert_contains(recode_ui, 'uiOutput("recode_same_reset_control")', "Recode reset control")
assert_contains(css, ".data-editor-workspace .recode-builder-action-row > #apply_recode_same", "Recode apply placement")
assert_contains(css, ".data-editor-workspace .recode-builder-action-row > #recode_same_reset_control", "Recode reset placement")

message("Checking layout documentation...")
assert_contains(layout_doc, "t-test / ANOVA baseline width of `1176px`", "documented standard width")
assert_contains(layout_doc, "Longitudinal / Panel Models: four-block analysis structure", "documented four-block exception")
assert_contains(layout_doc, "scripts/validate_stabilization.ps1", "documented stabilization validation command")

cat("UI layout contract validation passed.\n")
