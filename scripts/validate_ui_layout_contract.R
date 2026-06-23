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

count_fixed <- function(text, pattern) {
  matches <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (identical(matches, -1L)) 0L else length(matches)
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

assert_count_at_least <- function(text, pattern, minimum, label) {
  actual <- count_fixed(text, pattern)
  if (actual < minimum) {
    stop(sprintf(
      "UI layout contract missing: %s; expected at least %s, found %s",
      label,
      minimum,
      actual
    ), call. = FALSE)
  }
}

css <- read_project_file("www/style.css")
analysis_data_viewer_ui <- read_project_file("R/analysis_data_viewer.R")
setup_ui <- read_project_file("R/setup_ui.R")
data_editor_ui <- read_project_file("R/data_editor_ui.R")
wide_long_ui <- read_project_file("R/data_editor_wide_long.R")
rename_ui <- read_project_file("R/data_editor_rename.R")
recode_ui <- read_project_file("R/data_editor_recode.R")
missing_ui <- read_project_file("R/data_editor_missing.R")
easyflow_js <- read_project_file("www/easyflow.js")
layout_doc <- read_project_file("docs/UI_LAYOUT_CONTRACT.md")
r_text <- paste(vapply(list.files(file.path(repo_root, "R"), pattern = "\\.R$", full.names = TRUE), function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}, character(1)), collapse = "\n")

message("Checking shared Data Editor geometry contract...")
assert_contains(css, "--se-standard-setup-width: 1176px;", "standard setup width")
assert_contains(css, "--se-standard-panel-height: 520px;", "standard panel height")
assert_contains(css, "--se-standard-options-width: 310px;", "standard options width")
assert_contains(css, "--se-standard-options-button-width: 286px;", "standard options footer button width")
assert_not_contains(css, ".data-editor-workspace {\n  --se-standard", "duplicate Data Editor standard variable declarations")
assert_not_contains(css, "var(--se-standard-setup-width, 1140px)", "stale standard setup width fallback")
assert_contains(css, ".recode-same-setup-grid {\n  display: grid;\n  grid-template-columns: var(--se-standard-panel-width, 326px)", "base Data Editor setup grid uses shared variables")
assert_contains(css, ".recode-same-action-row {\n  display: grid;\n  grid-template-columns: var(--se-standard-panel-width, 326px)", "base Data Editor action row uses shared variables")
assert_contains(css, ".recode-builder-action-row {\n  grid-template-columns: var(--se-standard-panel-width, 326px)", "base Data Editor builder action row uses shared variables")
assert_contains(css, ".recode-same-action-row > .btn {\n  grid-column: 1;\n  justify-self: start;\n  margin-left: 30px;\n  width: var(--se-standard-inner-button-width, 300px);", "base Data Editor action buttons use shared width")
assert_contains(css, ".recode-builder-action-row > .btn {\n  grid-column: 1;\n  justify-self: start;\n  margin-left: 30px;\n  width: var(--se-standard-inner-button-width, 300px);", "base Data Editor builder action buttons use shared width")
assert_contains(css, ".missing-values-action-cell {\n  grid-column: 1;\n  justify-self: start;\n  display: flex;\n  flex-direction: column;\n  gap: 8px;\n  width: var(--se-standard-inner-button-width, 300px);", "Missing Values action cell uses shared width")
assert_count_at_least(wide_long_ui, "data-editor-workspace", 1L, "Wide to Long workspace wrapper")
assert_count_at_least(rename_ui, "data-editor-workspace", 1L, "Rename Variable workspace wrapper")
assert_count_at_least(recode_ui, "data-editor-workspace", 4L, "Recode tools workspace wrappers")
assert_count_at_least(missing_ui, "data-editor-workspace", 1L, "Missing Values workspace wrapper")
assert_contains(css, ".data-editor-workspace .analysis-workspace-heading", "workspace heading width rule")
assert_contains(css, ".data-editor-workspace .recode-same-setup-grid:not(.recode-builder-grid)", "standard setup grid rule")
assert_contains(css, ".data-editor-workspace .recode-same-action-row:not(.recode-builder-action-row)", "standard action row rule")
assert_contains(css, "grid-template-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) 20px var(--se-standard-options-width) !important;", "standard three-block grid columns")

message("Checking shared selected-data viewer button contract...")
assert_contains(analysis_data_viewer_ui, 'analysis_data_viewer_button <- function(id)', "selected-data viewer button helper")
assert_contains(analysis_data_viewer_ui, 'actionButton(id, "View selected data", class = "btn btn-default analysis-data-viewer-button")', "selected-data viewer button label")
assert_contains(analysis_data_viewer_ui, "analysis_workspace_heading <- function(title, prefix)", "workspace heading helper")
assert_contains(analysis_data_viewer_ui, 'analysis_data_viewer_button(paste0(prefix, "_view_data"))', "workspace heading uses shared viewer button")
if (count_fixed(r_text, '"View selected data"') != 1L) {
  stop("UI layout contract missing: View selected data must be created only by analysis_data_viewer_button()", call. = FALSE)
}
assert_contains(css, ".analysis-workspace-heading", "workspace heading CSS")
assert_contains(css, "justify-content: space-between;", "workspace heading right-edge alignment")
assert_contains(css, ".analysis-data-viewer-button", "selected-data viewer button CSS")

message("Checking shared analysis menu geometry contract...")
assert_contains(setup_ui, 'analysis_workspace_heading("t-test / ANOVA", "ttest_anova")', "t-test / ANOVA baseline heading")
assert_contains(setup_ui, 'class = "analysis-action-row ttest-anova-action-row"', "t-test / ANOVA baseline action row")
assert_contains(css, "body {\n  --se-standard-panel-width: 326px;", "global standard layout variables")
assert_contains(css, ".ttest-anova-setup-grid,", "t-test / ANOVA setup grid shares standard analysis geometry")
assert_contains(css, ".ttest-anova-action-row,", "t-test / ANOVA action row shares standard analysis geometry")
assert_contains(css, ".hierarchical-setup-grid,\n.regression-setup-grid {\n  grid-template-columns: var(--se-standard-panel-width, 326px)", "shared analysis setup grid uses standard variables")
assert_contains(css, ".hierarchical-action-row,\n.regression-action-row {\n  grid-template-columns: var(--se-standard-panel-width, 326px)", "shared analysis action row uses standard variables")
assert_contains(css, ".reliability-action-row > .btn,\n.frequencies-action-row > .btn,", "shared analysis action button selector")
assert_contains(css, "width: var(--se-standard-inner-button-width, 300px);\n  min-width: var(--se-standard-inner-button-width, 300px);", "shared analysis action buttons use standard width")
assert_contains(css, "width: 1140px !important;", "standard analysis setup/action width")
assert_contains(css, ".analysis-workspace-heading,\n.analysis-data-viewer-panel", "standard analysis heading width selector")
assert_contains(css, ".longitudinal-action-row {\n  grid-template-columns: 330px 40px 320px 40px 320px 330px !important;", "Longitudinal four-block action row exception")
assert_contains(css, ".longitudinal-setup-grid {\n  display: grid;\n  grid-template-areas: \"available panel-move panel model-move model options\";", "Longitudinal four-block setup exception")

message("Checking Wide to Long menu contract...")
assert_contains(data_editor_ui, 'lazy_tab_panel("Wide to Long", "data_editor_wide_long", "lazy_data_editor_wide_long")', "Data Editor menu label")
assert_contains(wide_long_ui, 'h1("Wide to Long")', "Wide to Long page title")
assert_contains(wide_long_ui, 'analysis_workspace_heading("Wide to long", "wide_long")', "Wide to Long workspace heading")
assert_not_contains(wide_long_ui, "Wide to Long Format", "old Wide to Long Format label")
assert_not_contains(css, ".wide_long-workspace-heading", "Wide to Long-specific workspace heading override")
assert_not_contains(css, ".recode-same-action-row.wide-long-action-row {\n  display: grid;", "legacy unscoped Wide to Long action-row override")
assert_contains(wide_long_ui, 'class = "analysis-action-row recode-same-action-row wide-long-action-row"', "Wide to Long action row")
assert_contains(wide_long_ui, 'actionButton("run_wide_long", "Run"', "Wide to Long run button")
assert_contains(wide_long_ui, 'actionButton("wide_long_remove_spec", "Remove"', "Wide to Long remove button")
assert_contains(wide_long_ui, 'actionButton("preview_wide_long", "Preview"', "Wide to Long preview button")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #run_wide_long", "Wide to Long run placement")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #wide_long_remove_spec", "Wide to Long remove placement")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #preview_wide_long", "Wide to Long preview placement")
assert_contains(css, ".data-editor-workspace .wide-long-set-button", "Wide to Long set button placement")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #run_wide_long {\n  grid-column: 1 !important;\n}", "Wide to Long run button column")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #wide_long_remove_spec {\n  grid-column: 3 !important;\n}", "Wide to Long remove button column")
assert_contains(css, ".data-editor-workspace .wide-long-action-row > #preview_wide_long {\n  grid-column: 5 !important;\n}", "Wide to Long preview button column")
assert_contains(css, ".data-editor-workspace .wide-long-set-button {\n  margin-top: auto !important;\n  margin-bottom: 28px !important;", "Wide to Long set button vertical anchor")
assert_contains(css, "width: var(--se-standard-options-button-width) !important;\n  min-width: var(--se-standard-options-button-width) !important;\n  max-width: var(--se-standard-options-button-width) !important;", "Wide to Long set/preview standard button width")
assert_contains(css, ".data-editor-workspace .wide-long-action-row {\n  grid-template-columns: var(--se-standard-panel-width) var(--se-standard-transfer-width) var(--se-standard-panel-width) 20px var(--se-standard-options-width) !important;", "Wide to Long action row standard columns")

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

message("Checking Auto Coding Error menu contract...")
assert_contains(recode_ui, 'analysis_workspace_heading("Auto coding error check", "coding_error")', "Coding error workspace heading")
assert_contains(recode_ui, 'class = "recode-same-setup-grid recode-different-setup-grid"', "Coding error standard grid")
assert_contains(recode_ui, '"coding_error_move",', "Coding error transfer button")
assert_contains(recode_ui, 'class = "analysis-action-row recode-same-action-row"', "Coding error action row")
assert_contains(recode_ui, 'actionButton("apply_coding_error", "Run"', "Coding error run button")
assert_contains(recode_ui, 'uiOutput("coding_error_reset_control")', "Coding error reset control")

message("Checking Auto Missing Values menu contract...")
assert_contains(missing_ui, 'analysis_workspace_heading("Auto missing value detection", "missing_values")', "Missing values workspace heading")
assert_contains(missing_ui, 'class = "recode-same-setup-grid missing-values-setup-grid"', "Missing values standard grid")
assert_contains(missing_ui, '"missing_values_move",', "Missing values transfer button")
assert_contains(missing_ui, 'class = "analysis-action-row recode-same-action-row missing-values-action-row"', "Missing values action row")
assert_contains(missing_ui, 'actionButton("mark_user_missing_values", "Mark as user missing"', "Missing values primary action")
assert_contains(missing_ui, 'actionButton("convert_missing_values_to_na", "Convert to NA"', "Missing values secondary action")
assert_contains(css, ".data-editor-workspace .recode-same-action-row:not(.recode-builder-action-row) > .btn,", "standard direct action placement")
assert_contains(css, ".data-editor-workspace .recode-same-action-row:not(.recode-builder-action-row) > .missing-values-action-cell", "Missing values action-cell placement")

message("Checking grouped menu navigation contract...")
assert_contains(easyflow_js, "function easyflowGroupedMenuConfigs()", "grouped menu configuration")
assert_contains(easyflow_js, "function markNavbarDropdownActive(link)", "grouped menu top-level active helper")
assert_contains(easyflow_js, "dropdown.closest('.navbar-nav').children('li.active').removeClass('active');", "grouped menu clears stale top-level active state")
assert_contains(easyflow_js, "dropdown.addClass('active');", "grouped menu activates clicked top-level menu")
assert_contains(easyflow_js, "click.easyflowAnalysisSubmenu", "grouped submenu click handler")
assert_contains(easyflow_js, "click.easyflowAnalysisDirectItem", "grouped direct-item click handler")
assert_contains(easyflow_js, "link.closest('.analysis-menu-section').addClass('active open');", "grouped submenu active section marker")
assert_contains(easyflow_js, "link.closest('.navbar-nav > li.dropdown').removeClass('open');", "grouped menu closes after selection")

message("Checking layout documentation...")
assert_contains(layout_doc, "Standard analysis and Data Editor menus use shared geometry variables declared\non `body` in `www/style.css`", "documented global standard geometry variables")
assert_contains(layout_doc, "three-block tools use `1176px`, including setup-grid padding where relevant", "documented standard setup width")
assert_contains(layout_doc, "Every standard Data Editor tool must render inside the\n  `.data-editor-workspace` wrapper", "documented Data Editor workspace wrapper requirement")
assert_contains(layout_doc, "must reference the standard variables instead of hard-coded copies", "documented standard variable requirement")
assert_contains(layout_doc, "`Wide to Long` is a standard Data Editor three-block tool, not a layout\nexception.", "documented Wide to Long standard layout classification")
assert_contains(layout_doc, "`Run` sits under Block 1.", "documented Wide to Long run placement")
assert_contains(layout_doc, "`Remove` sits under Block 2.", "documented Wide to Long remove placement")
assert_contains(layout_doc, "`Preview` sits under Block 3.", "documented Wide to Long preview placement")
assert_contains(layout_doc, "Longitudinal / Panel Models: four-block analysis structure", "documented four-block exception")
assert_contains(layout_doc, "scripts/validate_stabilization.ps1", "documented stabilization validation command")

cat("UI layout contract validation passed.\n")
