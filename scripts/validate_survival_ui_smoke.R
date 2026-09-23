run_survival_ui_smoke <- function() {
Sys.setlocale("LC_CTYPE", "English_United States.utf8")
old_cache <- Sys.getenv("STATEDU_MODULE_CACHE", unset=NA_character_)
Sys.setenv(STATEDU_MODULE_CACHE="false")
on.exit(if(is.na(old_cache)) Sys.unsetenv("STATEDU_MODULE_CACHE") else Sys.setenv(STATEDU_MODULE_CACHE=old_cache), add=TRUE)
script_path <- if (length(grep("^--file=", commandArgs(FALSE), value = TRUE)) > 0) {
  sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
} else {
  "scripts/validate_survival_ui_smoke.R"
}
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
setwd(repo_root)

source(file.path(repo_root, "R", "app_bootstrap.R"))
load_app_packages(check = FALSE)
source_app_modules()

fixture <- read.csv(file.path(repo_root, "scripts", "fixtures", "survival_validation.csv"), check.names = FALSE)
fixture$status_competing <- rep(c(0L, 1L, 2L, 1L, 0L, 2L), length.out = nrow(fixture))
fixture$sex <- factor(fixture$sex)
fixture$ph.ecog <- factor(fixture$ph.ecog)
variable_table <- data.frame(
  name = names(fixture),
  measurement = c("id", "continuous", "category", "category", "category", "continuous", "category"),
  stringsAsFactors = FALSE
)

server_under_test <- function(input, output, session) {
  register_survival_handlers(
    input = input, output = output, session = session,
    selected_names_fn = function() names(fixture), dataset_fn = function() fixture,
    variable_table_fn = function() variable_table, labels_fn = function() character(0),
    category_table_fn = function() data.frame(), mark_settings_dirty = function() invisible(TRUE),
    app_language_fn = function() "en"
  )
}

ui_html <- function(value) paste(as.character(value), collapse = "\n")
assert_save_buttons <- function(html, prefix, expect_pdf = FALSE, expect_excel = FALSE) {
  ids <- paste0("save_survival_", prefix, c("_html_dialog", "_pdf_dialog", "_excel_dialog"))
  add_id <- paste0("add_survival_", prefix, "_result")
  figure_id <- paste0("save_survival_", prefix, "_figures_dialog")
  for (id in c(ids[[1]], figure_id, add_id)) {
    pattern <- sprintf("<button[^>]*id=\"%s\"[^>]*>", id)
    location <- regexpr(pattern, html, perl = TRUE)
    if (location[[1]] < 0) stop(sprintf("Missing save control %s in: %s", id, substr(html, 1, 2000)))
    tag <- regmatches(html, location)
    stopifnot(!grepl(" disabled(?:=|\\s|>)", tag, perl = TRUE))
  }
  pdf_pattern <- sprintf("<button[^>]*id=\"%s\"[^>]*>", ids[[2]])
  pdf_location <- regexpr(pdf_pattern, html, perl = TRUE)
  if (isTRUE(expect_pdf)) {
    stopifnot(pdf_location[[1]] > 0)
  } else {
    stopifnot(pdf_location[[1]] < 0)
  }
  excel_pattern <- sprintf("<button[^>]*id=\"%s\"[^>]*>", ids[[3]])
  excel_location <- regexpr(excel_pattern, html, perl = TRUE)
  if (isTRUE(expect_excel)) {
    stopifnot(excel_location[[1]] > 0)
    stopifnot(!grepl(" disabled(?:=|\\s|>)", regmatches(html, excel_location), perl = TRUE))
  } else {
    stopifnot(excel_location[[1]] < 0)
  }
}

old_store <- Sys.getenv("STATEDU_RESULT_STORE", unset = NA_character_)
result_store <- tempfile("survival-ui-result-store-", fileext = ".json")
Sys.setenv(STATEDU_RESULT_STORE = result_store)
on.exit({
  if (is.na(old_store)) Sys.unsetenv("STATEDU_RESULT_STORE") else Sys.setenv(STATEDU_RESULT_STORE = old_store)
  unlink(result_store)
}, add = TRUE)

message("Checking live Shiny survival save controls...")
expect_live_document_exports <- analysis_document_export_available()
notifications <- character(0)
original_show_notification <- showNotification
assign("showNotification", function(ui, ...) {
  notifications <<- c(notifications, paste(as.character(ui), collapse = " "))
  invisible(NULL)
}, envir = .GlobalEnv)
on.exit(assign("showNotification", original_show_notification, envir = .GlobalEnv), add = TRUE)

shiny::testServer(server_under_test, {
  session$setInputs(
    survival_km_time_move = 0L, survival_km_event_move = 0L, survival_km_group_move = 0L,
    survival_cox_time_move = 0L, survival_cox_event_move = 0L, survival_cox_covariates_move = 0L,
    run_survival_km = 0L, run_survival_cox = 0L, run_survival_competing = 0L,
    add_survival_cox_result = 0L
  )
  session$flushReact()
  session$setInputs(survival_km_available = "time", survival_km_time_move = 1L)
  session$flushReact()
  session$setInputs(survival_km_available = "status", survival_km_event_move = 1L)
  session$flushReact()
  session$setInputs(survival_km_available = "sex", survival_km_group_move = 1L)
  session$flushReact()
  session$setInputs(
    survival_km_event_value = "1", survival_km_rate_times = "100, 200, 400",
    survival_km_analysis_method = "km", survival_km_test_method = "logrank",
    survival_km_output_tables = c("survival_table", "survival_time"),
    survival_km_plot_types = "survival", survival_km_plot_versions = "color",
    survival_km_show_ci = TRUE, survival_km_show_censor = TRUE,
    survival_km_data_shape = "single_record", run_survival_km = 1L
  )
  session$flushReact()
  km_control <- ui_html(output$survival_km_save_control)
  if (!nzchar(km_control)) stop(paste("KM result was not created.", paste(notifications, collapse = " | ")))
  assert_save_buttons(km_control, "km", expect_pdf = expect_live_document_exports, expect_excel = expect_live_document_exports)

  session$setInputs(survival_cox_available = "time", survival_cox_time_move = 1L)
  session$flushReact()
  session$setInputs(survival_cox_available = "status", survival_cox_event_move = 1L)
  session$flushReact()
  session$setInputs(survival_cox_available = c("age", "sex"), survival_cox_covariates_move = 1L)
  session$flushReact()
  session$setInputs(
    survival_cox_event_value = "1", survival_cox_data_shape = "single_record",
    survival_cox_ties_method = "efron", run_survival_cox = 1L
  )
  session$flushReact()
  cox_control <- ui_html(output$survival_cox_save_control)
  if (!nzchar(cox_control)) stop(paste("Cox result was not created.", paste(notifications, collapse = " | ")))
  assert_save_buttons(cox_control, "cox", expect_pdf = expect_live_document_exports, expect_excel = expect_live_document_exports)

  session$setInputs(
    survival_competing_time = "time", survival_competing_event = "status_competing",
    survival_competing_censored_value = "0", survival_competing_interest_value = "1",
    survival_competing_event_values = "2", survival_competing_group = "sex",
    survival_competing_rate_times = "100, 200, 400", survival_competing_covariates = "age",
    survival_competing_regression = "both", survival_competing_censoring_group = "",
    run_survival_competing = 1L
  )
  session$flushReact()
  competing_control <- ui_html(output$survival_competing_save_control)
  if (!nzchar(competing_control)) stop(paste("Competing result was not created.", paste(notifications, collapse = " | ")))
  assert_save_buttons(competing_control, "competing", expect_pdf = expect_live_document_exports, expect_excel = expect_live_document_exports)

  session$setInputs(add_survival_cox_result = 1L)
  session$flushReact()
  stopifnot(identical(result_snapshot_store_path(), result_store))
  stopifnot(file.exists(result_store), file.info(result_store)$size > 1000)
  saved_entries <- read_result_snapshot_store(result_store)
  stopifnot(length(saved_entries) == 1L, identical(saved_entries[[1]]$title, "Cox Regression"))
  stopifnot(grepl("StatEdu Studio Cox Regression Results", saved_entries[[1]]$html, fixed = TRUE))
})

message("Checking documented public/development save policy...")
old_public <- Sys.getenv("STATEDU_PUBLIC_RELEASE", unset = NA_character_)
old_edition <- Sys.getenv("STATEDU_EDITION", unset = NA_character_)
old_excel <- Sys.getenv("STATEDU_ENABLE_EXCEL_EXPORT", unset = NA_character_)
on.exit({
  if (is.na(old_public)) Sys.unsetenv("STATEDU_PUBLIC_RELEASE") else Sys.setenv(STATEDU_PUBLIC_RELEASE = old_public)
  if (is.na(old_edition)) Sys.unsetenv("STATEDU_EDITION") else Sys.setenv(STATEDU_EDITION = old_edition)
  if (is.na(old_excel)) Sys.unsetenv("STATEDU_ENABLE_EXCEL_EXPORT") else Sys.setenv(STATEDU_ENABLE_EXCEL_EXPORT = old_excel)
}, add = TRUE)

Sys.setenv(STATEDU_PUBLIC_RELEASE = "0", STATEDU_EDITION = "development", STATEDU_ENABLE_EXCEL_EXPORT = "1")
development_controls <- htmltools::renderTags(analysis_save_buttons(
  html_button_id = "save_survival_km_html_dialog", pdf_button_id = "save_survival_km_pdf_dialog",
  excel_button_id = "save_survival_km_excel_dialog", add_result_button_id = "add_survival_km_result",
  figure_button_id = "save_survival_km_figures_dialog", has_figures = TRUE, language = "en"
))$html
assert_save_buttons(development_controls, "km", expect_pdf = TRUE, expect_excel = TRUE)
development_controls_ko <- htmltools::renderTags(analysis_save_buttons(
  html_button_id = "save_survival_km_html_dialog", pdf_button_id = "save_survival_km_pdf_dialog",
  excel_button_id = "save_survival_km_excel_dialog", add_result_button_id = "add_survival_km_result",
  figure_button_id = "save_survival_km_figures_dialog", has_figures = TRUE, language = "ko"
))$html
stopifnot(
  grepl("HTML 저장", development_controls_ko, fixed = TRUE),
  grepl("PDF 저장", development_controls_ko, fixed = TRUE),
  grepl("Excel 저장", development_controls_ko, fixed = TRUE),
  grepl("결과 추가", development_controls_ko, fixed = TRUE)
)

Sys.setenv(STATEDU_PUBLIC_RELEASE = "1", STATEDU_EDITION = "free")
Sys.unsetenv("STATEDU_ENABLE_EXCEL_EXPORT")
public_controls <- htmltools::renderTags(analysis_save_buttons(
  html_button_id = "save_survival_km_html_dialog", pdf_button_id = "save_survival_km_pdf_dialog",
  excel_button_id = "save_survival_km_excel_dialog", add_result_button_id = "add_survival_km_result",
  figure_button_id = "save_survival_km_figures_dialog", has_figures = TRUE, language = "en"
))$html
assert_save_buttons(public_controls, "km", expect_pdf = analysis_document_export_available(), expect_excel = analysis_document_export_available())

Sys.setenv(STATEDU_PUBLIC_RELEASE = "0", STATEDU_EDITION = "pro", STATEDU_ENABLE_EXCEL_EXPORT = "1")
pro_controls <- htmltools::renderTags(analysis_save_buttons(
  html_button_id = "save_survival_km_html_dialog", pdf_button_id = "save_survival_km_pdf_dialog",
  excel_button_id = "save_survival_km_excel_dialog", add_result_button_id = "add_survival_km_result",
  figure_button_id = "save_survival_km_figures_dialog", has_figures = TRUE, language = "en"
))$html
assert_save_buttons(pro_controls, "km")
stopifnot(
  analysis_figure_dpi("pro", public_release = FALSE) == 600L,
  analysis_figure_dpi("pro", public_release = TRUE) == 600L,
  analysis_figure_dpi("pro", requested_dpi = 600L) == 600L,
  analysis_figure_dpi("free", requested_dpi = 600L) == 300L,
  analysis_figure_dpi("free") == 300L,
  analysis_figure_dpi("development", public_release = FALSE) == 600L,
  analysis_figure_dpi("development", public_release = TRUE) == 600L,
  analysis_figure_dpi("personal") == 300L,
  analysis_figure_dpi("institution") == 300L,
  analysis_figure_dpi("invalid") == 300L
)

message("All survival UI smoke validations passed.")

}
run_survival_ui_smoke()
