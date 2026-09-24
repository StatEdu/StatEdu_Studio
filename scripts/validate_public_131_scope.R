if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_ALL", "English_United States.utf8"))
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()
Sys.setenv(STATEDU_PUBLIC_RELEASE = "1", STATEDU_EDITION = "free")
stopifnot(identical(read_app_config()$version, "1.3.1"))
tabs <- enabled_analysis_tabs()
stopifnot(!tabs[["meta"]], !tabs[["one_group_rm_anova"]],
  tabs[["mixed_rm_anova"]], tabs[["paired"]], tabs[["paired_rm"]],
  analysis_document_export_available())
for (language in statedu_supported_languages()) {
  options(statedu.app_language = language)
  ui <- as.character(app_ui("1.3.1"))
  stopifnot(!grepl('data-value="analysis_meta"', ui, fixed = TRUE),
    !grepl('data-value="One-group repeated-measures ANOVA"', ui, fixed = TRUE))
}
Sys.setenv(STATEDU_PUBLIC_RELEASE = "0", STATEDU_EDITION = "development")
tabs <- enabled_analysis_tabs()
stopifnot(tabs[["meta"]], tabs[["one_group_rm_anova"]],
  identical(read_app_config()$version, "1.3.1-dev"))
cat("PASS: public 1.3.1 exclusion in all eight languages; other public analyses and developer menus preserved\n")
