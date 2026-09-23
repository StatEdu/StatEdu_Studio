if (.Platform$OS.type == "windows") Sys.setlocale("LC_CTYPE", "Korean_Korea.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE)
source_app_modules()

Sys.setenv(STATEDU_PUBLIC_RELEASE = "1", STATEDU_EDITION = "free")
stopifnot(read_app_config()$version == "1.3.0")
for (format in c("pdf", "word", "excel")) {
  stopifnot(analysis_save_feature_visible(format), analysis_save_feature_enabled(format),
    statedu_feature_enabled(paste0(format, "_export")))
}
stopifnot(analysis_figure_dpi() == 300L)
tabs <- enabled_analysis_tabs()
stopifnot(!tabs[["meta"]], !tabs[["one_group_rm_anova"]], tabs[["mixed_rm_anova"]], tabs[["paired_rm"]])
# Hidden public features must not register their execution handlers either.
stopifnot(is.null(register_meta_server()), is.null(register_one_group_rm_anova_handlers()))
for (language in c("ko", "en", "ja", "zh", "es", "fr", "de", "vi")) {
  panel <- analysis_tab_panel(language = language)
  html <- as.character(htmltools::tagList(panel$tabs))
  stopifnot(!grepl('lazy_analysis_meta', html, fixed = TRUE),
    !grepl('lazy_analysis_one_group_rm_anova', html, fixed = TRUE),
    grepl('lazy_analysis_mixed_rm_anova', html, fixed = TRUE))
}
# Verify that the same policy still blocks document exports on historical 1.2.x.
original_config <- read_app_config
read_app_config <- function(...) list(version = "1.2.7")
stopifnot(!analysis_document_export_available())
read_app_config <- original_config
Sys.setenv(STATEDU_PUBLIC_RELEASE = "0", STATEDU_EDITION = "development")
stopifnot(enabled_analysis_tabs()[["meta"]], enabled_analysis_tabs()[["one_group_rm_anova"]],
  analysis_document_export_available(), analysis_figure_dpi() == 600L)
cat("PASS: 1.3.0 public export policy, Free DPI, eight-language menu exclusions, development retention and 1.2.x compatibility.\n")
