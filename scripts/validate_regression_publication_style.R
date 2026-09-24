Sys.setlocale("LC_CTYPE", "English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE = "false")
source("R/app_bootstrap.R", encoding = "UTF-8")
load_app_packages(check = FALSE); source_app_modules()
out <- "tmp/regression-publication-style"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
table <- regression_main_table(data.frame(Term=c("(Intercept)","사회적 지지_전체 평균 × 디지털 헬스 리터러시 평균"), B=c("2.78","-.035"), `HC3 SE`=c(".42",".291"), LLCI=c("2.02","-.808"), ULCI=c("3.66",".394"), `Boot p`=c("<.001",".871"),check.names=FALSE))
render <- function(root) as.character(tags$div(class=root, coefficient_html_table(table, sheet_orientation="portrait", note_line="HC3 SE = robust standard error; LLCI = lower confidence limit; ULCI = upper confidence limit.")))
slopes <- data.frame(Path=rep("사회적 지지_전체 평균→부모유능감(9) 평균",3), `Moderator variable`=rep("디지털 헬스 리터러시_전체 평균_MEAN",3), Level=c("Low (M-SD)","Mean","High (M+SD)"), W=c("3.675","4.272","4.868"), Effect=c(".375",".353",".332"), SE=c(".428",".275",".169"), t=c(".874","1.283","1.970"), p=c(".384",".202",".051"), LLCI=c("-.474","-.192","-.002"), ULCI=c("1.223",".899",".666"),check.names=FALSE)
names(slopes)[names(slopes)=="Moderator variable"] <- "Moderator"
slopes <- mediation_moderation_conditional_table_layout(slopes)
conditional <- as.character(tags$div(class="regression-results mm-results", analysis_result_table_section("Conditional effects",slopes,class="result-section regression-result-panel mm-conditional-effects-section",table_fn=mediation_moderation_conditional_table)))
entries <- list(
 list(id="regression",title="Regression",html=render("regression-results")),
 list(id="mediation",title="Mediation/moderation",html=render("regression-results mm-results mm-combined-portrait-section")),
 list(id="conditional",title="Conditional effects",html=conditional))
models <- readRDS("outputs/spss_phase32_20260906/three_blocks/analysis.rds")
hierarchical <- as.character(htmltools::renderTags(hierarchical_results_panel(models))$html)
stopifnot(grepl("hierarchical-standard-model-title",hierarchical,fixed=TRUE),grepl("mm-combined-sublabel",hierarchical,fixed=TRUE))
saveRDS(entries,file.path(out,"entries.rds"))
write_result_collection_html(entries,file.path(out,"screen.html"))
write_result_collection_html(list(list(id="hierarchical",title="Hierarchical regression",html=hierarchical)),file.path(out,"hierarchical.html"))
# Verify the Results toolbar keeps the Korean-only HWPX button.
for(lang in c("ko","en")) {
 shiny::testServer(function(input,output,session) {
   register_result_accumulator_outputs(input,output,session,function() lang)
 }, {
   session$flushReact()
   controls <- output$result_export_controls$html
   stopifnot(grepl("save_result_collection_hwpx_dialog",controls,fixed=TRUE)==(lang=="ko"))
 })
}
message("PASS: shared hierarchical renderer, conditional labels and Results-only Korean HWPX control")
