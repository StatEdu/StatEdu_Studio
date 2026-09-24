source('R/app_bootstrap.R', encoding = 'UTF-8')
load_app_packages(check = FALSE)
source_app_modules()
options(statedu.app_language = 'ko')
out <- 'tmp/compact-publication'
dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(130)
n <- 90
d <- data.frame(group = factor(rep(c('종괴 있는 근성 사경', '종괴 없는 근성 사경', '자세성 사경'), each = 30)),
                category = factor(rep(c('주 1회', '주 2회 이상', '없음(병원에서만 물리치료 수행 포함)'), 30)),
                x = rnorm(n), pre = rnorm(n, 3), post = rnorm(n, 4), post2 = rnorm(n, 4.5))
d$post <- d$post + d$x * .5
info <- data.frame(name = names(d), measurement = c('category','category',rep('continuous',4)),
 var_label = c('GCM(M)_Occupation 긴 변수명', 'GCI_가정물치(3분류)', '공변량 점수', '양육스트레스 평균 사전', '양육스트레스 평균 사후', '양육스트레스 평균 추후'))
cross <- prepare_crosstab_results(d, 'group', 'category', info, options = list(row_percent = TRUE))
paired <- prepare_paired_results(d, 'pre', 'post', info, options = list(mean_sd = TRUE, effect_size = TRUE))
rm3 <- prepare_paired_rm_results(d, variables = c('pre','post','post2'), variable_info = info, options = list(mean_sd = TRUE))
binary <- as.data.frame(lapply(d[c('pre','post','post2')], function(x) ifelse(x > median(x), 1, 0)))
binary_info <- data.frame(name = names(binary), measurement = 'binary')
rm3binary <- prepare_paired_rm_results(binary, variables = names(binary), variable_info = binary_info)
ancova <- prepare_ancova_results(d, 'post', 'group', 'x', info,
 options = list(mean_se = TRUE, ordered_significance = TRUE, post_hoc = TRUE, normality_enabled = FALSE, plot_raw_overlay = TRUE))
mixed <- prepare_mixed_rm_anova_results(d, 'group', c('pre','post','post2'), covariates = 'x', variable_info = info,
 options = list(assumption_check = TRUE, posthoc = TRUE, time_labels = c('pre','post1','post2')))
publication <- mixed_rm_publication_tables(mixed)
stopifnot(!any(c('df1','df2','Correction','Mauchly W') %in% names(publication$main)), 'F(df1,df2)' %in% names(publication$main))
stopifnot(is.data.frame(publication$diagnostics), nrow(publication$diagnostics) > 0)
stopifnot(all(result_format_df(c('1.00','120.00','1.34')) == c('1','120','1.34')))
stopifnot(!any(grepl('공변량|^x', head(publication$main$Effect,3))))
gg <- mixed
time_row <- which(gg$anova$Effect == 'Time')
gg$anova$Correction[time_row] <- 'Greenhouse-Geisser'
gg$anova$p_sphericity[time_row] <- '<.001'; gg$anova[['epsilon(GG)']][time_row] <- '.67'
gg$anova$df1[time_row] <- '1.34'; gg$anova$df2[time_row] <- '154.15'
gg_view <- mixed_rm_publication_tables(gg)
stopifnot(grepl("Greenhouse-Geisser (Mauchly W's p <.001, GG epsilon = .67 < .75)",gg_view$note,fixed=TRUE))
stopifnot(grepl('1.34, 154.15',gg_view$main[['F(df1,df2)']][2],fixed=TRUE))
stopifnot(isTRUE(paired_setup_state(character(0))$mean_sd), isTRUE(ancova_setup_state(character(0))$mean_se),
 isTRUE(ancova_setup_state(character(0))$ordered_significance), isTRUE(ttest_anova_setup_state(character(0))$ordered_significance))
contents <- list(cross = crosstab_results_ui(cross), paired = paired_results_ui(paired),
 rm3 = paired_rm_results_ui(rm3), rm3binary = paired_rm_results_ui(rm3binary), ancova = ancova_results_ui(ancova, info), mixed = mixed_rm_anova_results_ui(mixed))
entries <- lapply(names(contents), function(name) {
 html <- saved_results_document(paste('StatEdu Studio',name), tags$div(class = 'regression-results', contents[[name]]))
 writeLines(html, file.path(out,paste0(name,'.html')),useBytes = TRUE)
 list(title=name,html=html)
})
saveRDS(list(cross=cross,paired=paired,rm3=rm3,ancova=ancova,mixed=mixed),file.path(out,'fixtures.rds'))
for (entry in entries) {
  tables <- result_entry_tables(entry)
  stopifnot(length(tables)>0)
}
if (!identical(Sys.getenv('STATEDU_LAYOUT_HTML_ONLY'), 'true')) {
  save_result_collection_excel_file(entries, file.path(out,'results.xlsx'))
  write_result_collection_docx(entries, file.path(out,'results.docx'))
  write_result_collection_pdf(entries, file.path(out,'results.pdf'))
}
png(file.path(out,'ancova-legend.png'),width=900,height=650,bg='white')
draw_ancova_raw_overlay_plot(ancova$results[[1]])
dev.off()
cat('PASS: compact layouts, defaults, RM publication/diagnostic separation and df formatting.',
 if (identical(Sys.getenv('STATEDU_LAYOUT_HTML_ONLY'), 'true')) 'HTML preview generated.\n' else 'HTML/PDF/Word/Excel generated.\n')
