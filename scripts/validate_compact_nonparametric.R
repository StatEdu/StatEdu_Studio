source('R/app_bootstrap.R',encoding='UTF-8'); load_app_packages(check=FALSE); source_app_modules()
set.seed(42)
d <- data.frame(y=c(1:15,10:24,30:44),g=rep(letters[1:3],each=15),pre=rnorm(45),post=rnorm(45,2),post2=rnorm(45,3))
info <- data.frame(name=names(d),measurement=c('continuous','category',rep('continuous',3)))
n <- prepare_ttest_anova_results(d,'y','g',info,options=list(force_nonparametric=TRUE,post_hoc=TRUE,add_mean_sd=TRUE))
stopifnot('Median(Q1~Q3)' %in% names(n$results[[1]]$table),'M ± SD' %in% names(n$results[[1]]$mean_sd_extra$table))
stopifnot(identical(n$results[[1]]$table$p,n$results[[1]]$mean_sd_extra$table$p))
p <- prepare_nonparametric_paired_unified_results(d,list(c('pre','post'),c('pre','post','post2')),info,options=list(add_mean_sd=TRUE,effect_size=TRUE))
h <- as.character(nonparametric_paired_results_ui(p))
stopifnot(identical(p$paired$table$p,p$mean_sd_extra$paired$table$p),identical(p$paired_rm$display_table$p,p$mean_sd_extra$paired_rm$display_table$p))
stopifnot(grepl('Median(Q1~Q3)',h,fixed=TRUE),grepl('M ± SD',h,fixed=TRUE))
stopifnot(grepl(paste0(p$paired$scale_table$Pre_M[[1]], ' (', p$paired$scale_table$Pre_SD[[1]], ')'),h,fixed=TRUE))
cor_data <- as.data.frame(matrix(rnorm(500),50,10)); names(cor_data) <- paste0('v',1:10)
cor_info <- data.frame(name=names(cor_data),measurement='continuous')
for (count in c(4,9,10)) {
 cor <- prepare_correlation_results(cor_data,names(cor_data)[1:count],cor_info,options=list(p_ci=TRUE))
 cor_html <- as.character(correlation_results_ui(cor))
 expected <- if(count<=9) 'portrait' else 'landscape'
 doc <- xml2::read_html(cor_html)
 main <- xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
 stopifnot(length(main)>0, all(xml2::xml_attr(main,'data-result-table-orientation')==expected))
 if(count==9) writeLines(paste0('<html><head><meta charset="utf-8"><link rel="stylesheet" href="../../www/style.css"></head><body>',cor_html,'</body></html>'),'tmp/compact-publication/correlation9.html',useBytes=TRUE)
}
stopifnot(isTRUE(nonparametric_setup_state(character())$median_iqr),isTRUE(nonparametric_paired_setup_state(character())$median_iqr))
saved <- as.character(saved_nonparametric_paired_results_html(p))
export_tables <- result_entry_tables(list(title='Nonparametric paired',html=saved,saved_at='2026-09-12'))
stopifnot(length(export_tables)>0,grepl('Median(Q1~Q3)',saved,fixed=TRUE),grepl('M ± SD',saved,fixed=TRUE))
out <- 'tmp/compact-publication'
dir.create(out,recursive=TRUE,showWarnings=FALSE)
for (name in c('nonparametric','nonparametric-paired')) {
 content <- if(name=='nonparametric') as.character(ttest_anova_results_ui(n)) else h
 writeLines(paste0('<html><head><meta charset="utf-8"><link rel="stylesheet" href="../../www/style.css"></head><body><div class="regression-results">',content,'</div></body></html>'),file.path(out,paste0(name,'.html')),useBytes=TRUE)
}
cat('PASS: median defaults, combined cells, separate mean-SD tables.\n')
