Sys.setlocale("LC_CTYPE","English_United States.utf8")
Sys.setenv(STATEDU_MODULE_CACHE="false")
source("R/app_bootstrap.R",encoding="UTF-8");load_app_packages(check=FALSE);source_app_modules()
languages<-setdiff(statedu_supported_languages(),c("en","ko"))
rows<-statedu_translation_table()
missing<-list()
for(key in names(rows)) {
 row<-rows[[key]]
 if(!all(c("en","ko") %in% names(row))||anyNA(row[c("en","ko")]))next
 for(lang in languages) if(!lang %in% names(row)||is.na(row[[lang]])||!nzchar(row[[lang]]))missing[[length(missing)+1L]]<-paste(lang,key)
}
stopifnot(!length(missing))
for(lang in languages) {
 options(statedu.app_language=lang)
 stopifnot(!identical(statedu_localized_text(lang,"Apply coding book","코딩북 적용"),"Apply coding book"))
 stopifnot(!identical(paired_appendix_text("Outliers",lang),"Outliers"))
 stopifnot(!identical(paired_appendix_text("Sphericity",lang),"Sphericity"))
 stopifnot(!grepl("Normality|Homogeneity|used",regression_appendix_text("Normality met; Homogeneity not met; HC3 used",lang)))
 template<-statedu_localized_text(lang,"Coding book applied to %s variables.")
 stopifnot(grepl("17",sprintf(template,17),fixed=TRUE))
 stopifnot(grepl("_START_",datatable_language_options(lang)$info,fixed=TRUE))
 stopifnot(identical(unname(statedu_measurement_choices(lang)),c("binary","category","ordered","continuous")))
 source<-data.frame(Item=c("Analysis","Sphericity","Subjects"),Value=c("Covariate-adjusted mixed repeated-measures ANOVA","Not satisfied (W=.40, p<.001); adjusted residuals (group + covariates).","사용자 변수"),N=c(589,589,589))
 localized<-mixed_rm_appendix_table(source)
 stopifnot(!any(grepl("[\uac00-\ud7a3]",localized[[2]][1:2],perl=TRUE)))
 stopifnot(!grepl("Not satisfied|adjusted residuals",localized[[2]][2]))
 stopifnot(identical(localized[[2]][3],source[[2]][3]),identical(localized[[3]],source[[3]]))
 stopifnot(grepl("W=.40, p<.001",localized[[2]][2],fixed=TRUE))
 cat("PASS:",lang,"catalog coverage, UI controls, templates, paired and mixed diagnostics\n")
}
