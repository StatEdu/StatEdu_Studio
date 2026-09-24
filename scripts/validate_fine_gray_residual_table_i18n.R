Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/fine-gray-residual-table-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='fine_gray',rate_times=c(100,250,500))
stopifnot(nrow(result$fine_gray$residual_review)>0)
lines<-readLines('R/result_survival_ui.R',encoding='UTF-8')
note<-sub('",?\\s*$','',sub('^\\s*"','',lines[grepl('^\\s*"The crr Schoenfeld-like residual plot',lines)]))
stopifnot(length(note)==1)
for(kind in c('actual','custom'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 r<-result
 if(kind=='custom'){
  r$fine_gray$residual_review<-r$fine_gray$residual_review[rep(1,2),,drop=FALSE]
  r$fine_gray$residual_review$Term<-c('Review','사용자 <&> %s')
  r$fine_gray$residual_review[['Review signal']]<-c(TRUE,FALSE)
 }
 rows<-survival_fine_gray_residual_table(r)
 html<-as.character(tagList(tags$h4(survival_appendix_title('Proportional subdistribution hazards review',language)),survival_simple_table(rows,table_language=language),survival_appendix_note(language,note,'crr Schoenfeld 유사 잔차도는 주된 기술적 부적합 검토입니다. Spearman 상관과 Holm 보정 p값은 탐색적 단조 추세 선별이며 공식 cox.zph 대응 검정이나 자동 가정 합격·불합격 판정이 아닙니다.')))
 doc<-xml2::read_html(html,encoding='UTF-8');values<-xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[position()<6]'))
 if(language=='en')baseline<-values else{
  stopifnot(identical(values,baseline),!any(names(rows)[c(2,4,5,6)]%in%xml2::xml_text(xml2::xml_find_all(doc,'//th'))),!grepl('Proportional subdistribution hazards review|The crr Schoenfeld-like residual plot|Review possible time-varying subdistribution effect|No monotonic time-pattern signal',xml2::xml_text(doc)))
 }
 stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[1]'))),rows$Term))
 main<-as.character(survival_simple_table(survival_fine_gray_coef_table(result),table_role='main',table_language='en'))
 if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'actual residuals, statuses, labels, precision and English coefficient table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
