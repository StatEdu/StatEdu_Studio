Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/competing-collinearity-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='both',rate_times=c(100,250,500))
for(kind in c('cause_specific','fine_gray'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 fg<-kind=='fine_gray';r<-result[[kind]]
 title<-if(fg)'Fine-Gray design-matrix collinearity review' else 'Cause-specific collinearity review'
 rows<-survival_cox_collinearity_table(r)
 html<-as.character(tagList(tags$h4(survival_appendix_title(title,language)),survival_simple_table(rows,table_language=language),survival_competing_collinearity_note(r,language,fg)))
 doc<-xml2::read_html(html,encoding='UTF-8');values<-xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[position()<4]'))
 if(language=='en')baseline<-values else stopifnot(identical(values,baseline),!grepl('collinearity review|Design-matrix condition number:|thresholds are heuristic',xml2::xml_text(doc)))
 stopifnot(grepl(survival_format_number(r$condition_number),xml2::xml_text(doc),fixed=TRUE))
 for(value in c(1.2345,NA_real_)){
  note<-xml2::read_html(as.character(survival_competing_collinearity_note(list(condition_number=value),language,fg)),encoding='UTF-8')
  stopifnot(grepl(survival_format_number(value),xml2::xml_text(note),fixed=TRUE))
 }
 main<-as.character(survival_simple_table(if(fg)survival_fine_gray_coef_table(result) else survival_cause_specific_coef_table(result),table_role='main',table_language='en'))
 if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'actual fits, collinearity notes, values and English coefficient table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
