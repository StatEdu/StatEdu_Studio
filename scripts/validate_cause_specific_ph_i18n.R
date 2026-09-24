Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cause-specific-ph-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='both',rate_times=c(100,250,500))
custom<-result;custom$cause_specific$ph_table<-result$cause_specific$ph_table[rep(1,4),,drop=FALSE]
custom$cause_specific$ph_table$Term<-c('Review','Normality','None','사용자 <&> %s');custom$cause_specific$ph_table$p<-c(.001,.05,.7,NA_real_)
title<-'Cause-specific proportional hazards review'
note<-'Schoenfeld results and residual plots are review signals, not an automatic proportional-hazards pass/fail decision.'
for(kind in c('actual','custom'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);r<-if(kind=='actual')result else custom
 doc<-xml2::read_html(as.character(survival_competing_results_panel(r,language=language)),encoding='UTF-8')
 headings<-xml2::xml_find_all(doc,'//h4');wanted<-survival_appendix_title(title,language)
 h<-headings[xml2::xml_text(headings)==wanted];stopifnot(length(h)==1)
 html<-as.character(h[[1]]);n<-xml2::xml_find_first(h[[1]],'following-sibling::*[1]')
 # Changed table/title/note snapshot; unchanged plot widget excluded.
 while(!inherits(n,'xml_missing') && xml2::xml_name(n)!='h4'){
  if(!grepl('shiny-plot-output',xml2::xml_attr(n,'class') %||% '',fixed=TRUE))html<-paste0(html,as.character(n))
  n<-xml2::xml_find_first(n,'following-sibling::*[1]')
 }
 part<-xml2::read_html(html,encoding='UTF-8')
 if(language!='en')stopifnot(wanted!=title,!grepl(note,xml2::xml_text(part),fixed=TRUE))
 values<-trimws(xml2::xml_text(xml2::xml_find_all(part,'//tbody/tr/td[position()>1]')))
 if(language=='en')baseline<-values else stopifnot(identical(values,baseline))
 stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(part,'//tbody/tr/td[1]'))),r$cause_specific$ph_table$Term))
 main<-as.character(survival_simple_table(survival_cause_specific_coef_table(r),table_role='main',table_language='en'))
 if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'actual panel, PH values and labels, localized title/note, English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
