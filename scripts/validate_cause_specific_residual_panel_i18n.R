Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cause-specific-residual-panel-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',covariates=c('age','sex'),regression='both',rate_times=c(100,250,500))
titles<-c('Cause-specific residual distribution','Cause-specific continuous-covariate functional-form review','Cause-specific influence review')
notes<-c('The Martingale-residual smoother is a descriptive functional-form diagnostic; it does not select a transformation automatically.','Standardized DFBETAS above 2/sqrt(n) are observations for sensitivity review, not automatic deletion rules.')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 doc<-xml2::read_html(as.character(survival_competing_results_panel(result,language=language)),encoding='UTF-8')
 headings<-xml2::xml_find_all(doc,'//h4')
 wanted<-vapply(titles,survival_appendix_title,character(1),language=language)
 stopifnot(all(wanted%in%xml2::xml_text(headings)))
 if(language!='en')stopifnot(!any(titles%in%xml2::xml_text(headings)),!any(vapply(notes,function(x)grepl(x,xml2::xml_text(doc),fixed=TRUE),logical(1))))
 selected<-headings[xml2::xml_text(headings)%in%wanted]
 # Export the changed diagnostic sections, excluding the unchanged plot widget.
 html<-''
 for(h in selected){
  html<-paste0(html,as.character(h));n<-xml2::xml_find_first(h,'following-sibling::*[1]')
  while(!inherits(n,'xml_missing') && xml2::xml_name(n)!='h4'){
   if(!grepl('shiny-plot-output',xml2::xml_attr(n,'class') %||% '',fixed=TRUE))html<-paste0(html,as.character(n))
   n<-xml2::xml_find_first(n,'following-sibling::*[1]')
  }
 }
 part<-xml2::read_html(html,encoding='UTF-8')
 values<-trimws(xml2::xml_text(xml2::xml_find_all(part,'(//table)[1]//tbody/tr/td[position()>1] | (//table)[2]//tbody/tr/td[position()>1 and not(position()=5)]')))
 if(language=='en')baseline<-values else stopifnot(identical(values,baseline))
 custom<-result$cause_specific;custom$influence_table$Term<-rep(c('Review','사용자 <&> %s'),length.out=nrow(custom$influence_table))
 tab<-xml2::read_html(as.character(survival_simple_table(survival_cox_influence_table(custom),table_language=language)),encoding='UTF-8')
 stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(tab,'//tbody/tr/td[1]'))),custom$influence_table$Term))
 main<-as.character(survival_simple_table(survival_cause_specific_coef_table(result),table_role='main',table_language='en'))
 if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
 if(language=='ja')entries[[1]]<-list(id='cause-specific-residual',title='Cause-specific diagnostics',html=html)
 cat('PASS:',language,'actual panel headings, notes, numerical values, user labels and English main table\n')
}
saveRDS(entries,file.path(out,'entries.rds'))
