Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cox-method-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv')
result<-prepare_cox_analysis_result(d,'time','status',c('age','sex'),event_value='1')
cases<-list(basic=list(),entry=list(entry='entry'),start=list(start='start'),strata=list(strata='Review'),cluster=list(cluster='Normality'),spline=list(spline_covariate='사용자 <&> %s',spline_df=4L),varying=list(time_varying_covariate='사용자 <&> %s'))
for(kind in names(cases))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);r<-modifyList(result,cases[[kind]])
 r$time_origin<-'Origin <&> %s';r$time_unit<-'Unit %d';r$ties<-'Efron'
 sentence<-survival_method_sentence(r,language)
 for(x in c(r$time_origin,r$time_unit,'Efron',if(kind=='spline')'4',unlist(cases[[kind]][intersect(names(cases[[kind]]),c('strata','cluster','spline_covariate','time_varying_covariate'))])))stopifnot(grepl(as.character(x),sentence,fixed=TRUE))
 if(!language%in%c('en','ko'))stopifnot(!grepl('Time was measured from|a Cox proportional hazards model|using the Efron method|modeled using|with the coefficient',sentence))
 blank<-r;blank$time_origin<-'';blank$time_unit<-''
 if(!language%in%c('en','ko'))stopifnot(!grepl('not specified',survival_method_sentence(blank,language),fixed=TRUE))
 html<-as.character(tagList(survival_simple_table(data.frame(Variable='사용자 <&> %s',N=10),table_language=language),tags$p(sentence)))
 parsed<-xml2::read_html(html,encoding='UTF-8');stopifnot(sentence%in%xml2::xml_text(xml2::xml_find_all(parsed,'//p')))
 main<-xml2::xml_text(xml2::xml_find_all(xml2::read_html(as.character(survival_cox_result_html_table(result,language)),encoding='UTF-8'),'//th|//td'))
 if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'method templates, raw metadata and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
