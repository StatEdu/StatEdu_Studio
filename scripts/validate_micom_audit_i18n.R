Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_audit<-function(x){
 if(missing(x)||!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name('configural_audit')))return(x[[3]])
 for(item in as.list(x)[-1]){r<-find_audit(item);if(!is.null(r))return(r)}
 NULL
}
expressions<-parse('R/setup_custom_model_canvas_structural_invariance_evaluation.R',encoding='UTF-8')
audit_expr<-NULL;for(x in expressions){audit_expr<-find_audit(x);if(!is.null(audit_expr))break};stopifnot(!is.null(audit_expr))
producer_env<-new.env(parent=globalenv());for(key in c('indicators_present','model_specification_valid','indicators_numeric','finite_or_missing','group_indicator_available'))producer_env[[key]]<-TRUE
producer_env$estimator<-'PLS';producer_env$constructs<-c('Review','Normality','사용자 <&> %s');producer_env$indicators<-paste0('x',1:8)
audit<-eval(audit_expr,producer_env)
options(statedu.output_decimal_digits=3L)
out<-'tmp/micom-audit-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(kind in c('pass','mixed','unknown','empty'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 table<-audit
 if(kind=='mixed')table$Passed<-c(TRUE,FALSE,NA)
 if(kind=='unknown'){table$Criterion[1]<-'Review';table$Evidence[1]<-'Normality 사용자 <&> %s'}
 if(kind=='empty')table<-table[FALSE,]
 bundle<-list(invariance_result=list(type='pls_micom',configural_audit=table))
 ui<-structural_canvas_invariance_appendix_ui(bundle,language)
 if(kind=='empty'){stopifnot(is.null(ui));next}
 html<-as.character(ui);doc<-xml2::read_html(html,encoding='UTF-8');cells<-lapply(1:3,function(j)trimws(xml2::xml_text(xml2::xml_find_all(doc,paste0('//tbody/tr/td[',j,']')))))
 heading<-xml2::xml_text(xml2::xml_find_first(doc,'//h5'));headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
 if(language=='en'){en_heading<-heading;en_cells<-cells}else{
  stopifnot(heading!=en_heading,!any(c('Criterion','Passed','Evidence')%in%headers))
  known<-if(kind=='unknown')2:3 else 1:3
  stopifnot(all(cells[[1]][known]!=table$Criterion[known]),all(cells[[3]][known]!=table$Evidence[known]))
  stopifnot(!any(c('Yes','Not available')%in%cells[[2]]))
 }
 if(kind!='unknown')stopifnot(grepl('3',cells[[3]][1],fixed=TRUE),grepl('8',cells[[3]][1],fixed=TRUE))
 if(kind=='unknown')stopifnot(cells[[1]][1]=='Review',cells[[3]][1]=='Normality 사용자 <&> %s')
 stopifnot(grepl('seminr::mean_replacement',cells[[3]][2],fixed=TRUE),grepl("analysis_type='plssem', estimator='PLS'",cells[[3]][3],fixed=TRUE))
 expected<-ifelse(is.na(table$Passed),statedu_localized_text(language,'Not available','산출 불가'),ifelse(table$Passed,statedu_localized_text(language,'Yes','예'),statedu_localized_text(language,'No','아니요')))
 stopifnot(identical(cells[[2]],expected))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'MICOM audit reasons, counts, logical status and raw user text\n')
}
# Logical values in the other shared appendix branches retain the same contract.
for(type in c('structural_path_comparison','measurement_invariance'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 bundle<-list(invariance_result=list(type=type,group_diagnostics=data.frame(Group=c('Review','Normality','사용자 <&> %s'),'Inference available'=c(TRUE,FALSE,NA),N=c(17L,23L,41L),check.names=FALSE)))
 html<-as.character(structural_canvas_invariance_appendix_ui(bundle,language));doc<-xml2::read_html(html,encoding='UTF-8')
 stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td[1]'))),bundle$invariance_result$group_diagnostics$Group))
 stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td[3]'))),c('17','23','41')))
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
