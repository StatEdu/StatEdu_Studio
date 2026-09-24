Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_render <- function(x) {
 if(!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-')) && grepl('"_result_fit_bootstrap"',paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]][[2]])
 for(item in as.list(x)[-1]){found<-find_render(item);if(!is.null(found))return(found)}
 NULL
}
expr<-find_render(body(structural_canvas_register_fit_diagnostic_outputs));stopifnot(!is.null(expr))
out<-'tmp/pls-effect-bootstrap-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
states<-c('Off','Adequate','Pending','Failed','Canceled','Insufficient','Not recorded','External state')
for(state in states)for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 table<-data.frame(Effect=c('Specific indirect','Total indirect','Total'),Path=c('Review -> 사용자 <&> %s','Normality -> Primary','Items -> Scoring'),beta=c('.123','.234','.345'),check.names=FALSE)
 full<-state=='Adequate'
 for(col in c('Boot SE','Boot 95% CI lower','Boot 95% CI upper','t','p','BH-adjusted p'))table[[col]]<-if(full)c('.111','.222','.333')else ''
 bundle<-list(pls_bootstrap=if(state=='Off')0L else 100L,pls_bootstrap_result=list(nboot=70L,requested_nboot=if(state=='Off')0L else 100L,timeout_failures=1L,estimation_failures=2L,nonconvergence_failures=3L,inadmissible_failures=4L,invalid_statistic_failures=5L,execution_failures=6L,canceled_failures=7L,retained_nonpositive_definite_plsc_draws=if(full)8L else 0L,minimum_valid_ratio=.8,bootstrap_status=state,inference_available=full,failure_message=if(state=='Failed')'Review 상세 <&> %s' else ''))
 env<-new.env(parent=globalenv());env$appendix_result_table<-function(kind)table;env$fit_result<-function()bundle;env$app_language_fn<-function()language
 render<-function()NULL;body(render)<-expr;environment(render)<-env
 html<-as.character(render());doc<-xml2::read_html(html,encoding='UTF-8')
 cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
 stopifnot(all(table$Path%in%cells),all(table$beta%in%cells),length(xml2::xml_find_all(doc,'//table'))==3L)
 headings<-xml2::xml_text(xml2::xml_find_all(doc,'//h5'));notes<-xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 if(language=='en'){en_headings<-headings;en_notes<-notes}else stopifnot(all(headings!=en_headings),all(notes!=en_notes))
 stopifnot(length(notes)==if(state=='Off')1L else if(full)3L else 2L)
 if(state!='Off')stopifnot(grepl('70/100',notes[[1]],fixed=TRUE))
 if(state=='Failed')stopifnot(grepl('Review 상세 <&> %s',tail(notes,1),fixed=TRUE))
 if(state=='Insufficient')stopifnot(grepl('80%',tail(notes,1),fixed=TRUE))
 if(state=='External state')stopifnot(all(grepl(state,notes,fixed=TRUE)))
 warnings<-xml2::xml_find_all(doc,"//p[contains(@class,'structural-result-warning')]");stopifnot(length(warnings)==as.integer(!state%in%c('Off','Adequate')))
 if(!full)stopifnot(length(xml2::xml_find_all(doc,'//th'))==6L)
 if(full && language!='en')stopifnot(!'BH-adjusted p'%in%xml2::xml_text(xml2::xml_find_all(doc,'//th')))
 if(language=='ja')entries[[state]]<-list(id=gsub(' ','',state),title=state,html=html)
 cat('PASS:',state,language,'sections, status notes, warning class, raw failure details, user paths and numbers\n')
}
env$appendix_result_table<-function(kind)data.frame();stopifnot(is.null(render()))
env$appendix_result_table<-function(kind)transform(table,Effect='Direct');stopifnot(is.null(render()))
saveRDS(unname(entries),file.path(out,'entries.rds'))
