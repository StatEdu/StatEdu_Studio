Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(929);d<-data.frame(y=ordered(rep(1:3,each=60)),x=rnorm(180))
info<-data.frame(name=c('y','x'),measurement=c('ordered','continuous'))
detail<-'검증 (Review) 50% <&>'
# Inject only the nominal-effects test failure; fit the retained ordinal model normally.
original<-logistic_parallel_odds_test
logistic_parallel_odds_test<-function(...)stop(detail,call.=FALSE)
results<-tryCatch(prepare_logistic_analysis_results(d,'y','x',variable_info=info),finally={logistic_parallel_odds_test<-original})
stopifnot(results[[1]]$method=='Ordinal logistic regression',is.na(results[[1]]$parallel$p),!isTRUE(results[[1]]$parallel$available))
summary<-'Proportional odds not assessable'
template<-'The proportional-odds nominal-effects test was unavailable (%s); the cumulative logit model was retained and this assumption requires external review.'
raw<-sprintf(template,detail);stopifnot(raw%in%logistic_result_notes(results[[1]]))
before<-serialize(results,NULL);captured<-list();failures<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 expected<-c(logistic_appendix_text(summary,lang),logistic_appendix_text(raw,lang))
 if(lang!='en'&&any(expected==c(summary,raw)))failures<-c(failures,paste(lang,'translation'))
 stopifnot(grepl(detail,expected[2],fixed=TRUE))
 html<-as.character(htmltools::renderTags(logistic_results_panel(results,info))$html);doc<-xml2::read_html(html)
 cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"))
 stopifnot(all(vapply(expected,function(p)any(grepl(p,cells,fixed=TRUE)),logical(1))))
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main)>0)
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 probe<-logistic_appendix_table(data.frame(Variable=c(summary,raw),Message=c(summary,raw)),lang);stopifnot(identical(probe[[1]],c(summary,raw)))
 captured[[lang]]<-html
}
if(length(failures))stop(paste('Unavailable odds failures:',paste(failures,collapse=', ')))
out<-'tmp/logistic-unavailable-odds-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS injected nominal-test failure with real retained ordinal fit in eight languages; error detail/main/source preserved\n')
