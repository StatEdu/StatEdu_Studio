Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-read.csv('scripts/fixtures/survival_validation.csv');d$time[1]<-NA_real_
idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
km<-prepare_km_single_analysis_result(d,'time','status')
competing<-prepare_competing_risk_result(d,'time','status',group='sex',rate_times=c(100,250))
titles<-c('Excluded rows by reason','Data review messages','Estimand contract')
tables<-list(survival_km_exclusion_table(km),survival_km_issue_table(km),competing$estimand_table)
stopifnot(all(vapply(tables,nrow,integer(1))>0))
out<-'tmp/survival-review-titles-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
before<-serialize(list(km,competing),NULL);missing<-list();captured<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 doc<-xml2::read_html(as.character(tagList(survival_km_result_panel(km,language=lang),survival_competing_results_panel(competing,language=lang))))
 headings<-xml2::xml_text(xml2::xml_find_all(doc,'//h3|//h4|//h5'))
 for(title in titles) {
  actual<-survival_appendix_title(title,lang);stopifnot(actual%in%headings)
  if(lang!='en'&&actual==title)missing[[length(missing)+1L]]<-data.frame(language=lang,english=title)
 }
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main)>0)
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 captured[[lang]]<-as.character(tagList(lapply(seq_along(titles),function(i)tagList(tags$h4(survival_appendix_title(titles[i],lang)),survival_simple_table(tables[[i]],table_language=lang)))))
 stopifnot(identical(before,serialize(list(km,competing),NULL)))
}
if(length(missing)) {
 missing<-unique(do.call(rbind,missing));write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated survival review titles',call.=FALSE)
}
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual KM exclusions/review and competing-risk estimand titles in eight languages; English main tables and source preserved\n')
