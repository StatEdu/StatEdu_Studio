source('scripts/validate_logistic_complete_case_guidance_i18n.R',encoding='UTF-8')
raw<-unique(unlist(lapply(results,function(r){n<-logistic_result_notes(r);n[startsWith(n,'Binary event for ')]})))
stopifnot(length(raw)==1,raw=='Binary event for y is 2; reference is 1.')
before<-serialize(results,NULL);captured<-list();failures<-character()
special<-'Binary event for 사용자 <&> %s is 사건 (50%); reference is 기준 [0].'
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang);expected<-logistic_appendix_text(raw,lang)
 if(lang!='en'&&expected==raw)failures<-c(failures,lang)
 translated<-logistic_appendix_text(special,lang)
 stopifnot(all(vapply(c('사용자 <&> %s','사건 (50%)','기준 [0]'),function(p)grepl(p,translated,fixed=TRUE),logical(1))))
 if(lang!='en'&&translated==special)failures<-c(failures,paste(lang,'named values'))
 html<-as.character(htmltools::renderTags(logistic_results_panel(results,info))$html);doc<-xml2::read_html(html)
 cells<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td"));stopifnot(any(grepl(expected,cells,fixed=TRUE)))
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main)>0)
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(results,NULL)))
 probe<-logistic_appendix_table(data.frame(Variable=c(raw,special),Message=c(raw,special)),lang);stopifnot(identical(probe[[1]],c(raw,special)))
 captured[[lang]]<-html
}
if(length(failures))stop(paste('Binary event failures:',paste(failures,collapse=', ')))
out<-'tmp/logistic-binary-event-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS actual binary event/reference guidance and named-value probes in eight languages; main/source/user labels preserved\n')
