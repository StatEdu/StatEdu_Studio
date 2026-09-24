Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
d<-data.frame(Review=1:12,Normality=c(2,4,3,7,5,9,6,10,12,8,13,15),third=c(4,3,6,5,9,7,12,9,13,15,11,16))
r<-prepare_paired_rm_results(d,variables=names(d),variable_info=data.frame(name=names(d),measurement='continuous'),options=list(assumption_check=FALSE))
# Simulate a restored historical result; this does not test a new GG model fit.
r$table$Method<-'RM ANOVA + Greenhouse-Geisser correction'
r$table$`GG epsilon`<-'0.750';r$table$`GG p`<-'.123'
r$display_table<-paired_rm_display_table(r)
out<-'tmp/rm-gg-restored-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
saveRDS(r,file.path(out,'restored-fixture.rds'));r<-readRDS(file.path(out,'restored-fixture.rds'))
before<-serialize(r,NULL);captured<-list();missing<-character()
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 options(statedu.app_language=lang)
 label<-paired_appendix_text('RM ANOVA + GG',lang);long<-paired_appendix_text(r$table$Method,lang)
 if(lang!='en'&&(label=='RM ANOVA + GG'||long==r$table$Method))missing<-c(missing,lang)
 html<-as.character(paired_rm_results_ui(r));doc<-xml2::read_html(html)
 stopifnot(label%in%xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//td")))
 stopifnot(paired_appendix_table(data.frame(Method=r$table$Method))[[1]][1]==long)
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"));stopifnot(length(main)>0)
 if(lang=='en')baseline<-main else stopifnot(identical(main,baseline))
 stopifnot(identical(before,serialize(r,NULL)),r$table$`GG epsilon`=='0.750',r$table$`GG p`=='.123')
 captured[[lang]]<-html
}
if(length(missing))stop(paste('Untranslated restored GG names:',paste(missing,collapse=', ')))
jsonlite::write_json(captured,file.path(out,'captured.json'),auto_unbox=TRUE)
cat('PASS restored GG fixture names in eight languages; main tables and stored results preserved (no GG fitting claim)\n')
