Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/cif-integrity-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
d<-read.csv('scripts/fixtures/survival_validation.csv');idx<-which(d$status==1);d$status[idx[seq(1,length(idx),by=2)]]<-2
result<-prepare_competing_risk_result(d,'time','status',group='sex',rate_times=c(100,250,500))
tables<-list(actual=result$cif_integrity)
for(kind in c('sum','bounds')){
 curve<-result$curve;curve$CIF<-if(kind=='sum').8 else -.2
 tables[[kind]]<-survival_cif_integrity_table(curve)
 stopifnot(all(!tables[[kind]][['Integrity passed']]))
}
for(kind in names(tables))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 raw<-tables[[kind]];raw$Group<-rep(c('Review','사용자 <&> %s'),length.out=nrow(raw))
 rows<-survival_cif_integrity_display_table(list(cif_integrity=raw),language)
 html<-as.character(tagList(tags$h4(survival_appendix_title('CIF integrity checks',language)),survival_simple_table(rows,table_language=language)))
 doc<-xml2::read_html(html,encoding='UTF-8')
 stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[1]'))),raw$Group))
 values<-xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[position()>=2 and position()<=4]'))
 if(language=='en')baseline<-values else stopifnot(identical(values,baseline),!any(names(raw)[-1]%in%xml2::xml_text(xml2::xml_find_all(doc,'//th'))))
 for(i in 5:8){
  expected<-ifelse(raw[[i]],statedu_localized_text(language,'Yes','예'),statedu_localized_text(language,'No','아니요'))
  stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,paste0('//tbody/tr/td[',i,']')))),expected))
 }
 main<-as.character(survival_simple_table(survival_competing_rate_table(result),table_role='main',table_language='en'))
 if(language=='en')main_baseline<-main else stopifnot(identical(main,main_baseline))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'integrity flags, numeric values, labels and English CIF table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
