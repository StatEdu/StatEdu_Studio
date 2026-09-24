Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
# Read the production prefix inventory, including later indexed additions.
scope<-new.env(parent=baseenv())
for(node in as.list(body(survival_stability_evidence_text))) {
 if(!is.call(node)||!identical(node[[1]],as.name('<-')))next
 lhs<-node[[2]]
 if(identical(lhs,as.name('sparse_prefixes')) ||
    (is.call(lhs)&&identical(lhs[[1]],as.name('[['))&&identical(lhs[[2]],as.name('sparse_prefixes'))))eval(node,scope)
}
prefixes<-scope$sparse_prefixes;stopifnot(length(prefixes)>30)
suffixes<-c(' Review / Normality / None / 사용자 <&> %s: events = 99',' 1.20e+05; 0,25; 3/7')
records<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(code in names(prefixes))for(suffix in suffixes) {
 raw<-paste0(prefixes[[code]],suffix)
 actual<-survival_stability_evidence_text(code,raw,lang)
 stopifnot(endsWith(actual,suffix))
 if(lang=='en')stopifnot(identical(actual,raw))else stopifnot(actual!=raw)
 table<-data.frame(Level='review',Code=code,Evidence=actual,Guidance='',stringsAsFactors=FALSE)
 doc<-xml2::read_html(as.character(survival_simple_table(table,table_language=lang)))
 cell<-xml2::xml_text(xml2::xml_find_first(doc,'//tbody/tr/td[3]'))
 stopifnot(identical(cell,actual))
 records[[length(records)+1L]]<-data.frame(language=lang,code=code,source=raw,display=cell)
}
out<-'tmp/survival-evidence-prefix-catalog';dir.create(out,recursive=TRUE,showWarnings=FALSE)
write.csv(do.call(rbind,records),file.path(out,'evidence.csv'),row.names=FALSE,fileEncoding='UTF-8')
cat('PASS',length(prefixes),'dynamic diagnostic prefixes x 8 languages x 2 suffixes; rendered names and numbers preserved\n')
