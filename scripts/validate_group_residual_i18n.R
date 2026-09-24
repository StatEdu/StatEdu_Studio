Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.output_decimal_digits=3L)
out<-'tmp/group-residual-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
fit<-lavaan::cfa('visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9',data=lavaan::HolzingerSwineford1939,group='school')
original<-structural_canvas_residual_diagnostics(fit)
stopifnot(nrow(original$group_summary)==2L,nrow(original$group_largest)>0L)
for(kind in c('actual','fallback','custom','empty'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 residuals<-original
 if(kind%in%c('fallback','custom')){
  for(name in c('group_summary','group_largest')){
   value<-residuals[[name]]
   for(column in intersect(c('Group','Indicator1','Indicator2'),names(value)))value[[column]]<-rep(c('Review','Normality','사용자 <&> %s'),length.out=nrow(value))
   residuals[[name]]<-value
  }
  residuals$group_largest[['Residual scale']]<-if(kind=='fallback')'Correlation residual fallback' else 'Review'
  residuals$group_largest[['Exceeds descriptive cutoff']]<-rep(c(TRUE,FALSE,NA),length.out=nrow(residuals$group_largest))
 }
 if(kind=='empty')residuals<-list()
 bundle<-list(invariance_result=list(type='measurement_invariance',group_residuals=residuals))
 ui<-structural_canvas_invariance_appendix_ui(bundle,language)
 if(kind=='empty'){stopifnot(is.null(ui));next}
 html<-as.character(ui);doc<-xml2::read_html(html,encoding='UTF-8');tables<-xml2::xml_find_all(doc,'//table');stopifnot(length(tables)==2L)
 for(i in 1:2){
  source<-residuals[[c('group_summary','group_largest')[i]]]
  headers<-xml2::xml_text(xml2::xml_find_all(tables[[i]],'.//th'))
  if(language!='en')stopifnot(!any(c('Max |standardized residual|','Flagged residuals','Indicator1','Indicator2','Residual scale')%in%headers))
  for(j in seq_along(source)){
   cells<-trimws(xml2::xml_text(xml2::xml_find_all(tables[[i]],paste0('./tbody/tr/td[',j,']'))))
   name<-names(source)[j];raw<-source[[j]]
   if(name%in%c('Group','Indicator1','Indicator2'))stopifnot(identical(cells,raw))
   if(name=='Flagged residuals')stopifnot(identical(cells,as.character(raw)))
   if(is.numeric(raw)){
    key<-paste(kind,i,j,sep='_')
    if(language=='en')assign(key,cells)else stopifnot(identical(cells,get(key)))
   }
   if(is.logical(raw))stopifnot(identical(cells,ifelse(is.na(raw),statedu_localized_text(language,'Not available','산출 불가'),ifelse(raw,statedu_localized_text(language,'Yes','예'),statedu_localized_text(language,'No','아니요')))))
   if(name=='Residual scale'){
    if(kind=='custom'||language=='en')stopifnot(identical(cells,raw))else stopifnot(all(cells!=raw))
   }
  }
 }
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'residual headers, scales, exact identifiers and numbers\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
