Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.output_decimal_digits=3L)
out<-'tmp/group-reliability-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
fit<-lavaan::cfa('visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9',data=lavaan::HolzingerSwineford1939,group='school')
reliability<-structural_canvas_group_reliability_estimates(fit)
htmt<-structural_canvas_group_htmt(fit);stopifnot(nrow(reliability)==6L,nrow(htmt)==6L)
reasons<-c('At least two indicators per factor are required','Cross-loaded indicators prevent standard HTMT calculation','Indicator correlations are unavailable','Within-factor correlations are insufficient','','사용자 <&> %s')
for(kind in c('actual','diagnostics','model_implied','empty'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 rel<-if(kind=='model_implied')structural_canvas_group_reliability_estimates(fit,'model_implied')else reliability
 pairs<-htmt
 if(kind=='diagnostics'){
  rel$Group<-rel$Factor<-pairs$Group<-pairs$Factor1<-pairs$Factor2<-rep(c('Review','Normality','사용자 <&> %s'),2)
  pairs$Reason<-reasons;pairs$Criterion<-c('Below reference','Review needed','Not assessed','Review','Normality','사용자 <&> %s')
  pairs$HTMT[c(3,4)]<-NA_real_
 }
 bundle<-list(invariance_result=list(type='measurement_invariance',group_reliability=rel,group_htmt=pairs))
 if(kind=='empty')bundle$invariance_result<-list(type='measurement_invariance')
 ui<-structural_canvas_invariance_appendix_ui(bundle,language)
 if(kind=='empty'){stopifnot(is.null(ui));next}
 html<-as.character(ui);doc<-xml2::read_html(html,encoding='UTF-8');tables<-xml2::xml_find_all(doc,'//table');stopifnot(length(tables)==2L)
 for(i in 1:2){
  source<-list(rel,pairs)[[i]];headers<-xml2::xml_text(xml2::xml_find_all(tables[[i]],'.//th'))
  if(language!='en')stopifnot(!any(c("Cronbach's alpha",'Factor1','Factor2')%in%headers))
  if(i==1)stopifnot(headers[7]==statedu_localized_text(language,'Omega total','총 오메가'))
  for(j in seq_along(source)){
   cells<-trimws(xml2::xml_text(xml2::xml_find_all(tables[[i]],paste0('./tbody/tr/td[',j,']'))));raw<-source[[j]];name<-names(source)[j]
   if(name%in%c('Group','Factor','Factor1','Factor2'))stopifnot(identical(cells,raw))
   if(name=='k')stopifnot(identical(cells,as.character(raw)))
   if(is.numeric(raw)){
    key<-paste(kind,i,j,sep='_');if(language=='en')assign(key,cells)else stopifnot(identical(cells,get(key)))
   }
   if(name%in%c('Criterion','Reason')){
    custom<-raw%in%c('','Review','Normality','사용자 <&> %s');stopifnot(identical(cells[custom],raw[custom]))
    if(language=='en')stopifnot(identical(cells,raw))else stopifnot(all(cells[!custom]!=raw[!custom]))
   }
  }
 }
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'reliability and HTMT headings, diagnostics, identifiers and numbers\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
