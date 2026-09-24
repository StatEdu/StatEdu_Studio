Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.output_decimal_digits=3L)
out<-'tmp/micom-pair-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
labels<-eval(body(structural_canvas_micom_pair_display)[[3]],new.env(parent=globalenv()))
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 table<-data.frame(Reason=names(labels),check.names=FALSE)
 translated<-structural_canvas_micom_pair_display(table,language)$Reason
 if(language=='en')stopifnot(identical(translated,names(labels)))else stopifnot(all(translated!=names(labels)))
}
raw<-c('Review','Normality','사용자 <&> %s')
for(kind in c('standard','unknown','fallback','empty'))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 validity<-lapply(c(95L,75L,0L),structural_canvas_pls_bootstrap_validity,requested=100L)
 group<-data.frame(Group=raw,'Valid N'=vapply(validity,function(x)x$valid,integer(1)),'Requested N'=100L,'Valid Ratio'=vapply(validity,function(x)x$ratio,numeric(1)),'Minimum Valid N'=vapply(validity,function(x)x$minimum_valid,integer(1)),Status=vapply(validity,function(x)x$status,character(1)),check.names=FALSE)
 pair<-data.frame('Group 1'=raw,'Group 2'=rev(raw),'MICOM admitted'=c(TRUE,FALSE,FALSE),'MICOM reason'=c(names(labels)[1],names(labels)[2],structural_canvas_pls_mga_pair_admission(list(evaluated=FALSE),'Review','Normality')$reason),'Valid N'=group[['Valid N']],'Requested N'=100L,'Valid Ratio'=group[['Valid Ratio']],'Minimum Valid N'=group[['Minimum Valid N']],Status=c('Adequate','Insufficient','Blocked by MICOM'),check.names=FALSE)
 gate<-data.frame('Group 1'=raw,'Group 2'=rev(raw),'N 1'=c(21L,105L,88L),'N 2'=c(88L,105L,21L),'Small-N warning'=c(names(labels)[4],'None',names(labels)[4]),'Composite-score invariance gate'=c(TRUE,FALSE,FALSE),'Constructs passed'=c(3L,2L,0L),'Constructs tested'=3L,'Valid permutations'=c(100L,81L,5L),'Requested permutations'=100L,'Valid ratio'=c(1,.81,.05),'Pair seed'=c(1201L,1202L,1203L),Reason=names(labels)[1:3],check.names=FALSE)
 if(kind=='unknown'){gate$Reason[1]<-raw[3];gate[['Small-N warning']][1]<-'Review';pair[['MICOM reason']][1]<-raw[3];pair$Status[1]<-'Review';group$Status[1]<-'Normality'}
 bundle<-list(invariance_result=list(type='pls_micom',pairwise_gate=gate,pls_mga=list(validity_gate=list(groups=group,pairs=pair)),pls_modmed_mga=list(pairwise_validity=pair)))
 if(kind=='fallback'){bundle$invariance_result$pls_mga$pls_modmed_mga<-bundle$invariance_result$pls_modmed_mga;bundle$invariance_result$pls_modmed_mga<-NULL}
 if(kind=='empty')bundle<-list(invariance_result=list(type='pls_micom'))
 ui<-structural_canvas_invariance_appendix_ui(bundle,language);if(kind=='empty'){stopifnot(is.null(ui));next}
 html<-as.character(ui);doc<-xml2::read_html(html,encoding='UTF-8');tables<-xml2::xml_find_all(doc,'//table');stopifnot(length(tables)==4L)
 originals<-list(gate,group,pair,pair)
 for(i in 1:4){
  original<-originals[[i]];headers<-xml2::xml_text(xml2::xml_find_all(tables[[i]],'./thead//th'))
  if(language!='en')stopifnot(!any(c('Small-N warning','Constructs passed','Constructs tested','MICOM reason','Valid Ratio','Minimum Valid N')%in%headers))
  for(j in seq_along(original)){
   cells<-trimws(xml2::xml_text(xml2::xml_find_all(tables[[i]],paste0('./tbody/tr/td[',j,']'))))
   name<-names(original)[j];source<-original[[j]]
   if(name%in%c('Group','Group 1','Group 2'))stopifnot(identical(cells,source))
   if(is.logical(source))stopifnot(identical(cells,ifelse(source,statedu_localized_text(language,'Yes','예'),statedu_localized_text(language,'No','아니요'))))
   if(is.numeric(source)){
    if(language=='en')assign(paste0(kind,i,j),cells,envir=globalenv())else stopifnot(identical(cells,get(paste0(kind,i,j),envir=globalenv())))
   }
   if(name%in%c('Reason','MICOM reason','Small-N warning','Status')){
    custom<-source%in%raw;stopifnot(identical(cells[custom],source[custom]))
    if(language=='en')stopifnot(identical(cells,source))else stopifnot(all(cells[!custom]!=source[!custom]))
   }
  }
 }
 if(language=='ja'&&kind!='fallback')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'four validity tables, exact user groups, flags and numeric values\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
