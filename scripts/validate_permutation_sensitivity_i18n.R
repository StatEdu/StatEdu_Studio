Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.output_decimal_digits=3L)
out<-'tmp/permutation-sensitivity-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
raw<-c('Review','Normality','사용자 <&> %s')
value<-data.frame(Path=raw,Predictor=raw,Outcome=rev(raw),'Group 1'=raw,'Group 2'=rev(raw),
 'Path difference'=c(-.123,.456,NA_real_),'Permutation p'=c(.0001,.045,NA_real_),
 'BH-adjusted p'=c(.0003,.0675,NA_real_),'MGA permutation adequate'=c(TRUE,FALSE,NA),
 'Valid permutations'=c(100L,79L,0L),check.names=FALSE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 bundle<-list(invariance_result=list(type='pls_micom',permutation_path_sensitivity=value))
 html<-as.character(structural_canvas_invariance_appendix_ui(bundle,language))
 doc<-xml2::read_html(html,encoding='UTF-8');tables<-xml2::xml_find_all(doc,'//table');stopifnot(length(tables)==1L)
 headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
 if(language!='en')stopifnot(!any(c('Path difference','Permutation p','MGA permutation adequate')%in%headers))
 stopifnot(identical(headers[2:3],c(statedu_localized_text(language,'Predictor','예측변수'),statedu_localized_text(language,'Outcome','결과변수'))))
 for(j in seq_along(value)){
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,paste0('//tbody/tr/td[',j,']'))))
  if(j<=5L)stopifnot(identical(cells,value[[j]]))
  if(is.numeric(value[[j]])){
   if(language=='en')assign(paste0('numeric',j),cells)else stopifnot(identical(cells,get(paste0('numeric',j))))
  }
  if(is.logical(value[[j]]))stopifnot(identical(cells,c(statedu_localized_text(language,'Yes','예'),statedu_localized_text(language,'No','아니요'),statedu_localized_text(language,'Not available','산출 불가'))))
 }
 stopifnot(is.null(structural_canvas_invariance_appendix_ui(list(invariance_result=list(type='pls_micom',permutation_path_sensitivity=value[FALSE,])),language)))
 if(language=='ja')entries[[1]]<-list(id='sensitivity',title='Permutation sensitivity',html=html)
 cat('PASS:',language,'sensitivity headers, identifiers, flags, numbers and empty output\n')
}
saveRDS(entries,file.path(out,'entries.rds'))
