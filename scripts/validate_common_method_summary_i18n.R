Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.output_decimal_digits=3L)
languages<-c('en','ko','ja','zh','es','fr','de','vi')
raw<-c('Review','Normality','사용자 <&> %s','Primary')
helpers<-c('method','statistic','status','guidance','conclusion_guidance')
out<-'tmp/common-method-summary-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
for(type in helpers){
 fn<-get(paste0('structural_canvas_common_method_',type,'_label'))
 labels<-eval(body(fn)[[2]],new.env(parent=globalenv()))
 for(language in languages){
  actual<-vapply(names(labels),fn,character(1),language=language,USE.NAMES=FALSE)
  if(language=='en')stopifnot(identical(actual,names(labels)))else stopifnot(all(actual!=names(labels)))
  # Only recognized program values are translated.
  unknown<-setdiff(raw,names(labels))
  stopifnot(identical(vapply(unknown,fn,character(1),language=language,USE.NAMES=FALSE),unknown))
 }
 cat('PASS: all known',type,'labels in eight languages; unknown labels preserved\n')
}
for(language in languages)for(prefix in c('Single-factor CFA failed:','Common latent factor model failed:')){
 value<-paste(prefix,paste(raw,collapse=' / '));translated<-structural_canvas_common_method_guidance_label(value,language)
 stopifnot(endsWith(translated,substring(value,nchar(prefix)+1L)))
 if(language=='en')stopifnot(identical(translated,value))else stopifnot(!startsWith(translated,prefix))
}
# Exercise all combinations of the four review reasons, plus other conclusion branches.
methods<-c('Common latent factor screen','Common latent factor screen','Single-factor CFA comparison','Harman single-factor screen')
statistics<-c('Max loading change','Mean absolute method loading','One-factor model fit','First factor percent')
for(scenario in c(as.character(0:15),'flag','unavailable','screen','empty','fallback'))for(language in languages){
 summary<-data.frame(Method=methods,Statistic=statistics,Value=c(.234,.567,.123,56.789),Status='OK',Guidance='Descriptive size of the common method factor loadings.',check.names=FALSE)
 if(scenario%in%as.character(0:15))summary$Status[as.logical(intToBits(as.integer(scenario))[1:4])]<-'Review'
 if(scenario=='flag')summary$Status[1]<-'Screen flag'
 if(scenario=='unavailable')summary$Status[1]<-'Not available'
 if(scenario=='screen')summary$Status[1]<-'Screen only'
 if(scenario=='fallback'){summary$Method<-'Custom method';summary$Status<-'Review'}
 if(scenario=='empty')summary<-summary[FALSE,]
 result<-list(summary=summary)
 conclusion<-structural_canvas_common_method_conclusion(result,language)
 if(language=='en')en_conclusion<-conclusion else stopifnot(all(conclusion!=en_conclusion))
 display<-structural_canvas_common_method_display_table(result,language=language)
 if(nrow(display))stopifnot(identical(display[[3]],c('.234','.567','.123','56.789')))
 attr(conclusion,'result_user_columns')<-seq_along(conclusion)
 html<-as.character(htmltools::tagList(
  htmltools::tags$h5(statedu_localized_text(language,'Common method bias diagnostics','동일방법편의 진단')),
  structural_canvas_basic_html_table(conclusion,language=language),
  if(nrow(display))structural_canvas_basic_html_table(display,language=language)))
 if(language=='ja'&&scenario%in%c('15','flag','unavailable','screen','empty','fallback'))entries[[scenario]]<-list(id=scenario,title=scenario,html=html)
}
# Unknown custom content must survive the actual generic table renderer too.
for(language in languages){
 summary<-data.frame(Method=raw,Statistic=raw,Value=c(.123,.234,.345,.456),Status=raw,Guidance=raw,check.names=FALSE)
 display<-structural_canvas_common_method_display_table(list(summary=summary),language=language)
 html<-as.character(structural_canvas_basic_html_table(display,language=language))
 doc<-xml2::read_html(html,encoding='UTF-8')
 for(j in c(1,2,5))stopifnot(identical(trimws(xml2::xml_text(xml2::xml_find_all(doc,paste0('//tbody/tr/td[',j,']')))),raw))
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
cat('PASS: 21 conclusion scenarios x 8 languages; dynamic error details, exact values, unknown user text and export fixtures\n')

