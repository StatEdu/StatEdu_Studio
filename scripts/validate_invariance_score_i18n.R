Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.output_decimal_digits=3L)
out<-'tmp/invariance-score-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
fit<-lavaan::cfa('visual =~ x1 + x2 + x3',data=lavaan::HolzingerSwineford1939,group='school',group.equal='loadings')
value<-structural_canvas_invariance_score_diagnostics(fit);stopifnot(nrow(value)>0)
custom<-value[rep(1L,3L),];custom$Constraint<-c('Review','Normality','사용자 <&> %s')
stages<-c('Configural','Metric','Thresholds','Scalar','Scalar (thresholds + loadings)','Strict','사용자 <&> %s')
entries<-list()
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 score<-setNames(rep(list(value),length(stages)),stages);score[[7]]<-custom
 bundle<-list(invariance_result=list(type='measurement_invariance',score_diagnostics=score))
 html<-as.character(structural_canvas_invariance_appendix_ui(bundle,language));doc<-xml2::read_html(html,encoding='UTF-8')
 tables<-xml2::xml_find_all(doc,'//table');stopifnot(length(tables)==7L)
 titles<-xml2::xml_text(xml2::xml_find_all(doc,'//h5'))
 stopifnot(startsWith(titles[7],stages[7]))
 if(language!='en')stopifnot(!any(grepl('Configural|Metric|Thresholds|Scalar|Strict',titles[1:6])))
 for(i in seq_along(tables)){
  source<-score[[i]];headers<-xml2::xml_text(xml2::xml_find_all(tables[[i]],'.//th'))
  if(language!='en')stopifnot(!any(c('Constraint','Max |standardized EPC|','Raw p','Raw BH-adjusted p')%in%headers))
  for(j in seq_along(source)){
   cells<-trimws(xml2::xml_text(xml2::xml_find_all(tables[[i]],paste0('./tbody/tr/td[',j,']'))))
   if(j==1L)stopifnot(identical(cells,source[[j]]))
   else if(language=='en')assign(paste0('numeric',i,'_',j),cells)else stopifnot(identical(cells,get(paste0('numeric',i,'_',j))))
  }
 }
 stopifnot(is.null(structural_canvas_invariance_appendix_ui(list(invariance_result=list(type='measurement_invariance',score_diagnostics=list(Metric=value[FALSE,]))),language)))
 if(language=='ja')entries[[1]]<-list(id='score',title='Equality constraints',html=html)
 cat('PASS:',language,'six known stages, custom stage and constraints, actual score statistics, empty output\n')
}
saveRDS(entries,file.path(out,'entries.rds'))
