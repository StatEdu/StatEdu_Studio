Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_render<-function(x){
 if(missing(x)||!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&grepl('"_result_parcel_plan"',paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]][[2]])
 for(item in as.list(x)[-1]){r<-find_render(item);if(!is.null(r))return(r)}
 NULL
}
expr<-find_render(body(structural_canvas_register_result_outputs));stopifnot(!is.null(expr))
options(statedu.output_decimal_digits=3L)
out<-'tmp/parcel-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
languages<-c('en','ko','ja','zh','es','fr','de','vi')
labels<-eval(body(structural_canvas_parcel_text)[[2]],new.env(parent=globalenv()))
for(language in languages){
 actual<-vapply(names(labels),structural_canvas_parcel_text,character(1),language=language,USE.NAMES=FALSE)
 if(language=='en')stopifnot(identical(actual,names(labels)))else stopifnot(all(actual!=names(labels)))
 stopifnot(structural_canvas_parcel_text('Review 사용자 <&> %s',language)=='Review 사용자 <&> %s')
}
for(language in languages)for(n in c(3L,4L)){
 original<-sprintf('At least %s indicators are required to preview %s parcels with at least two items each.',n*2L,n)
 translated<-structural_canvas_parcel_text(original,language)
 stopifnot(grepl(as.character(n*2L),translated,fixed=TRUE),grepl(as.character(n),translated,fixed=TRUE))
 if(language=='en')stopifnot(identical(original,translated))else stopifnot(!grepl('At least',translated,fixed=TRUE))
}
set.seed(782);factor<-rnorm(150);data<-as.data.frame(replicate(8,.8*factor+rnorm(150,sd=.5)));names(data)<-paste0('x',1:8)
fit<-lavaan::cfa('Review =~ x1+x2+x3+x4+x5+x6+x7+x8',data=data)
snapshot<-list(nodes=list(list(id='f',name='Review',role='latent',constructType='commonFactor',measurementMode='reflective')),edges=list())
base<-structural_canvas_parcel_plan(list(fit=fit,admissible=TRUE),snapshot,TRUE,'Review',3L,'사용자 목적 <&> %s')
stopifnot(base$available,nrow(base$allocation)==8L)
results<-list(requested=base)
results$applied<-structural_canvas_parcel_plan(list(fit=fit,admissible=TRUE),snapshot,TRUE,'Review',4L,'Normality')
results$applied$applied<-TRUE;results$applied$status<-'Item-level parcel-factor CFA fitted';results$applied$item_level_constructs<-c('Primary','사용자 <&> %s')
results$failed<-base;results$failed$applied<-FALSE;results$failed$status<-'Item-level parcel-factor model could not be fitted';results$failed$fit_error<-'Review 사용자 오류 <&> %s';results$failed$warning<-paste(base$warning,results$failed$fit_error)
results$no_purpose<-structural_canvas_parcel_plan(list(fit=fit,admissible=TRUE),snapshot,TRUE,'Review',3L,'')
for(i in 1:7)results[[paste0('reason',i)]]<-list(enabled=TRUE,available=FALSE,reason=names(labels)[i])
for(n in c(3L,4L))results[[paste0('count',n)]]<-list(enabled=TRUE,available=FALSE,reason=sprintf('At least %s indicators are required to preview %s parcels with at least two items each.',n*2L,n))
results$custom<-list(enabled=TRUE,available=FALSE,reason='Normality 사용자 오류 <&> %s')
for(kind in names(results))for(language in languages){
 result<-results[[kind]]
 if(isTRUE(result$available)){
  result$allocation$Indicator[1:3]<-c('Review','Normality','사용자 <&> %s')
  result$summary$Items[1]<-'Review, Normality, 사용자 <&> %s'
 }
 env<-new.env(parent=globalenv());env$analysis_type<-'cfa';env$fit_result<-function()list(parcel_result=result);env$ui_language<-function()language;env$app_language_fn<-function()language
 render<-function()NULL;body(render)<-expr;environment(render)<-env
 html<-as.character(render());doc<-xml2::read_html(html,encoding='UTF-8');heading<-xml2::xml_text(xml2::xml_find_first(doc,'//h5'));notes<-xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 if(language=='en'){en_heading<-heading;en_notes<-notes}else stopifnot(heading!=en_heading,!identical(notes,en_notes))
 if(isTRUE(result$available)){
  for(section in c('allocation','summary')){
   original<-result[[section]];node<-xml2::xml_find_first(doc,paste0('//table[contains(@class,"structural-parcel-',section,'-table")]'))
   for(j in seq_along(original)){
    cells<-trimws(xml2::xml_text(xml2::xml_find_all(node,paste0('./tbody/tr/td[',j,']'))))
    expected<-if(is.numeric(original[[j]]))vapply(original[[j]],format_decimal3,character(1))else original[[j]]
    stopifnot(identical(cells,expected))
   }
   headers<-xml2::xml_text(xml2::xml_find_all(node,'./thead//th'))
   if(language!='en')stopifnot(!any(c('Indicator','Loading','Standardized loading','Mean absolute loading','Items')%in%headers))
  }
  if(nzchar(result$purpose))stopifnot(grepl(result$purpose,paste(notes,collapse=' '),fixed=TRUE))
  if(kind=='failed')stopifnot(grepl(result$fit_error,paste(notes,collapse=' '),fixed=TRUE))
  if(language!='en')stopifnot(!grepl(result$status,paste(notes,collapse=' '),fixed=TRUE),!grepl('Loading-balanced allocation',paste(notes,collapse=' '),fixed=TRUE))
 }else if(kind=='custom')stopifnot(grepl(result$reason,paste(notes,collapse=' '),fixed=TRUE))
 options(statedu.app_language=language)
 main<-as.character(structural_canvas_measurement_html_table(data.frame(Latent='Review',Indicator='사용자 <&> %s',B='.700',SE='.050',beta='.720',z='14.000',p='<.001',check.names=FALSE)))
 if(language=='en')en_main<-main else stopifnot(identical(main,en_main))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=paste(main,html))
 cat('PASS:',kind,language,'program text, exact allocation, values and user content\n')
}
env$analysis_type<-'plssem';stopifnot(is.null(render()));env$analysis_type<-'cfa';result<-list(enabled=FALSE);stopifnot(is.null(render()))
saveRDS(unname(entries),file.path(out,'entries.rds'))
