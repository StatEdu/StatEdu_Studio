Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_render<-function(x){
 if(missing(x)||!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&grepl('"_result_redundancy"',paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]][[2]])
 for(item in as.list(x)[-1]){r<-find_render(item);if(!is.null(r))return(r)}
 NULL
}
expr<-find_render(body(structural_canvas_register_result_outputs));stopifnot(!is.null(expr))
options(statedu.output_decimal_digits=3L)
out<-'tmp/redundancy-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
labels<-eval(body(structural_canvas_redundancy_text)[[2]],new.env(parent=globalenv()))
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 actual<-structural_canvas_redundancy_text(names(labels),language)
 if(language=='en')stopifnot(identical(actual,names(labels)))else stopifnot(all(actual!=names(labels)))
 stopifnot(identical(structural_canvas_redundancy_text(c('Review','사용자 <&> %s'),language),c('Review','사용자 <&> %s')))
}
results<-list()
for(i in 1:10)results[[paste0('reason',i)]]<-list(available=FALSE,reason=names(labels)[i])
results$custom<-list(available=FALSE,reason='Review 사용자 <&> %s')
results$missing<-list(available=FALSE)
# Real redundancy calculations with strong, weak, and negative correlations.
snapshot<-list(nodes=list(list(id='f',name='Review',role='latent',constructType='composite',measurementMode='formative')),edges=list())
for(kind in c('strong','weak','negative')){
 score<-seq_len(20);y<-switch(kind,strong=score+sin(score),weak=rep(c(-1,1),10),negative=-score+sin(score))
 data<-data.frame(y);names(data)<-'사용자 <&> %s'
 scores<-matrix(score,ncol=1,dimnames=list(NULL,'Review'))
 results[[kind]]<-structural_canvas_pls_redundancy_analysis(list(fit=list(construct_scores=scores,rawdata=data)),snapshot,data,'Review','사용자 <&> %s')
 stopifnot(isTRUE(results[[kind]]$available))
}
for(kind in names(results))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 result<-results[[kind]]
 env<-new.env(parent=globalenv());env$analysis_type<-'plssem';env$fit_result<-function()list(redundancy_result=result);env$ui_language<-function()language;env$app_language_fn<-function()language
 render<-function()NULL;body(render)<-expr;environment(render)<-env
 html<-as.character(render());doc<-xml2::read_html(html,encoding='UTF-8')
 heading<-xml2::xml_text(xml2::xml_find_first(doc,'//h5'));notes<-xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 if(language=='en'){en_heading<-heading;en_notes<-notes}else stopifnot(heading!=en_heading,!identical(notes,en_notes))
 if(isTRUE(result$available)){
  cells<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//td')))
  expected<-c(result$construct,result$criterion,as.character(result$n),vapply(c(result$loading,result$ci_lower,result$ci_upper,result$r2),format_decimal3,character(1)))
  stopifnot(identical(cells[1:7],expected))
  if(language=='en')stopifnot(cells[8]==result$guidance)else stopifnot(cells[8]!=result$guidance)
  headers<-xml2::xml_text(xml2::xml_find_all(doc,'//th'))
  if(language!='en')stopifnot(!any(c('Construct','Global criterion variable','Loading','95% CI lower','95% CI upper','Guidance')%in%headers))
 }else if(kind=='custom')stopifnot(grepl(result$reason,paste(notes,collapse=' '),fixed=TRUE))
 options(statedu.app_language=language)
 main<-as.character(structural_canvas_measurement_html_table(data.frame(Latent='Review',Indicator='사용자 <&> %s',B='.700',SE='.050',beta='.720',z='14.000',p='<.001',check.names=FALSE)))
 if(language=='en')en_main<-main else stopifnot(identical(main,en_main))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=paste(main,html))
 cat('PASS:',kind,language,'title, explanation, exact values and user names\n')
}
env$analysis_type<-'cfa';stopifnot(is.null(render()))
saveRDS(unname(entries),file.path(out,'entries.rds'))
