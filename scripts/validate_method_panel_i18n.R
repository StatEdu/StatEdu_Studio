Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
make_case <- function(types,measurement='continuous',objective='confirmatory',name='Review') {
 nodes <- lapply(seq_along(types),function(i)list(id=paste0('f',i),name=paste0('F',i),role='latent',constructType=types[i]))
 nodes <- c(nodes,list(list(id='x',name=name,role='indicator')))
 structural_canvas_method_recommendation(list(nodes=nodes),data.frame(name=name,measurement=measurement),objective)
}
cases <- list(make_case('commonFactor','nominal'),make_case('unspecified'),
 make_case('composite','ordered'),make_case('commonFactor','ordered'),
 make_case('composite'),make_case(c('commonFactor','composite')),
 make_case('commonFactor',objective='predictive'),make_case('commonFactor'),
 make_case('commonFactor','nominal',name='Primary'),make_case('commonFactor','nominal',name='사용자 <&> %s'))
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(recommendation in cases)for(selected in c('CB-SEM','PLS-SEM')) {
  original <- recommendation
  doc <- xml2::read_html(as.character(structural_canvas_method_recommendation_ui(recommendation,selected,language)))
  text <- xml2::xml_text(doc);stopifnot(identical(original,recommendation))
  if(language!='en') {
   stopifnot(!grepl('Method guidance:|Show rationale and alternatives|Small samples, nonnormality',text,fixed=FALSE))
   for(reason in c(recommendation$candidates$Reason,
     recommendation$candidates$Limitation[recommendation$candidates$Reason!='Nominal indicators require a different measurement model.']))
     if(nzchar(reason))stopifnot(!grepl(reason,text,fixed=TRUE))
  }
  if(recommendation$candidates$Reason[1]=='Nominal indicators require a different measurement model.') {
    stopifnot(grepl(recommendation$candidates$Limitation[1],text,fixed=TRUE))
  }
  if(!is.na(recommendation$primary))stopifnot(grepl(recommendation$primary,text,fixed=TRUE))
 }
 cat('PASS:',language,'10 actual recommendation cases, two selections, all reason/limitation translations and literal nominal names\n')
}
