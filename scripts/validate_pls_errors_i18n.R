Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
literal<-'Review 사용자 <&> %s, x.1'
messages<-c(
 paste0('PLS model contract blocked estimation: observed covariates/control variables and covariateTargets. Covariates: ',literal,'.'),
 paste0('PLS model contract blocked estimation: fixed/free constraints, fixed values, start values, parameter names, and equality labels. Modified elements: ',literal,'.'),
 'PLS model contract blocked estimation: directed structural paths must be acyclic.',
 paste0('PLS model contract blocked estimation: each indicator may belong to only one construct. Duplicate indicator ownership: ',literal,'.'),
 'PLS model contract blocked estimation: unsupported specification.',
 paste0('PLS model indicators missing from the current data: ',literal,'. Reassign the highlighted measurement variables to columns in the current data.'),
 'undefined columns selected')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 results<-vapply(messages,function(message)structural_canvas_error_message(simpleError(message),language),character(1))
 if(language=='en')stopifnot(identical(unname(results),unname(messages))) else stopifnot(all(results!=messages),length(unique(results))==7L)
 stopifnot(all(vapply(results[c(1,2,4,6)],function(text)grepl(literal,text,fixed=TRUE),logical(1))))
 unknown<-paste0('Other engine failure: ',literal)
 stopifnot(structural_canvas_error_message(unknown,language)==unknown)
 cat('PASS:',language,'seven PLS errors; literal names, punctuation and unknown errors preserved\n')
}
