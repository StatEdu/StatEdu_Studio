Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
errors<-lapply(c('not_declared','clustered','complex_survey','longitudinal_repeated'),function(design)tryCatch(structural_canvas_sampling_design_gate(design),error=identity))
stopifnot(all(vapply(errors,inherits,logical(1),'error')),structural_canvas_sampling_design_gate('independent_cross_sectional')$supported)
errors<-c(errors,list(simpleError('Sampling-design gate blocked estimation. Unknown design.')))
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 translated<-vapply(errors,structural_canvas_error_message,character(1),language=language)
 original<-vapply(errors,conditionMessage,character(1))
 if(language=='en')stopifnot(identical(translated,original)) else stopifnot(all(translated!=original),length(unique(translated))==5L)
 unknown<-'Review 사용자 <&> %s: engine detail 0.00012'
 stopifnot(identical(structural_canvas_error_message(simpleError(unknown),language),unknown))
 cat('PASS:',language,'four actual gate errors, fallback, unchanged English and unrecognized engine details\n')
}
