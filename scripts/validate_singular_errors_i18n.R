Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
actual<-tryCatch(solve(diag(c(1,1e-20))),error=identity)
stopifnot(inherits(actual,'error'),grepl('computationally singular',conditionMessage(actual),fixed=TRUE))
messages<-c('modindices: information matrix is singular','Modification indices unavailable',
 'information matrix is singular',conditionMessage(actual),
 'computationally singular: reciprocal condition number = 1.23456789e-18',
 'computationally singular')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 result<-vapply(messages,structural_canvas_error_message,character(1),language=language)
 if(language=='en')stopifnot(identical(unname(result),messages)) else stopifnot(all(result!=messages))
 stopifnot(grepl('1.23456789e-18',result[5],fixed=TRUE))
 actual_number<-sub('^.*reciprocal condition number = ','',conditionMessage(actual))
 stopifnot(grepl(actual_number,result[4],fixed=TRUE))
 if(!language %in% c('en','ko'))stopifnot(!grepl('Reciprocal condition number|reciprocal condition number',result[6]))
 unknown<-'Unknown error: 사용자 <&> Review %s 1.2e-18'
 stopifnot(structural_canvas_error_message(simpleError(unknown),language)==unknown)
 cat('PASS:',language,'MI branches, actual matrix error, exact condition number and unknown errors\n')
}
