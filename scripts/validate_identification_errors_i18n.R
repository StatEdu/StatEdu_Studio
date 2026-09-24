Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
codes<-c('unmeasured_latent','single_indicator_auto_fixed','single_indicator_constrained','two_indicators','few_lower_order_factors','mixed_measurement_level','multiple_higher_order_parents','invalid_fixed_residual','negative_fixed_residual','boundary_fixed_residual','duplicate_path','duplicate_covariance')
source_lines<-readLines('R/setup_custom_model_canvas_structural_identification_diagnostics.R',encoding='UTF-8')
messages<-vapply(codes,function(code){
 line<-source_lines[grepl(paste0('"',code,'", "'),source_lines,fixed=TRUE)]
 stopifnot(length(line)==1L)
 sub(paste0('^.*"',code,'", "([^"]*)".*$'),'\\1',line)
},character(1))
issues<-data.frame(Element=rep('Review 사용자 <&> %s',length(codes)),Code=codes,Message=unname(messages))
structural_canvas_show_notification<-function(message,...){captured<<-message;invisible(TRUE)}
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 result<-mapply(structural_canvas_identification_issue_message,codes,messages,MoreArgs=list(language=language),USE.NAMES=FALSE)
 if(language=='en')stopifnot(identical(result,unname(messages)))
 if(!language %in% c('en','ko'))stopifnot(all(result!=messages))
 failure<-structural_canvas_identification_issue_text(issues,language)
 captured<-NULL;structural_canvas_notify_identification_warnings(issues,language)
 stopifnot(grepl(issues$Element[1],failure,fixed=TRUE),grepl(issues$Element[1],captured,fixed=TRUE))
 if(language!='en')stopifnot(!startsWith(failure,'Model identification'),!startsWith(captured,'Identification warning'))
 for(code in c('cross_loading','unknown'))stopifnot(structural_canvas_identification_issue_message(code,'Review',language)=='Review')
 cat('PASS:',language,'12 static issues, failure/warning prefixes, literal elements and dynamic-message fallback\n')
}
