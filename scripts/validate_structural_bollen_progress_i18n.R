Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_structural_quality_summary_i18n.R',encoding='UTF-8')
# Capture the progress API calls while executing real bootstrap fits.
structural_canvas_with_progress<-function(message,value=0,expr){messages<<-c(messages,message);force(expr)}
structural_canvas_inc_progress<-function(amount=.1,detail=NULL){messages<<-c(messages,detail)}
structural_canvas_set_progress<-function(value=NULL,detail=NULL){stopifnot(value>=0,value<=1);messages<<-c(messages,detail)}
baseline<-NULL
errors<-c('Bollen-Stine bootstrap is available only for ML estimation.','Bollen-Stine bootstrap is not available for ordered indicators.','Bollen-Stine bootstrap is currently available only for single-group CFA.','Bollen-Stine bootstrap is not informative for a saturated model with df = 0.','Bollen-Stine bootstrap requires complete analyzed data in this implementation.')
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language='en');messages<-character()
 result<-structural_canvas_run_bollen_stine_bootstrap('cfa',4L,cfa,924L,language)
 if(is.null(baseline))baseline<-result else stopifnot(identical(result,baseline))
 stopifnot(length(messages)>=4L,any(grepl('4/4',messages,fixed=TRUE)),!any(grepl('%s',messages,fixed=TRUE)))
 if(language!='en'){
  stopifnot(!any(grepl('Estimating Bollen|transformed-data bootstrap|valid replicates|Preparing bootstrap',messages)))
  stopifnot(all(vapply(errors,function(x)structural_canvas_reporting_text(x,language)!=x,logical(1))))
 }
 cat('PASS:',language,'real bootstrap unchanged, progress localization and five preflight message translations\n')
}
messages<-character();stopifnot(is.null(structural_canvas_run_bollen_stine_bootstrap('sem',4L,cfa,924L,'ja')),length(messages)==0L)
sat<-lavaan::cfa('F =~ x1+x2+x3',data=d)
stopifnot(identical(structural_canvas_bollen_stine_eligibility(sat)$reason,errors[4]))
gls<-lavaan::cfa('F =~ x1+x2+x3',data=d,estimator='GLS')
stopifnot(identical(structural_canvas_bollen_stine_eligibility(gls)$reason,errors[1]))
cat('PASS: actual saturated and non-ML preflight reasons; non-CFA skip unchanged\n')
