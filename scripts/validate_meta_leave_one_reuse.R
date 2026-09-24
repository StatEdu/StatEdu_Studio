invisible(try(Sys.setlocale('LC_CTYPE','English_United States.utf8'),silent=TRUE))
source('R/analysis_meta.R',encoding='UTF-8')
reference <- function(result, rho=.5) {
  aggregated <- meta_aggregate_study_effects(result,rho)
  if(nrow(aggregated)<3L) stop('Leave-one-study-out analysis requires at least three studies.',call.=FALSE)
  method <- if(nzchar(result$tau_method)) result$tau_method else 'REML'
  base <- meta_fit_aggregated(aggregated,result$family,result$model,method,result$conf_level)
  do.call(rbind,lapply(seq_len(nrow(aggregated)),function(index) {
    fit <- meta_fit_aggregated(aggregated[-index,,drop=FALSE],result$family,result$model,method,result$conf_level)
    data.frame(omitted_study=aggregated$study_id[[index]],estimate=fit$estimate,
      ci_lower=fit$ci[[1]],ci_upper=fit$ci[[2]],change=fit$estimate-base$estimate,stringsAsFactors=FALSE)
  }))
}
capture <- function(fn,input,rho) {
  set.seed(923); conditions<-list()
  record<-function(x)list(class=class(x),message=conditionMessage(x))
  stdout<-capture.output(value<-tryCatch(withCallingHandlers(fn(input,rho),
    warning=function(w){conditions[[length(conditions)+1L]]<<-record(w);invokeRestart('muffleWarning')},
    message=function(m){conditions[[length(conditions)+1L]]<<-record(m);invokeRestart('muffleMessage')}),error=record))
  list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
checked<-0L
for(family in c('g','r','or')) for(method in c('fixed','REML','PM','DL')) for(shape in c('single','mixed','named')) {
  set.seed(487)
  ids<-if(shape=='single')c('연구 가','S3','β','S1')else c('연구 가','연구 가','S3','β','β','β','S1')
  n<-length(ids)
  input<-structure(list(rows=data.frame(study_id=ids,study_name=ids,publication_year=2020,
    yi=rnorm(n,.3,.4),vi=runif(n,.03,.12)),family=family,model=if(method=='fixed')'fixed'else'random',
    tau_method=if(method=='fixed')''else method,conf_level=.90),class='statedu_meta_model')
  if(shape=='named') rownames(input$rows)<-paste0('row',seq_len(n))
  rho<-if(shape=='single')0 else .7
  a<-capture(reference,input,rho);b<-capture(meta_leave_one_study_out,input,rho)
  stopifnot(identical(a,b,num.eq=FALSE))
  for(language in c('ko','en')) stopifnot(identical(meta_leave_one_study_out_table(a$value,family,language),meta_leave_one_study_out_table(b$value,family,language),num.eq=FALSE))
  # Verify complete reduced models, including normalized rows and their identifiers.
  aggregated<-meta_aggregate_study_effects(input,rho)
  tau<-if(nzchar(input$tau_method))input$tau_method else 'REML'
  base<-meta_fit_aggregated(aggregated,family,input$model,tau,input$conf_level)
  for(index in seq_len(nrow(aggregated))) {
    effects<-base$rows[-index,,drop=FALSE];effects$row_id<-seq_len(nrow(effects));rownames(effects)<-NULL
    original<-meta_fit_aggregated(aggregated[-index,,drop=FALSE],family,input$model,tau,input$conf_level)
    reused<-meta_fit_model(effects,family,input$model,tau,input$conf_level,prediction_interval=FALSE)
    stopifnot(identical(original,reused,num.eq=FALSE))
  }
  checked<-checked+1L
}
for(shape in c('too_few','invalid_year','missing_year','invalid_variance','invalid_rho')) {
  x<-input;rho<-.5
  if(shape=='too_few') x$rows<-x$rows[1:3,]
  if(shape=='invalid_year') x$rows$publication_year[1]<-1700
  if(shape=='missing_year') x$rows$publication_year[]<-NA_real_
  if(shape=='invalid_variance') x$rows$vi[1]<-NA_real_
  if(shape=='invalid_rho') rho<-1
  stopifnot(identical(capture(reference,x,rho),capture(meta_leave_one_study_out,x,rho),num.eq=FALSE))
  checked<-checked+1L
}
cat('PASS:',checked,'leave-one-study-out cases; complete reduced-model and Korean/English table equality\n')
