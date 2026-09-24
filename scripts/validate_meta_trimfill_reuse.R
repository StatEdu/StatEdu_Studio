invisible(try(Sys.setlocale('LC_CTYPE','English_United States.utf8'),silent=TRUE))
current<-new.env(parent=.GlobalEnv);sys.source('R/analysis_meta.R',current)
reference<-new.env(parent=.GlobalEnv);sys.source('R/analysis_meta.R',reference)
code<-deparse(body(reference$meta_trimfill),width.cutoff=500L)
stopifnot(sum(grepl('if (k0 == 0L)',code,fixed=TRUE))==1L)
code<-sub('if (k0 == 0L) original_fit else ','',code,fixed=TRUE)
body(reference$meta_trimfill)<-parse(text=code)[[1]]
capture<-function(env,input,iterations=100L,rho=.5){
 set.seed(392);conditions<-list();record<-function(x)list(class=class(x),message=conditionMessage(x))
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(env$meta_trimfill(input,rho,iterations),warning=function(w){conditions[[length(conditions)+1L]]<<-record(w);invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-record(m);invokeRestart('muffleMessage')}),error=record))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
checked<-0L;zero<-0L;filled<-0L
for(family in c('g','r','or'))for(method in c('fixed','REML','PM','DL'))for(shape in c('symmetric','skew','reverse')){
 k<-12L;yi<-switch(shape,symmetric=seq(-.6,.6,length.out=k),skew=-exp(seq(-2,2,length.out=k)),reverse=exp(seq(-2,2,length.out=k)))
 input<-structure(list(rows=data.frame(study_id=paste0('연구 ',1:k),study_name=paste0('Study ',1:k),publication_year=2020,yi=yi,vi=rep(.1,k)),family=family,model=if(method=='fixed')'fixed'else'random',tau_method=if(method=='fixed')''else method,conf_level=.95),class='statedu_meta_model')
 a<-capture(reference,input);b<-capture(current,input)
 stopifnot(identical(a,b,num.eq=FALSE),is.list(a$value$original_fit))
 zero<-zero+as.integer(a$value$k0==0);filled<-filled+as.integer(a$value$k0>0)
 for(language in c('ko','en'))stopifnot(identical(current$meta_trimfill_results_table(a$value,language),current$meta_trimfill_results_table(b$value,language),num.eq=FALSE))
 checked<-checked+1L
}
for(shape in c('zero_iterations','two_studies','missing_year','invalid_year','invalid_variance','invalid_rho','dependent')){
 x<-input;iterations<-100L;rho<-.5
 if(shape=='zero_iterations')iterations<-0L
 if(shape=='two_studies')x$rows<-x$rows[1:2,]
 if(shape=='missing_year')x$rows$publication_year[]<-NA_real_
 if(shape=='invalid_year')x$rows$publication_year[1]<-1700
 if(shape=='invalid_variance')x$rows$vi[1]<-NA_real_
 if(shape=='invalid_rho')rho<-1
 if(shape=='dependent')x$rows$study_id<-rep(paste0('S',1:4),each=3)
 stopifnot(identical(capture(reference,x,iterations,rho),capture(current,x,iterations,rho),num.eq=FALSE))
 checked<-checked+1L
}
stopifnot(zero>0L,filled>0L)
cat('PASS:',checked,'cases;',zero,'unfilled and',filled,'filled normal cases; full models, conditions, RNG and Korean/English tables exact\n')
