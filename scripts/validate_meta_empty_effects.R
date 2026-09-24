invisible(try(Sys.setlocale('LC_CTYPE','English_United States.utf8'),silent=TRUE))
current<-new.env(parent=.GlobalEnv);sys.source('R/analysis_meta.R',current)
reference<-new.env(parent=.GlobalEnv);sys.source('R/analysis_meta.R',reference)
# Rebuild the same declared columns with the original general data.frame constructor.
column_call<-body(current$meta_empty_effects)[[2]][[2]]
stopifnot(identical(column_call[[1]],as.name('list')))
constructor<-as.call(c(list(as.name('data.frame')),as.list(column_call)[-1],list(stringsAsFactors=FALSE,check.names=FALSE)))
body(reference$meta_empty_effects)<-substitute({X},list(X=constructor))
a<-current$meta_empty_effects();b<-reference$meta_empty_effects()
stopifnot(identical(a,b,num.eq=FALSE),identical(names(a),current$meta_effect_columns()),nrow(a)==0L)
a[1,'study_id']<-'changed'
stopifnot(identical(current$meta_empty_effects(),b,num.eq=FALSE))
capture<-function(env,values){
 set.seed(951);conditions<-list();record<-function(x)list(class=class(x),message=conditionMessage(x))
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(env$meta_normalize_effect(values,row_id=7L),warning=function(w){conditions[[length(conditions)+1L]]<<-record(w);invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-record(m);invokeRestart('muffleMessage')}),error=record))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
checked<-0L
for(family in c('g','r','or'))for(type in names(current$meta_input_types(family)))for(shape in c('valid','absent','text','factor','missing','nonfinite','vector','moderator','negative')){
 values<-list(study_id='연구 α',study_name='Study 1',publication_year=2020,family=family,input_type=type,direction='positive',
 m1=3,sd1=1,n1=30,m0=2,sd0=1,n0=35,n=65,g=.4,d_value=.4,r=.3,r_pb=.3,fisher_z=.3,t_value=2,k_controls=1,
 cell_a=10,cell_b=20,cell_c=5,cell_d=25,or_value=2,log_or=.5,logit_b=.5,se=.1,ci_lower=.1,ci_upper=.8)
 fields<-setdiff(names(values),c('study_id','study_name','publication_year','family','input_type','direction'))
 if(shape=='absent')values[fields]<-rep(list(NULL),length(fields))
 if(shape=='text')values[fields]<-lapply(values[fields],as.character)
 if(shape=='factor')values[fields]<-lapply(values[fields],factor)
 if(shape=='missing')values[fields]<-rep(list(NA_real_),length(fields))
 if(shape=='nonfinite')values[fields]<-rep(list(Inf),length(fields))
 if(shape=='vector')values[fields]<-lapply(values[fields],function(x)c(x,999))
 if(shape=='moderator')values$moderator_continuous<-'age=invalid'
 if(shape=='negative')values$direction<-'negative'
 stopifnot(identical(capture(current,values),capture(reference,values),num.eq=FALSE))
 checked<-checked+1L
}
cat('PASS:',checked,'input-format cases; values, attributes, conditions, stdout and RNG exact\n')

