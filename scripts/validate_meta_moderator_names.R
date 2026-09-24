invisible(try(Sys.setlocale('LC_CTYPE','English_United States.utf8'),silent=TRUE))
current<-new.env(parent=.GlobalEnv);sys.source('R/analysis_meta.R',current)
reference<-new.env(parent=.GlobalEnv);sys.source('R/analysis_meta.R',reference)
code<-deparse(body(reference$meta_moderator_catalog),width.cutoff=500L)
stopifnot(sum(grepl('lower_names <-',code,fixed=TRUE))==1L)
code<-code[!grepl('lower_names <-',code,fixed=TRUE)]
code<-sub('lower_names == tolower(name)','tolower(long$name) == tolower(name)',code,fixed=TRUE)
body(reference$meta_moderator_catalog)<-parse(text=code)[[1]]
capture<-function(env,effects){
 set.seed(92);conditions<-list();record<-function(x)list(class=class(x),message=conditionMessage(x))
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(env$meta_moderator_catalog(effects),warning=function(w){conditions[[length(conditions)+1L]]<<-record(w);invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-record(m);invokeRestart('muffleMessage')}),error=record))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
count<-0L
for(n in c(0L,1L,24L))for(shape in c('case','unicode','invalid','empty','missing','mixed')){
 cats<-switch(shape,case=c('Region=A','region=B','REGION=C'),unicode=c('지역=서울','École=A','école=B'),invalid=c('x=A; X=B','bad; x=A','x=A'),empty='',missing=NA_character_,mixed=c('age=Young','region=A','region=B'))
 effects<-data.frame(row_id=seq_len(n),study_id=paste0('S',seq_len(n))[seq_len(n)],study_name=rep('Study',n),publication_year=rep(2020,n),outcome=rep('Outcome',n),predictor=rep('Treatment',n),moderator_categorical=rep(cats,length.out=n),moderator_continuous=rep('age=40',n))
 stopifnot(identical(capture(current,effects),capture(reference,effects),num.eq=FALSE));count<-count+1L
}
cat('PASS:',count,'catalog cases; values, attributes, conditions, stdout and RNG exact\n')
