invisible(try(Sys.setlocale('LC_CTYPE','English_United States.utf8'),silent=TRUE))
current<-new.env(parent=.GlobalEnv);sys.source('R/analysis_meta.R',current)
reference<-new.env(parent=.GlobalEnv);sys.source('R/analysis_meta.R',reference)
reference$meta_moderator_parser<-function(...)reference$meta_parse_moderators
capture<-function(fn){
 set.seed(145);conditions<-list();record<-function(x)list(class=class(x),message=conditionMessage(x))
 stdout<-capture.output(value<-tryCatch(withCallingHandlers(fn(),warning=function(w){conditions[[length(conditions)+1L]]<<-record(w);invokeRestart('muffleWarning')},message=function(m){conditions[[length(conditions)+1L]]<<-record(m);invokeRestart('muffleMessage')}),error=record))
 list(value=value,conditions=conditions,stdout=stdout,rng=.Random.seed)
}
checked<-0L
for(shape in c('repeated','unique','empty','missing','factor','unicode','bad','duplicate','delimiter')){
 n<-24L
 cats<-switch(shape,repeated=rep(c('region=Asia','region=Europe'),12),unique=paste0('region=G',1:n),empty=rep('',n),missing=rep(NA_character_,n),factor=factor(rep('region=Asia',n)),unicode=rep('지역=서울; region=아시아',n),bad=rep('region=Asia; bad',n),duplicate=rep('age=young',n),delimiter=rep(c('region=A::B','region=A; B=C'),12))
 effects<-data.frame(row_id=1:n,study_id=paste0('S',1:n),study_name='Study',publication_year=2020,outcome='Outcome',predictor='Treatment',moderator_categorical=cats,moderator_continuous='age=40')
 for(key in c('catalog','categorical::region','continuous::age','builtin::publication_year','invalid')){
  run<-function(env)if(key=='catalog')env$meta_moderator_catalog(effects)else env$meta_extract_moderator(effects,key)
  stopifnot(identical(capture(function()run(current)),capture(function()run(reference)),num.eq=FALSE));checked<-checked+1L
 }
}
# Quiet hits, FIFO eviction, mutation isolation, and uncached warning/message calls.
original<-current$meta_parse_moderators;calls<-0L
current$meta_parse_moderators<-function(...){calls<<-calls+1L;original(...)}
parser<-current$meta_moderator_parser();a<-parser('region=Asia','age=40');b<-parser('region=Asia','age=40')
stopifnot(calls==1L,identical(a,b));a$data$value[1]<-'changed'
stopifnot(identical(parser('region=Asia','age=40'),b))
for(i in 1:9)parser(paste0('region=',i),'age=40')
before<-calls;invisible(parser('region=Asia','age=40'));stopifnot(calls==before+1L)
stopifnot(identical(current$meta_moderator_parser(c('a','b'),c('x','x')),current$meta_parse_moderators))
current$meta_parse_moderators<-function(...){calls<<-calls+1L;warning('test warning');message('test message');original(...)}
parser<-current$meta_moderator_parser();before<-calls
x<-capture(function()parser('region=Asia','age=40'));y<-capture(function()parser('region=Asia','age=40'))
stopifnot(calls==before+2L,identical(x,y,num.eq=FALSE))
cat('PASS:',checked,'catalog/extraction cases; hits, eviction, mutation isolation, conditions and RNG checks\n')
