source('R/utils.R',encoding='UTF-8')
source('R/analysis_interrater_agreement.R',encoding='UTF-8')
reference<-new.env(parent=.GlobalEnv);sys.source('R/analysis_interrater_agreement.R',reference)
reference$interrater_pair_table<-function(x,y,levels) {
  ok<-!is.na(x)&!is.na(y)
  x<-factor(as.character(x[ok]),levels=levels)
  y<-factor(as.character(y[ok]),levels=levels)
  table(x,y)
}
args<-commandArgs(TRUE)
if(length(args))sys.source(args[[1L]],reference)
capture_kappa<-function(expr) {
  warnings<-messages<-character()
  value<-withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},
    message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(93)
vectors<-list(sample(c('A','B','C',NA),60,TRUE),factor(rep(c('A','C'),30),levels=c('C','B','A')),
  rep(NA_character_,60),character(),rep(1:3,20),rep(c('','한글','x y'),20))
levels_list<-list(c('A','B','C'),c('C','B','A','unused'),character(),c('A',NA,'B'),c('','한글','x y'),1:3,c('A','A','B'))
count<-0L
for(x in vectors)for(y in vectors)for(levels in levels_list) {
  before<-capture_kappa(reference$interrater_pair_table(x,y,levels))
  after<-capture_kappa(interrater_pair_table(x,y,levels))
  stopifnot(identical(before,after,num.eq=FALSE));count<-count+1L
}
for(k in c(2,3,10)) {
  frame<-as.data.frame(matrix(sample(c('A','B','C',NA),80*k,TRUE),80,k))
  for(fun in c('interrater_cohen_kappa','interrater_light_kappa','interrater_weighted_kappa')) {
    before<-capture_kappa(reference[[fun]](frame,c('C','B','A','unused')))
    after<-capture_kappa(get(fun)(frame,c('C','B','A','unused')))
    stopifnot(identical(before,after,num.eq=FALSE));count<-count+1L
  }
}
cat('PASS:',count,'exact contingency table/kappa/condition/RNG comparisons.\n')
