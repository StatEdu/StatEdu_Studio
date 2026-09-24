source('output/survival-interval-order-20260914/common.R')
cases<-list()
for(n in c(0L,1L,30L,120L))for(missing in c(0,.1,1)) {
  data<-make_data(n,missing);cases[[length(cases)+1L]]<-list(data=data,settings=settings)
}
for(i in 1:36) {
  data<-make_data(120L,.1);set.seed(i)
  data$time[sample.int(120,12)]<-sample(c(NA_real_,-1,Inf),12,TRUE)
  data$event[sample.int(120,12)]<-NA_integer_
  data$group[sample.int(120,12)]<-NA
  setup<-settings
  if(i %% 3L == 1L) {
    setup$data_shape<-'entry_exit';setup$roles$entry<-'entry'
    data$entry<-sample(c(0,10,NA_real_),120,TRUE)
  } else if(i %% 3L == 2L) {
    setup$data_shape<-'start_stop';setup$roles$subject_id<-'id';setup$roles$start<-'start';setup$roles$stop<-'time'
    data$id<-rep(1:60,each=2);data$id[sample.int(120,3)]<-NA_integer_
    data$start<-rep(c(0,5),60)
  }
  cases[[length(cases)+1L]]<-list(data=data,settings=setup)
}
for(case in cases) {
  answers<-list()
  for(v in c('baseline','current')) {
    select_variant(v);conditions<-character();set.seed(941)
    result<-withCallingHandlers(tryCatch(survival_preflight(case$data,case$settings),error=function(e)list(error=conditionMessage(e))),
      warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')},
      message=function(m){conditions<<-c(conditions,conditionMessage(m));invokeRestart('muffleMessage')})
    answers[[v]]<-list(result=result,conditions=conditions,rng=.Random.seed)
  }
  stopifnot(identical(answers$baseline,answers$current,num.eq=FALSE))
}
cat('PASS:',length(cases),'complete preflight results/errors, conditions and RNG states.\n')
