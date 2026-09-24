source('scripts/validate_longitudinal_family_guidance_i18n.R',encoding='UTF-8')
out<-'tmp/longitudinal-gee-family-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
set.seed(928)
d<-data.frame(id=rep(1:50,each=5),time=rep(0:4,50),x=rnorm(250))
eta<-.2+.2*d$x+.05*d$time+rep(rnorm(50,sd=.25),each=5)
outcomes<-list(gaussian=eta+rnorm(250),binomial=rbinom(250,1,plogis(eta)),
 poisson=rpois(250,exp(eta)),negative_binomial=rnbinom(250,mu=exp(eta),size=2),
 gamma=rgamma(250,shape=3,scale=exp(eta)/3))
tables<-list();fits<-list()
for(family in names(outcomes)) {
 input<-d;input$y<-outcomes[[family]]
 fit<-longitudinal_fit_model(input,'y','id','time',c('time','x'),'gee',family,'exchangeable')
 checks<-longitudinal_assumption_checks(input,fit$model,'gee',family,fit$formula,'id','time','exchangeable',check_options='family')$checks
 tables[[family]]<-checks[checks$Check=='Outcome family / link',,drop=FALSE]
 stopifnot(nrow(tables[[family]])==1L,nrow(fit$coef_table)>0)
 fits[[family]]<-fit$coef_table
}
# The unresolved count label is a helper branch, not a separate fitted family.
tables$count<-longitudinal_check_response_family('count','gee')
before<-serialize(list(tables,fits),NULL);missing<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(name in names(tables)) {
 original<-tables[[name]];translated<-longitudinal_appendix_table(original,lang)
 for(column in c('Check','Result','Interpretation','Recommendation')) {
  values<-original[[column]];actual<-translated[[match(column,names(original))]]
  if(lang!='en' && any(values==actual))missing[[length(missing)+1L]]<-data.frame(language=lang,english=values[values==actual])
 }
 for(column in c('Statistic','p','Issue'))stopifnot(identical(original[[column]],translated[[match(column,names(original))]]))
 probe<-data.frame(Variable=original$Interpretation)
 stopifnot(identical(longitudinal_appendix_table(probe,lang)[[1]],probe[[1]]),identical(before,serialize(list(tables,fits),NULL)))
}
if(length(missing)) {
 missing<-unique(do.call(rbind,missing));write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated GEE family guidance',call.=FALSE)
}
saveRDS(tables,file.path(out,'tables.rds'))
cat('PASS five fitted GEE-path families and one count helper in eight languages; source results and user labels preserved\n')
