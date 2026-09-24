Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/longitudinal-variance-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
mixed_fit<-function(effects) {
 n<-length(effects);d<-data.frame(id=factor(rep(seq_len(n),each=4)),x=rep(c(-1,1,-1,1),n))
 d$y<-rep(effects,each=4)+.3*d$x+rep(c(-.2,-.2,.2,.2),n)
 lme4::lmer(y~x+(1|id),data=d)
}
normal_fit<-mixed_fit(qnorm((seq_len(24)-.5)/24))
skewed_fit<-mixed_fit(c(rep(0,23),10))
short_fit<-mixed_fit(c(-1,1))
constant_fit<-mixed_fit(rep(0,12))
# Exercise the unavailable optional-package branch without changing packages
# or the application's function environment.
without_lmtest<-longitudinal_check_heteroskedasticity
environment(without_lmtest)<-list2env(list(requireNamespace=function(package,...) {
 if(identical(package,'lmtest'))FALSE else base::requireNamespace(package,...)
}),parent=environment(longitudinal_check_heteroskedasticity))
d<-data.frame(x=seq(.1,10,length.out=200));d$y<-2+.7*d$x+sin(seq_len(200)*2.3)
hetero<-d;hetero$y<-2+.7*d$x+sin(seq_len(200)*2.3)*d$x^2
fit<-lm(y~x,data=d)
cases<-list(
 random_short=longitudinal_check_random_effect_normality(short_fit),
 random_constant=longitudinal_check_random_effect_normality(constant_fit),
 random_normal=longitudinal_check_random_effect_normality(normal_fit),
 random_skewed=longitudinal_check_random_effect_normality(skewed_fit),
 dispersion_unavailable=longitudinal_check_overdispersion(fit,numeric()),
 dispersion_low=longitudinal_check_overdispersion(fit,residuals(fit)),
 dispersion_high=longitudinal_check_overdispersion(fit,10*residuals(fit)),
 variance_non_gaussian=longitudinal_check_heteroskedasticity(d,y~x,'poisson'),
 variance_no_package=without_lmtest(d,y~x,'gaussian'),
 variance_failed=longitudinal_check_heteroskedasticity(d,y~missing_column,'gaussian'),
 variance_equal=longitudinal_check_heteroskedasticity(d,y~x,'gaussian'),
 variance_unequal=longitudinal_check_heteroskedasticity(hetero,y~x,'gaussian'))
stopifnot(!cases$random_normal$Issue,cases$random_skewed$Issue,
 identical(cases$random_constant$Interpretation,'Random-effect normality screening could not be computed.'),
 !cases$dispersion_low$Issue,cases$dispersion_high$Issue,
 !cases$variance_equal$Issue,cases$variance_unequal$Issue)
before<-serialize(cases,NULL);missing<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(name in names(cases)) {
 original<-cases[[name]];translated<-longitudinal_appendix_table(original,lang)
 for(column in c('Check','Result','Interpretation','Recommendation')) {
  stopifnot(column %in% names(original));value<-original[[column]]
  if(lang!='en' && nzchar(value) && identical(value,unname(translated[[match(column,names(original))]])))
   missing[[length(missing)+1L]]<-data.frame(language=lang,case=name,column,english=value)
 }
 for(column in c('Statistic','p','Issue'))stopifnot(identical(original[[column]],translated[[match(column,names(original))]]))
 probe<-data.frame(Variable=original$Interpretation,Details=original$Interpretation)
 stopifnot(identical(longitudinal_appendix_table(probe,lang)[[1]],probe$Variable),identical(before,serialize(cases,NULL)))
}
if(length(missing)) {
 missing<-do.call(rbind,missing);write.csv(missing,file.path(out,'missing.csv'),row.names=FALSE,fileEncoding='UTF-8')
 cat(paste(unique(missing$english),collapse='\n'),'\n');stop('Untranslated diagnostic messages',call.=FALSE)
}
saveRDS(cases,file.path(out,'tables.rds'))
cat('PASS twelve diagnostic branches in eight languages; statistics, p values, issue flags and user identifiers unchanged\n')
