Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/longitudinal-mixed-messages-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
fit<-function(effects) {
 d<-data.frame(id=factor(rep(seq_along(effects),each=4)),x=rep(c(-1,1,-1,1),length(effects)))
 d$y<-rep(effects,each=4)+.3*d$x+rep(c(-.2,-.2,.2,.2),length(effects))
 lme4::lmer(y~x+(1|id),data=d)
}
normal<-fit(qnorm((seq_len(24)-.5)/24));singular<-fit(rep(0,12))
external<-'User x.1; tolerance=1e-08 (50%)\ncustom <&> %s'
warned<-normal;warned@optinfo$conv$lme4$messages<-external
singular_only<-singular;singular_only@optinfo$conv$lme4$messages<-NULL
prefix_external<-normal;prefix_external@optinfo$conv$lme4$messages<-paste0('singular random-effects fit; ',external)
cases<-list(converged=longitudinal_check_mixed_convergence(normal),
 singular=longitudinal_check_mixed_convergence(singular),external=longitudinal_check_mixed_convergence(warned),
 singular_only=longitudinal_check_mixed_convergence(singular_only),
 prefix_external=longitudinal_check_mixed_convergence(prefix_external))
for(slope in c(FALSE,TRUE))for(cluster in c(FALSE,TRUE)) {
 cases[[paste('structure',slope,cluster)]]<-longitudinal_check_random_effect_group('subject.id',
  'time.1',slope,if(cluster)'site.1' else NULL)
}
stopifnot(!cases$converged$Issue,cases$singular$Issue,cases$external$Issue)
labels<-c('subject.id. An additional cluster-level random intercept is grouped by fake',
 'time.1, with a random slope for user\n<&> %s','site.1')
options_only<-list(mixed_convergence=FALSE,random_effects=TRUE,random_effect_normality=FALSE,
 residual_normality=FALSE,heteroskedasticity=FALSE,serial_correlation=FALSE)
for(slope in c(FALSE,TRUE))for(cluster in c(FALSE,TRUE)) {
 assembled<-longitudinal_assumption_checks(model.frame(normal),normal,'lmm','gaussian',y~x,
  labels[1],labels[2],'exchangeable',cluster=if(cluster)labels[3] else character(),
  random_slope=slope,check_options=options_only)$checks
 displayed<-longitudinal_display_assumption_table(list(assumption_checks=assembled))
 stopifnot(length(attr(displayed,'longitudinal_structure_messages'))==1)
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
  expected<-if(slope)sprintf(statedu_t('longitudinal.mixed_message.slope',lang),labels[1],labels[2])
    else sprintf(statedu_t('longitudinal.mixed_message.intercept',lang),labels[1])
  if(cluster)expected<-paste(expected,sprintf(statedu_t('longitudinal.mixed_message.cluster',lang),labels[3]))
  stopifnot(identical(longitudinal_appendix_table(displayed,lang)[[5]],expected))
 }
 cases[[paste('assembled',slope,cluster)]]<-assembled
}
before<-serialize(cases,NULL);missing<-list()
for(lang in c('en','ko','ja','zh','es','fr','de','vi'))for(name in names(cases)) {
 original<-cases[[name]];translated<-longitudinal_appendix_table(original,lang)
 if(name=='external')stopifnot(grepl(external,translated[[5]],fixed=TRUE))
 if(name=='prefix_external')stopifnot(grepl(paste0('singular random-effects fit; ',external),translated[[5]],fixed=TRUE))
 if(name=='singular_only')stopifnot(identical(translated[[5]],sprintf(statedu_t('longitudinal.mixed_message.warning',lang),statedu_t('longitudinal.mixed_message.singular',lang))))
 if(name=='singular')stopifnot(grepl("boundary (singular) fit: see help('isSingular')",translated[[5]],fixed=TRUE))
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
cat('PASS thirteen mixed-model diagnostic cases in eight languages; assembly/display metadata, external warnings and user identifiers preserved\n')
