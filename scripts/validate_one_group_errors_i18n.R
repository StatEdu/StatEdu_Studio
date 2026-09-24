Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
phrases<-jsonlite::read_json('scripts/fixtures/one_group_errors.json',simplifyVector=TRUE)
d<-data.frame(x1=1:4,x2=2:5,x3=3:6,x4=4:7)
long<-expand.grid(id=c('사용자 50% for subject A','Normality','Yes'),group=c('Normality','사용자 처치'),time=c('Yes','Status'),stringsAsFactors=FALSE)
long$score<-seq_len(nrow(long));long[['공변량 for subject 50%']]<-rep(1, nrow(long))
covname<-'공변량 for subject 50%';missingname<-'Normality 50% 사용자'
badcov<-long;badcov[[covname]][4]<-2
badkeys<-long;badkeys$id<-NA_character_
calls<-list(
 covariates_missing=quote(one_group_rm_prepare_covariate_frame(d,missingname)),
 covariate_missing=quote(one_group_rm_long_covariate_frame(long,long$id,unique(long$id),missingname)),
 covariate_varies=quote(one_group_rm_long_covariate_frame(badcov,badcov$id,unique(badcov$id),covname)),
 long_missing=quote(one_group_rm_long_input(long,'id','group','time',missingname)),
 long_keys=quote(one_group_rm_long_input(badkeys,'id','group','time','score')),
 duplicate_keys=quote(one_group_rm_long_input(rbind(long,long[1,]),'id','group','time','score')),
 two_groups=quote(one_group_rm_long_input(long[long$group=='Normality',],'id','group','time','score')),
 two_times=quote(one_group_rm_long_input(long[long$time=='Yes',],'id','group','time','score')),
 select_group=quote(one_group_rm_long_to_wide(long,'id','group','time')),
 wide_times=quote(one_group_rm_wide_input(d,'x1',c('x3','x4'))),
 wide_equal=quote(one_group_rm_wide_input(d,c('x1','x2'),c('x2','x3','x4'))),
 wide_overlap=quote(one_group_rm_wide_input(d,c('x1','x2'),c('x2','x3'))),
 wide_covariate=quote(one_group_rm_wide_input(d,c('x1','x2'),c('x3','x4'),'x1')),
 wide_missing=quote(one_group_rm_wide_input(d,c('x1',missingname),c('x3','x4'))),
 long_roles=quote(prepare_one_group_rm_anova_results(long,input_format='long')),
 distinct_roles=quote(prepare_one_group_rm_anova_results(long,input_format='long',id_variable='id',group_variable='group',time_variable='time',outcome_variable='time')),
 separate_blocks=quote(prepare_one_group_rm_anova_results(d,repeated_variables=c('x1','x2'))),
 complete_subjects=quote(prepare_one_group_rm_anova_results(d[1:2,],experimental_variables=c('x1','x2'),control_variables=c('x3','x4'))),
 identical_values=quote(prepare_one_group_rm_anova_results(data.frame(x1=1:4,x2=1:4,x3=1:4,x4=1:4),experimental_variables=c('x1','x2'),control_variables=c('x3','x4')))
)
errors<-lapply(calls,function(expr)tryCatch(eval(expr),error=identity))
for(key in names(errors))stopifnot(inherits(errors[[key]],'one_group_rm_input_error'),identical(errors[[key]]$key,key))
stopifnot(identical(errors$covariate_varies$args,list(covname,long$id[1])))
stopifnot(grepl(long$id[1],errors$duplicate_keys$args[[1]],fixed=TRUE))
# Missing car is checked as a typed condition fixture; installed packages are untouched.
errors$car_required<-tryCatch(one_group_rm_abort('car_required'),error=identity)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 for(key in names(errors)){
  e<-errors[[key]];template<-statedu_t(paste0('analysis.one_group.error.',key),language)
  expected<-do.call(sprintf,c(list(template),e$args));actual<-one_group_rm_error_ui_text(e,language)
  stopifnot(identical(actual,expected),identical(conditionMessage(e),do.call(sprintf,c(list(phrases[[key]]),e$args))))
  for(arg in e$args)stopifnot(grepl(arg,actual,fixed=TRUE))
  if(language!='en')stopifnot(template!=phrases[[key]])
 }
 for(key in c('wide_blocks','long_blocks','completed')){
  text<-statedu_t(paste0('analysis.one_group.error.',key),language)
  stopifnot(nzchar(text));if(language!='en')stopifnot(text!=phrases[[key]])
 }
 stopifnot(identical(one_group_rm_error_ui_text(simpleError('Unmapped external error 50%'),language),'Unmapped external error 50%'))
 cat('PASS:',language,'19 actual input errors, car condition fixture, 3 notification keys; original parameters and English engine messages\n')
}
server<-paste(readLines('R/server_one_group_rm_anova.R',encoding='UTF-8'),collapse='\n')
stopifnot(grepl('one_group_rm_error_ui_text(e, statedu_current_language(app_language_fn))',server,fixed=TRUE))
for(key in c('wide_blocks','long_blocks','completed'))stopifnot(grepl(paste0('analysis.one_group.error.',key),server,fixed=TRUE))
