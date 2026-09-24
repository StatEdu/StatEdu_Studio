Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(942);n<-40;s<-rnorm(n);d<-data.frame(id=seq_len(n),age=rnorm(n))
for(i in 1:6)d[[paste0('x',i)]]<-s+i/4+.4*d$age+rnorm(n,sd=.5)
info<-data.frame(name=names(d),measurement=c('category',rep('continuous',7)))
treatments<-c('사용자 처치 50%','Normality');times<-c('Yes','사용자 시점','Status')
long<-do.call(rbind,lapply(seq_len(n),function(i)data.frame(id=d$id[i],group=factor(rep(treatments,each=3),levels=treatments),time=factor(rep(times,2),levels=times,ordered=TRUE),score=as.numeric(d[i,paste0('x',1:6)]),age=d$age[i])))
names(long)<-c('Normality','사용자 집단','Yes','사용자 결과','age')
li<-data.frame(name=names(long),measurement=c('category','category','ordered','continuous','continuous'))
widefit<-function(cov)prepare_one_group_rm_anova_results(d,experimental_variables=paste0('x',1:3),control_variables=paste0('x',4:6),covariates=cov,variable_info=info,options=list(treatment_labels=treatments,time_labels=times))
longfit<-function(cov)prepare_one_group_rm_anova_results(long,input_format='long',id_variable=names(long)[1],group_variable=names(long)[2],time_variable=names(long)[3],outcome_variable=names(long)[4],covariates=cov,variable_info=li)
fits<-list(wide=widefit(character()),long=longfit(character()),wide_covariate=widefit('age'),long_covariate=longfit('age'))
stopifnot(identical(fits$wide$anova,fits$long$anova),identical(fits$wide_covariate$anova,fits$long_covariate$anova))
baseline<-list();exports<-list();out<-'tmp/one-group-formats-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language);tr<-function(key)statedu_t(paste0('analysis.one_group.',key),language)
 for(name in names(fits)){
  html<-as.character(one_group_rm_anova_results_ui(fits[[name]]));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  content<-lapply(main,function(t)xml2::xml_text(xml2::xml_find_all(t,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]")))
  if(language=='en')baseline[[name]]<-content
  stopifnot(length(main)>0,identical(content,baseline[[name]]))
  appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']");cells<-xml2::xml_text(xml2::xml_find_all(appendix,'.//th|.//td'))
  stopifnot(all(xml2::xml_attr(appendix,'data-result-table-language')==language),tr('within_factors') %in% cells,paste(sprintf(tr('paired_group'),treatments,as.character(n)),collapse=', ') %in% cells)
  stopifnot(all(treatments %in% cells),all(times %in% cells))
  stopifnot(tr(paste0(fits[[name]]$input_format,'_format')) %in% cells)
  if(grepl('long',name))stopifnot(paste(paste0(vapply(c('subject_id','treatment_group','time','outcome'),tr,character(1)),': ',names(long)[1:4]),collapse='; ') %in% cells)
  if(grepl('covariate',name))stopifnot('age' %in% cells)
  if(language=='ja')exports[[name]]<-html
 }
 cat('PASS:',language,'WIDE/LONG, with/without covariate; main English; localized structured summaries; original labels\n')
}
saveRDS(list(list(id='one-group-formats',title='Within-subject repeated measures',html=paste(unlist(exports),collapse='\n'))),file.path(out,'entries.rds'))
