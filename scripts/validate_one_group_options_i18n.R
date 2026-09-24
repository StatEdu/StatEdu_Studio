Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(943);n<-48;s<-rnorm(n);d<-data.frame(id=seq_len(n),age=rnorm(n),category=factor(rep(c('Yes','사용자 범주'),24)))
for(i in 1:6)d[[paste0('x',i)]]<-s+i/4+.4*d$age+.2*as.numeric(d$category)+rnorm(n,sd=.6)
info<-data.frame(name=names(d),measurement=c('category','continuous','category',rep('continuous',6)),var_label=c('ID','사용자 공변량','Normality',paste0('x',1:6)))
treatments<-c('사용자 처치 50%','Normality');times<-c('Yes','사용자 시점','Status')
long<-do.call(rbind,lapply(seq_len(n),function(i)data.frame(id=d$id[i],group=factor(rep(treatments,each=3),levels=treatments),time=factor(rep(times,2),levels=times,ordered=TRUE),score=as.numeric(d[i,paste0('x',1:6)]),age=d$age[i],category=d$category[i])))
li<-data.frame(name=names(long),measurement=c('category','category','ordered','continuous','continuous','category'),var_label=c('ID','사용자 집단','Time','Score','사용자 공변량','Normality'))
widefit<-function(cov,k=3,extra=list())prepare_one_group_rm_anova_results(d,experimental_variables=paste0('x',seq_len(k)),control_variables=paste0('x',3+seq_len(k)),covariates=cov,variable_info=info,options=modifyList(list(treatment_labels=treatments,time_labels=times[seq_len(k)]),extra))
longfit<-function(cov)prepare_one_group_rm_anova_results(long,input_format='long',id_variable='id',group_variable='group',time_variable='time',outcome_variable='score',covariates=cov,variable_info=li)
fits<-list(wide_category=widefit('category'),long_category=longfit('category'),wide_multiple=widefit(c('age','category')),long_multiple=longfit(c('age','category')),two_times=widefit(c('age','category'),2,list(posthoc_adjustment='bonferroni')),minimal=widefit(c('age','category'),extra=list(posthoc=FALSE,assumption_check=FALSE,mean_sd=FALSE)))
stopifnot(identical(fits$wide_category$anova,fits$long_category$anova),identical(fits$wide_multiple$anova,fits$long_multiple$anova))
stopifnot(nrow(fits$minimal$posthoc)==0,nrow(fits$minimal$normality)==0,nrow(fits$minimal$assumption)==0,grepl('Bonferroni',fits$two_times$posthoc_note,fixed=TRUE))
baseline<-list();out<-'tmp/one-group-options-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);remaining<-character()
for(language in c('en','ko','ja','zh','es','fr','de','vi','ko','en')){
 options(statedu.app_language=language)
 for(name in names(fits)){
  fit<-fits[[name]];html<-as.character(one_group_rm_anova_results_ui(fit));doc<-xml2::read_html(html,encoding='UTF-8')
  main<-xml2::xml_find_all(doc,"//table[@data-result-table-role='main']")
  content<-lapply(main,function(t)xml2::xml_text(xml2::xml_find_all(t,".//th|.//td|preceding::h3[1]|ancestor::div[@data-result-table-sheet='true'][1]/*[not(self::table)]")))
  if(is.null(baseline[[name]]))baseline[[name]]<-content
  stopifnot(length(main)>0,identical(content,baseline[[name]]),all(xml2::xml_attr(main,'data-result-table-language')=='en'))
  appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']");cells<-xml2::xml_text(xml2::xml_find_all(appendix,'.//th|.//td'))
  stopifnot(all(xml2::xml_attr(appendix,'data-result-table-language')==language))
  covlabel<-fit$overview$Value[fit$overview$Item=='Covariates'];stopifnot(covlabel %in% cells)
  stopifnot(statedu_t('analysis.one_group.within_factors',language) %in% cells)
  if(name!='minimal')stopifnot(all(treatments %in% cells),all(fit$time_labels %in% cells))
  if(name=='two_times'){
   stopifnot(statedu_t('analysis.ui.only_two_repeated_time_points_were_selected',language) %in% cells)
   if(language=='ja')saveRDS(list(list(id='one-group-two-times',title='Within-subject repeated measures',html=html)),file.path(out,'entries.rds'))
  }
  if(language=='ja')remaining<-c(remaining,cells[grepl('[A-Za-z]{3}',cells)])
 }
 cat('PASS:',language,'6 actual categorical/multiple covariate and option paths; English main; original covariate labels; renderer language return\n')
}
writeLines(unique(remaining),file.path(out,'japanese-latin-cells.txt'),useBytes=TRUE)
