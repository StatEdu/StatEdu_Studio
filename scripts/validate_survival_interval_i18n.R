Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/survival-interval-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
check<-function(d,shape,roles)survival_preflight(d,list(data_shape=shape,roles=roles,event_of_interest='1'))
single<-check(data.frame(t=c('bad','-1','5'),e=c(0,1,1)),'single_record',list(time='t',event='e'))
entry<-check(data.frame(t=c(5,4,8),a=c(NA,6,0),e=c(0,1,1)),'entry_exit',list(time='t',entry='a',event='e'))
interval<-check(data.frame(id=c('a','a','b','b','c','d',NA,'e'),a=c(0,1,0,2,NA,-1,0,5),b=c(3,4,2,4,5,3,2,4),e=c(0,1,1,1,0,0,0,0)),
 'start_stop',list(start='a',stop='b',subject_id='id',event='e'))
tables<-lapply(list(single=single,entry=entry,interval=interval),function(p)survival_cox_exclusion_table(list(preflight=p)))
codes<-c('invalid_time_encoding','invalid_time','missing_entry','invalid_time_order','missing_interval','invalid_interval_time','missing_subject_id_value','overlapping_interval','interval_after_event','multiple_subject_events')
stopifnot(all(codes%in%unlist(lapply(tables,function(t)t[[1]]))))
tables$custom<-data.frame('Exclusion reason'=c('Review','사용자 <&> %s'),N=c(2,3),check.names=FALSE)
for(kind in names(tables))for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 rows<-tables[[kind]];html<-as.character(survival_simple_table(rows,table_language=language))
 doc<-xml2::read_html(html,encoding='UTF-8')
 values<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[1]')))
 counts<-trimws(xml2::xml_text(xml2::xml_find_all(doc,'//tbody/tr/td[2]')))
 stopifnot(identical(as.numeric(counts),as.numeric(rows$N)))
 known<-rows[[1]]%in%c(codes,'missing_time')
 if(language=='en'){baseline<-counts;stopifnot(identical(values,rows[[1]]))}else{
  stopifnot(identical(counts,baseline),all(values[known]!=rows[[1]][known]))
 }
 stopifnot(identical(values[!known],rows[[1]][!known]))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=html)
 cat('PASS:',kind,language,'actual preflight reasons, counts and custom text\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
