.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/category-snapshot-batch-20260915'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
sys.source('scripts/fixtures/category_snapshot_reference.R',old)
sys.source('R/data_category_labels.R',new)
for(env in list(old,new))env$apply_category_label_snapshot<-compiler::cmpfun(env$apply_category_label_snapshot)
capture<-function(env,args){
 ds<-list();stdout<-capture.output(v<-tryCatch(withCallingHandlers(do.call(env$apply_category_label_snapshot,args),warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=v,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
make_table<-function(n,pairs=11L){
 tab<-data.frame(name=if(n)paste0('v',seq_len(n))else character())
 for(column in category_label_edit_columns(pairs))tab[[column]]<-rep('',n)
 tab$reference<-rep(c(' 1 ','11','none',''),length.out=n)
 for(i in seq_len(pairs)){
  tab[[paste0('value_',i)]]<-rep(as.character(i),n)
  tab[[paste0('label_',i)]]<-rep(paste0('한글 <&',i),n)
 }
 tab
}
set.seed(20260915);checks<-0L
compare<-function(args){
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<<-seed;b<-capture(new,args)
 if(!identical(a,b,num.eq=FALSE)){
  saveRDS(list(args=args,a=a,b=b),file.path(root,'mismatch.rds'));stop('Exact snapshot mismatch')
 }
 invisible(a)
}
for(n in c(0L,1L,12L,100L))for(pairs in c(1L,3L,11L))for(kind in c('plain','duplicates','missing_reference','missing_value','missing_label','factor','numeric','named','list','matrix','unchanged','empty_reference'))for(update in c(FALSE,TRUE)){
 tab<-make_table(n,pairs)
 if(kind=='duplicates'&&pairs>1L)tab$value_2<-tab$value_1
 if(kind=='missing_reference'&&n)tab$reference[1]<-NA_character_
 if(kind=='missing_value'&&n)tab$value_1[1]<-NA_character_
 if(kind=='missing_label'&&n)tab$label_1[1]<-NA_character_
 if(kind=='factor')tab$reference_label<-factor(tab$reference_label)
 if(kind=='numeric')tab$value_1<-rep(1,n)
 if(kind=='named')names(tab$reference_label)<-tab$name
 if(kind=='list')tab$value_1<-rep(list(NULL),n)
 if(kind=='matrix')tab$value_1<-matrix(rep('1',n),ncol=1L)
 if(kind=='unchanged')tab<-old$apply_category_label_snapshot(tab,list(),max_pairs=pairs)$table
 if(kind=='empty_reference')tab$reference<-rep('',n)
 incoming<-if(update)list(v1=list(reference='1',var_label='updated'),added=list(reference='1',value_1='1',label_1='added label'))else list()
 compare(list(current=tab,incoming=incoming,max_pairs=pairs));checks<-checks+1L
}
for(pairs in list(0L,-1L,NA_integer_,c(1L,2L),1.5,'3')){
 compare(list(current=make_table(2L),incoming=list(),max_pairs=pairs));checks<-checks+1L
}
cat('Exact snapshot comparisons:',checks,'\n')
# Saving the same snapshot a second time must preserve changed and label updates as well.
tab<-make_table(12L)
first<-compare(list(current=tab,incoming=list()))$value
compare(list(current=first$table,incoming=list()))
cat('Repeated save comparison: 1\n')
utf8<-enc2utf8('\u00e9');latin1<-iconv(utf8,from='UTF-8',to='latin1')
encoding_checks<-0L
for(reference in c(utf8,latin1))for(value in c(utf8,latin1)){
 tab<-make_table(2L);tab$reference<-rep(reference,2L);tab$value_1<-rep(value,2L)
 tab$reference_label<-rep(latin1,2L);tab$label_1<-rep(utf8,2L)
 compare(list(current=tab,incoming=list()));encoding_checks<-encoding_checks+1L
}
cat('Mixed encoding comparisons:',encoding_checks,'\n')
