.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules()
root<-'output/category-label-role-20260915'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=.GlobalEnv);new<-new.env(parent=.GlobalEnv)
sys.source('scripts/fixtures/category_label_role_reference.R',old)
sys.source('R/data_category_labels.R',new)
for(env in list(old,new)) env$category_label_display_data<-compiler::cmpfun(env$category_label_display_data)
capture<-function(env,args){
 ds<-list();stdout<-capture.output(v<-tryCatch(withCallingHandlers(do.call(env$category_label_display_data,args),warning=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleWarning')},message=function(e){ds[[length(ds)+1L]]<<-list(class(e),conditionMessage(e));invokeRestart('muffleMessage')}),error=function(e)list(error=conditionMessage(e),class=class(e))))
 list(value=v,diagnostics=ds,stdout=stdout,rng=.Random.seed)
}
make_info<-function(n){data.frame(source_order=seq_len(n),name=if(n)paste0('v',seq_len(n))else character(),var_label=if(n)paste('Variable',seq_len(n))else character(),measurement=rep(c('binary','category','ordered','continuous'),length.out=n),n_unique=rep(3L,n))}
set.seed(20260915);checks<-0L
for(n in c(0L,1L,12L,100L))for(kind in c('none','all','partial','duplicate','missing','factor','numeric','named','attribute','saved_factor','list','matrix','no_match','empty_saved','full'))for(pairs in c(1L,3L,11L)){
 info<-make_info(n);saved<-data.frame(name=rev(info$name),reference=rep('2',n),var_label=if(n)paste0('<&',rev(info$name))else character(),value_1=rep('1',n),label_1=rep('첫째',n))
 if(kind=='none')saved<-NULL
 if(kind=='partial')saved<-saved[seq_len(n)%in%c(1L,3L),c('name','reference'),drop=FALSE]
 if(kind=='duplicate'&&n>1L){saved$name[2]<-saved$name[1];info$name[2]<-info$name[1]}
 if(kind=='missing'&&n){saved$name[1]<-NA;info$name[1]<-NA;saved$reference[1]<-NA}
 if(kind=='factor'){saved$name<-factor(saved$name);info$name<-factor(info$name);info$var_label<-factor(info$var_label)}
 if(kind=='numeric'){saved$name<-rev(seq_len(n));info$name<-seq_len(n)}
 if(kind=='named'){names(saved$reference)<-saved$name;names(info$var_label)<-info$name}
 if(kind=='attribute')attr(saved$reference,'description')<-'labels'
 if(kind=='saved_factor')saved$reference<-factor(saved$reference)
 if(kind=='list')saved$reference<-rep(list(NULL),n)
 if(kind=='matrix')saved$reference<-matrix(rep('2',n),ncol=1L)
 if(kind=='no_match'&&n)saved$name<-paste0('other_',saved$name)
 if(kind=='empty_saved')saved<-saved[FALSE,,drop=FALSE]
 if(kind=='full')for(column in category_label_edit_columns(pairs)){
   saved[[column]]<-rep(c('한글 <&"',NA_character_,''),length.out=n)
   info[[column]]<-rep(c('original',''),length.out=n)
 }
 args<-list(info=info,selected_names=info$name,saved_values=saved,max_pairs=pairs,dependent=head(info$name,1),independent=tail(info$name,1))
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 if(!identical(a,b,num.eq=FALSE)){saveRDS(list(args=args,a=a,b=b),file.path(root,'mismatch.rds'));stop('Exact mismatch')};checks<-checks+1L
}
cat('Exact comparisons:',checks,'\n')
role_checks <- 0L
for(n in c(0L,1L,30L))for(kind in c('plain','named','factor','numeric'))for(assignment in c('empty','overlap','missing','factor','numeric')){
 info<-make_info(n)
 if(kind=='named')names(info$name)<-rev(info$name)
 if(kind=='factor')info$name<-factor(info$name)
 if(kind=='numeric')info$name<-seq_len(n)
 dep<-ind<-ctrl<-character()
 if(assignment=='overlap'){dep<-head(as.character(info$name),n%/%2L);ind<-as.character(info$name);ctrl<-rev(ind)}
 if(assignment=='missing'){dep<-c(NA_character_,'v1');ind<-c('v2','unknown');ctrl<-c('v1','v2')}
 if(assignment=='factor'){dep<-factor('v1');ind<-factor('v2');ctrl<-factor('v3')}
 if(assignment=='numeric'){dep<-1;ind<-2;ctrl<-3}
 args<-list(info=info,selected_names=info$name,dependent=dep,independent=ind,controls=ctrl)
 seed<-.Random.seed;a<-capture(old,args);.Random.seed<-seed;b<-capture(new,args)
 stopifnot(identical(a,b,num.eq=FALSE));role_checks<-role_checks+1L
}
cat('Role assignment table comparisons:',role_checks,'\n')
for(language in c('en','ko')) {
 info<-make_info(12L)
 saved<-data.frame(name=rev(info$name),reference='1',var_label=paste0('<&',rev(info$name)),value_1='1',label_1='첫째')
 render<-function(env){
  tab<-env$category_label_display_data(info,selected_names=info$name,saved_values=saved)
  defs<-category_label_column_defs(tab,language=language)
  widget<-DT::datatable(tab,rownames=FALSE,colnames=data_table_colnames(names(tab),language),escape=FALSE,filter='top',selection='none',options=category_label_table_options(defs,language),callback=category_label_table_callback(language))
  widget$elementId<-'category-label-comparison'
  as.character(htmltools::renderTags(widget)$html)
 }
 stopifnot(identical(render(old),render(new),num.eq=FALSE))
}
cat('Fixed-ID category table HTML comparisons: 2\n')

