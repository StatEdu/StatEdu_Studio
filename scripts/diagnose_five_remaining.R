Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'outputs/spss_phase43_20260907';dir.create(out,showWarnings=FALSE)
a<-'outputs/spss_phase11_20260906/gee'
plans<-jsonlite::read_json(file.path(a,'plans.json'));bad<-jsonlite::read_json(file.path(a,'not_compared.json'))
ids<-vapply(bad,`[[`,character(1),'id');original<-longitudinal_gee_adjusted;results<-list()
for(p in Filter(function(p)p$id%in%ids,plans)){
 d<-readRDS(file.path(a,'data',paste0(p$id,'.rds')))
 for(limit in c(100L,1000L)){
  if(limit==1000L&&!p$id%in%vapply(Filter(function(b)b$reason=='nonconvergence',bad),`[[`,character(1),'id'))next
  diag<-list();f<-original
  txt<-paste(deparse(body(f)),collapse='\n');txt<-gsub('1:100','1:LIMIT',txt,fixed=TRUE);txt<-gsub('LIMIT',as.character(limit),txt,fixed=TRUE)
  original_body<-parse(text=txt)[[1]]
  body(f)<-substitute({on.exit({
    diag<<-list(iter=if(exists('iter',inherits=FALSE))iter else NA_integer_,max_step=if(exists('step',inherits=FALSE))max(abs(step))else NA_real_,
       eigenvalues=if(exists('s',inherits=FALSE))eigen(s$corr,symmetric=TRUE,only.values=TRUE)$values else NULL,
       max_abs_correlation=if(exists('s',inherits=FALSE))max(abs(s$corr[row(s$corr)!=col(s$corr)]))else NA_real_,
       phi=if(exists('s',inherits=FALSE))s$phi else NA_real_)
  },add=TRUE);BODY},list(BODY=original_body))
  longitudinal_gee_adjusted<-f
  ans<-tryCatch({fit<-longitudinal_fit_model(d,'y','id','time',unlist(p$x),'gee',switch(p$family,NORMAL='gaussian',GAMMA='gamma',BINOMIAL='binomial',POISSON='poisson'),'unstructured_adjusted');list(success=TRUE,coefficients=summary(fit$model)$coefficients)},error=function(e)list(success=FALSE,error=conditionMessage(e)))
  results[[length(results)+1]]<-c(list(id=p$id,family=p$family,limit=limit,n=nrow(d),subjects=length(unique(d$id)),times=length(unique(d$time)),parameters=length(p$x)+1,y_min=min(d$y),nonpositive=sum(d$y<=0)),ans,list(diagnostics=diag))
  jsonlite::write_json(results,file.path(out,'gee_diagnostics.json'),pretty=TRUE,auto_unbox=TRUE,digits=16)
  cat(p$id,limit,ans$success,ans$error,'\n')
 }
}
longitudinal_gee_adjusted<-original
ps<-jsonlite::read_json('outputs/spss_phase3_20260906/plans.json');wp<-Filter(function(p)p$kind=='wilcoxon',ps)[[1]]
d<-readRDS(file.path('outputs/spss_phase3_20260906/data',paste0(wp$id,'.rds')))
vs<-unlist(wp$y);delta<-d[[vs[1]]]-d[[vs[2]]];delta<-delta[is.finite(delta)&delta!=0]
w<-list(id=wp$id,n_nonzero=length(delta),ties=anyDuplicated(abs(delta))>0,p_corrected=wilcox.test(delta,exact=FALSE,correct=TRUE)$p.value,p_uncorrected=wilcox.test(delta,exact=FALSE,correct=FALSE)$p.value)
jsonlite::write_json(w,file.path(out,'wilcoxon_recheck.json'),pretty=TRUE,auto_unbox=TRUE,digits=16)
