source('R/utils.R',encoding='UTF-8')
source('R/analysis_correlation.R',encoding='UTF-8')
reference <- correlation_method_for_pair
original <- paste(deparse(body(reference),width.cutoff=500L),collapse='\n')
original <- sub('label = label','label = tools::toTitleCase(method)',original,fixed=TRUE)
original <- sub('sprintf("%s was selected for two continuous variables.", label)',
  'sprintf("%s was selected for two continuous variables.", tools::toTitleCase(method))',original,fixed=TRUE)
body(reference) <- parse(text=original)[[1L]]
count<-0L
for(x in c('continuous','binary','ordered','category'))for(y in c('continuous','binary','ordered','category')) {
  for(method in c('auto','pearson','spearman','kendall','unknown','PEARSON'))for(normal in c(FALSE,TRUE)) {
    table<-data.frame(Name=c('x','y'),normal=c(normal,normal))
    args<-list(x_measure=x,y_measure=y,continuous_method=method,x_name='x',y_name='y',normality_table=table,normality_checked=TRUE)
    stopifnot(identical(do.call(reference,args),do.call(correlation_method_for_pair,args),num.eq=FALSE))
    count<-count+1L
  }
}
cat('PASS:',count,'exact method-label/reason comparisons.\n')
