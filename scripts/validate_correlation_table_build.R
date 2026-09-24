source('R/app_bootstrap.R',encoding='UTF-8')
load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
reference<-correlation_pair_rows_and_matrices
restore<-function(expr) {
  if(is.call(expr)&&identical(expr[[1L]],as.name('list'))&&'Variable1' %in% names(expr)) {
    expr[[1L]]<-as.name('data.frame');expr$check.names<-FALSE;return(expr)
  }
  if(is.call(expr)&&identical(expr[[1L]],as.name('<-'))&&identical(expr[[2L]],as.name('pairwise_table')))
    return(quote(pairwise_table <- if(length(rows)>0) do.call(rbind,rows) else data.frame()))
  if(is.call(expr))for(i in seq_along(expr))expr[i]<-list(restore(expr[[i]]))
  expr
}
body(reference)<-restore(body(reference))
variables<-c('x','y','z')
measurements<-setNames(rep('continuous',3),variables)
pair<-function(x,y,n=30L,r=.123,p=.024,ci=c(-.1,.4)) list(x_name=x,y_name=y,result=list(
  type1='Continuous',type2='Continuous',n=n,label='Pearson',coefficient=r,p=p,ci=ci,reason='Test reason'))
sets<-list(list(),list(pair('x','y')),list(pair('x','y'),pair('x','z',n=29),pair('y','z',r=NA_real_,p=NA_real_,ci=c(NA_real_,NA_real_))))
count<-0L
for(labels in list(setNames(variables,variables),setNames(c('같은 이름','같은 이름','Third'),variables))) {
  for(digits in 0:5)for(pformat in c('apa','leading_zero'))for(pairs in sets) {
    options(statedu.output_decimal_digits=digits,statedu.p_value_format=pformat)
    before<-reference(NULL,variables,labels,measurements,pairs)
    after<-correlation_pair_rows_and_matrices(NULL,variables,labels,measurements,pairs)
    stopifnot(identical(before,after,num.eq=FALSE));count<-count+1L
  }
}
cat('PASS:',count,'exact table/matrix/column-type/format comparisons.\n')
