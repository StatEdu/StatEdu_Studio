source('output/correlation-triangle-display-20260913/common.R')
capture <- function(v,m,p,ci,kind) {
  select_variant(v);conditions <- character();calls <- numeric();set.seed(81)
  formatter <- function(x) {calls<<-c(calls,x);if(kind=='warning')warning('format warning');if(kind=='rng')runif(1);format_decimal3(x)}
  value <- withCallingHandlers(tryCatch(list(
    r=correlation_lower_matrix_display_table(m,formatter,p,TRUE),
    p=correlation_p_matrix_display_table(list(p_matrix=p,ci_matrix=ci))),
    error=function(e)list(error=conditionMessage(e))),
    warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')})
  list(value=value,conditions=conditions,calls=calls,rng=.Random.seed)
}
count <- 0L
for(nr in c(0L,1L,2L,5L,12L))for(nc in c(0L,1L,3L,5L,12L)) {
  m <- matrix(rep(c(-.5,0,.8,NA,NaN,Inf),length.out=nr*nc),nr,nc,
    dimnames=list(if(nr)paste0('row',seq_len(nr))else character(),if(nc)paste0('col',seq_len(nc))else character()))
  p <- abs(m)/10
  for(ci in list(NULL,matrix('CI',nr,nc),matrix('small',1,1)))for(kind in c('quiet','warning','rng')) {
    stopifnot(identical(capture('baseline',m,p,ci,kind),capture('current',m,p,ci,kind),num.eq=FALSE));count <- count+1L
  }
}
set.seed(82);d <- as.data.frame(matrix(rnorm(120*10),120,10));names(d)<-paste0('V',1:10)
result <- prepare_correlation_results(d,names(d),options=list(continuous_method='pearson',p_ci=TRUE,significance_levels=TRUE))
for(latent in c(FALSE,TRUE)) {
  r <- result
  if(latent) {
    mixed <- data.frame(x=d[[1]],y=d[[2]],ordinal=rep(c('low','middle','high'),40),binary=rep(c('no','yes'),60))
    info <- data.frame(name=names(mixed),measurement=c('continuous','continuous','ordered','binary'))
    r <- prepare_correlation_results(mixed,names(mixed),info,
      options=list(continuous_method='auto',latent_correlations=TRUE,p_ci=TRUE,significance_levels=TRUE))
    stopifnot(is.matrix(r$correlation_matrix))
  }
  outputs <- list()
  for(v in names(variants)) {
    select_variant(v)
    path <- file.path(root,paste0(v,'-',latent,'.html'));write_correlation_results_html(r,path)
    excel <- file.path(root,paste0(v,'-',latent,'.xlsx'));save_correlation_excel_file(r,excel)
    sheets <- openxlsx::getSheetNames(excel)
    outputs[[v]] <- list(screen=htmltools::renderTags(correlation_results_ui(r)),
      html=readBin(path,'raw',n=file.info(path)$size),sheets=sheets,
      cells=lapply(sheets,function(s)openxlsx::read.xlsx(excel,sheet=s)))
  }
  stopifnot(identical(outputs$baseline,outputs$current,num.eq=FALSE))
}
select_variant('current')
cat('PASS:',count,'rectangular/empty/nonfinite/CI-error/order/condition/RNG comparisons and complete screen/HTML/Excel comparisons.\n')
