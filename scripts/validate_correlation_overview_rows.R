source('output/correlation-overview-rows-20260914/common.R')
capture <- function(fn,m) {
  conditions <- character();set.seed(901)
  value <- withCallingHandlers(tryCatch(fn(list(method_matrix=m)),error=function(e)list(error=conditionMessage(e))),
    warning=function(w){conditions<<-c(conditions,conditionMessage(w));invokeRestart('muffleWarning')})
  list(value=value,conditions=conditions,rng=.Random.seed)
}
count <- 0L
for(nr in c(0L,1L,2L,5L,20L))for(nc in c(0L,1L,3L,5L,20L))for(kind in c('methods','blank','duplicate')) {
  values <- if(kind=='blank')'' else c('Pearson','Spearman','Kendall','polychoric','unknown','',NA_character_)
  labels <- if(nc)if(kind=='duplicate')rep('same',nc)else paste0('V',1:nc)else character()
  m <- matrix(rep(values,length.out=nr*nc),nr,nc,
    dimnames=list(if(nr)paste0('R',1:nr)else character(),labels))
  stopifnot(identical(capture(baseline,m),capture(current,m),num.eq=FALSE));count <- count+1L
}
set.seed(902);d <- as.data.frame(matrix(rnorm(120*10),120,10));names(d)<-paste0('V',1:10)
for(latent in c(FALSE,TRUE)) {
  data <- d; info <- NULL
  if(latent) {
    data <- data.frame(x=d[[1]],y=d[[2]],ordinal=rep(c('low','middle','high'),40),binary=rep(c('no','yes'),60))
    info <- data.frame(name=names(data),measurement=c('continuous','continuous','ordered','binary'))
  }
  r <- prepare_correlation_results(data,names(data),info,options=list(continuous_method='auto',latent_correlations=latent,p_ci=TRUE,significance_levels=TRUE))
  outputs <- list()
  for(v in c('baseline','current')) {
    select_variant(v)
    path <- file.path(root,paste0(v,'-',latent,'.html'));write_correlation_results_html(r,path)
    excel <- file.path(root,paste0(v,'-',latent,'.xlsx'));save_correlation_excel_file(r,excel)
    sheets <- openxlsx::getSheetNames(excel)
    html <- paste(readLines(path,warn=FALSE,encoding='UTF-8'),collapse='\n')
    outputs[[v]] <- list(screen=htmltools::renderTags(correlation_results_ui(r)),
      html=readBin(path,'raw',n=file.info(path)$size),accumulated=result_entry_tables(list(title='Correlation',html=html),1L),
      sheets=sheets,cells=lapply(sheets,function(s)openxlsx::read.xlsx(excel,sheet=s)))
  }
  stopifnot(identical(outputs$baseline,outputs$current,num.eq=FALSE))
}
select_variant('current')
cat('PASS:',count,'complete table/attribute/condition/RNG comparisons; observed/latent-option screens, saved HTML, accumulated tables and Excel cells identical.\n')
