.libPaths(R.home('library'))
source('R/utils.R',encoding='UTF-8')
old <- new.env(parent=.GlobalEnv);sys.source('R/data_io.R',old)
old$repair_text_encoding <- function(values) {
  if(length(values)==0) return(values)
  values <- as.character(values);repaired <- values
  broken <- is.na(suppressWarnings(iconv(repaired,from='',to='UTF-8'))) |
    suppressWarnings(grepl('\uFFFD',repaired,fixed=TRUE))
  broken[is.na(values)] <- FALSE
  if(!any(broken,na.rm=TRUE)) return(values)
  pending <- which(broken)
  for(encoding in c('CP949','EUC-KR','UTF-8','latin1')) {
    converted <- suppressWarnings(iconv(values[pending],from=encoding,to='UTF-8'))
    usable <- !is.na(converted)
    if(any(usable)) {
      repaired[pending[usable]] <- converted[usable]
      pending <- pending[!usable]
    }
    if(length(pending)==0L) break
  }
  repaired
}
new <- new.env(parent=.GlobalEnv);sys.source('R/data_io.R',new)
capture <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr),error=function(e)list(error=conditionMessage(e))),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')},message=function(m){messages<<-c(messages,conditionMessage(m));invokeRestart('muffleMessage')})
  list(value=value,encoding=if(is.character(value))Encoding(value) else NULL,warnings=warnings,messages=messages,rng=.Random.seed)
}
set.seed(822)
bad <- rawToChar(as.raw(c(255,254,65)))
cp949 <- rawToChar(iconv('한글',from='UTF-8',to='CP949',toRaw=TRUE)[[1]])
pool <- c('a','한글','é','',NA,'\uFFFD',bad,cp949)
cases <- list(character(),factor(c('a',NA)),as.Date(c('2026-01-01',NA)))
for(n in c(127,1023,1024,10000)) for(mark in c('unknown','UTF-8','latin1','bytes','mixed')) {
  for(p in list(pool,'한글',c('a',NA),rep(NA_character_,2),c(bad,cp949))) {
    x <- rep(p,length.out=n)
    if(mark!='mixed') Encoding(x) <- mark
    cases[[length(cases)+1L]] <- x
    names(x) <- paste0('n',seq_along(x));cases[[length(cases)+1L]] <- x
  }
}
for(k in c(16,17,64,65,10000)) cases[[length(cases)+1L]] <- c(rep('prefix',128),rep(paste0('v',seq_len(k)),length.out=10000))
for(i in 1:50) cases[[length(cases)+1L]] <- sample(pool,2048,TRUE)
# A dispersed eligibility sample must never replace full validation.
for(n in c(1024L,10000L)) {
  sampled <- unique(c(seq_len(128L),as.integer(seq.int(1L,n,length.out=128L))))
  for(k in c(16L,17L,64L,65L,256L)) {
    x <- rep(paste0('v',seq_len(k)),length.out=n)
    x[sampled] <- 'prefix'
    cases[[length(cases)+1L]] <- x
    x[setdiff(seq_len(n),sampled)[[1L]]] <- bad
    cases[[length(cases)+1L]] <- x
  }
  x <- rep('valid',n)
  x[setdiff(seq_len(n),sampled)[[1L]]] <- cp949
  cases[[length(cases)+1L]] <- x
}
for(x in cases) {
  stopifnot(identical(capture(old$repair_text_encoding(x)),capture(new$repair_text_encoding(x)),num.eq=FALSE))
  d <- data.frame(x=x)
  stopifnot(identical(capture(old$normalize_text_encoding(d)),capture(new$normalize_text_encoding(d)),num.eq=FALSE))
}
cat('PASS:',length(cases)*2,'exact repair/data/encoding/condition/RNG comparisons.\n')

