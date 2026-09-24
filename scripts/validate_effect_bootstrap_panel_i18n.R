Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
find_render<-function(x){
 if(missing(x)||!is.call(x))return(NULL)
 if(identical(x[[1]],as.name('<-'))&&grepl('"_result_effect_bootstrap"',paste(deparse(x[[2]]),collapse=''),fixed=TRUE))return(x[[3]][[2]])
 for(item in as.list(x)[-1]){r<-find_render(item);if(!is.null(r))return(r)}
 NULL
}
expr<-find_render(body(structural_canvas_register_result_outputs));stopifnot(!is.null(expr))
options(statedu.output_decimal_digits=3L)
out<-'tmp/effect-bootstrap-panel-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);entries<-list()
raw<-'Review 사용자 <&> %s'
blocked<-eval(body(structural_canvas_effect_bootstrap_blocked_text)[[2]],new.env(parent=globalenv()))
cases<-c('percentile','bias_corrected','legacy','paths_only','pending','canceled','error','empty','off',paste0('blocked',1:3),'blocked_detail','blocked_custom')
for(kind in cases)for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 method<-if(kind=='percentile')'percentile'else'bias_corrected'
 result<-data.frame(lhs=c('Review','Normality',raw),rhs=c(raw,'Primary','Scoring'),op=c('modmed','modmed','modmed'),estimate=c(.123,.234,.345),lower=c(.001,NA,.012),upper=c(.456,NA,.567),p=c(.023,NA,.004),beta_status=c('Not reported: product-indicator index is scale-dependent','Not available - insufficient valid bootstrap replicates','Estimated'),valid=c(95L,75L,20L),requested=100L,valid_percent=c(95,75,20),status=c('Adequate','Caution','Unreliable'),ci_method=method,quantile_type=7L,check.names=FALSE)
 if(kind=='legacy'){result$beta_status<-NULL;result$quantile_type<-NULL}
 if(kind=='paths_only')result$op<-rep('~',3)
 bundle<-list(effect_bootstrap=100L,effect_bootstrap_result=result,effect_bootstrap_ci_method=method,effect_bootstrap_seed=20260917)
 if(kind=='pending')bundle$effect_bootstrap_pending<-TRUE
 if(kind=='canceled')bundle$effect_bootstrap_canceled<-TRUE
 if(kind=='error')bundle$effect_bootstrap_error<-raw
 if(kind=='empty')bundle$effect_bootstrap_result<-result[FALSE,]
 if(kind=='off')bundle$effect_bootstrap<-0L
 if(grepl('^blocked[123]$',kind))bundle$effect_bootstrap_blocked_reason<-names(blocked)[as.integer(sub('blocked','',kind))]
 if(kind=='blocked_detail')bundle$effect_bootstrap_blocked_reason<-paste0(names(blocked)[3],' Reasons: ',raw)
 if(kind=='blocked_custom')bundle$effect_bootstrap_blocked_reason<-raw
 env<-new.env(parent=globalenv());env$fit_result<-function()bundle;env$ui_language<-function()language;env$app_language_fn<-function()language
 render<-function()NULL;body(render)<-expr;environment(render)<-env
 ui<-render();if(kind=='off'){stopifnot(is.null(ui));next}
 html<-as.character(ui);doc<-xml2::read_html(html,encoding='UTF-8');notes<-xml2::xml_text(xml2::xml_find_all(doc,'//p'));headings<-xml2::xml_text(xml2::xml_find_all(doc,'//h5'))
 if(language=='en'){en_notes<-notes;en_headings<-headings}else if(kind!='blocked_custom'){
 stopifnot(!identical(notes,en_notes));if(length(headings))stopifnot(all(headings!=en_headings))
 }
 if(kind%in%c('error','blocked_detail','blocked_custom'))stopifnot(grepl(raw,paste(notes,collapse=' '),fixed=TRUE))
 if(kind%in%c('percentile','bias_corrected','legacy','paths_only')){
  stopifnot(grepl('20260917',paste(notes,collapse=' '),fixed=TRUE),grepl('95%',paste(notes,collapse=' '),fixed=TRUE),grepl('80%',paste(notes,collapse=' '),fixed=TRUE))
  tables<-xml2::xml_find_all(doc,'//table');stopifnot(length(tables)==if(kind=='paths_only')1L else 2L)
  for(i in seq_along(tables)){
   table<-tables[[i]];headers<-xml2::xml_text(xml2::xml_find_all(table,'./thead//th'))
   if(language!='en')stopifnot(!any(c('Quantile type','Indirect path','Standardized index')%in%headers))
   numeric_columns<-if(i==1)c(2,3,4,5)else c(3,4,5,6,8,9,10)
   cells<-lapply(seq_along(headers),function(j)trimws(xml2::xml_text(xml2::xml_find_all(table,paste0('./tbody/tr/td[',j,']')))))
   if(language=='en'){
    if(i==1)english_diag<-cells else english_mod<-cells
   }else{
    english<-if(i==1)english_diag else english_mod
    for(j in numeric_columns)stopifnot(identical(cells[[j]],english[[j]]))
    stopifnot(all(cells[[length(headers)]]!=english[[length(headers)]]))
   }
   if(i==2)stopifnot(identical(cells[[1]],result$lhs),identical(cells[[2]],result$rhs))
  }
 }
 options(statedu.app_language=language)
 main<-as.character(structural_canvas_measurement_html_table(data.frame(Latent='Review',Indicator=raw,B='.700',SE='.050',beta='.720',z='14.000',p='<.001',check.names=FALSE)))
 if(language=='en')en_main<-main else stopifnot(identical(main,en_main))
 if(language=='ja')entries[[kind]]<-list(id=kind,title=kind,html=paste(main,html))
 cat('PASS:',kind,language,'status, notes, values, path names and English main table\n')
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
