Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
rows<-read.csv('tmp/multilingual-audit/missing-translations.csv',fileEncoding='UTF-8',stringsAsFactors=FALSE)
rows<-unique(rows[rows$file=='R/setup_custom_model_canvas_i18n.R',c('english','korean')])
stopifnot(nrow(rows)>0)
catalog<-statedu_translation_table();records<-list()
for(i in seq_len(nrow(rows)))for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 key<-custom_model_canvas_translation_key(rows$english[i]);stopifnot(nzchar(key))
 full_key<-paste0('custom_model_canvas.',key)
 builtin<-custom_model_canvas_builtin_text(lang,key)
 source_kind<-if(nzchar(builtin))'builtin'else'catalog'
 value<-if(nzchar(builtin))builtin else unname(catalog[[full_key]][lang])
 if((length(value)!=1L||is.na(value)||!nzchar(value))&&lang%in%c('en','ko')) {
  value<-if(lang=='ko')rows$korean[i]else rows$english[i]
  source_kind<-'source_literal'
 }
 stopifnot(length(value)==1L,!is.na(value),nzchar(value))
 actual<-custom_model_canvas_text(lang,rows$english[i],rows$korean[i])
 stopifnot(identical(actual,value))
 placeholders<-function(x)regmatches(x,gregexpr('%(?:[0-9]+\u0024)?[sd]',x,perl=TRUE))[[1]]
 stopifnot(identical(sort(placeholders(actual)),sort(placeholders(rows$english[i]))))
 records[[length(records)+1L]]<-data.frame(english=rows$english[i],language=lang,key=full_key,source=source_kind,display=actual)
}
out<-'tmp/canvas-static-candidate-review';dir.create(out,recursive=TRUE,showWarnings=FALSE)
write.csv(do.call(rbind,records),file.path(out,'classified.csv'),row.names=FALSE,fileEncoding='UTF-8')
cat('PASS',nrow(rows),'static candidates resolved through canvas-specific keys in eight languages; placeholders preserved\n')
