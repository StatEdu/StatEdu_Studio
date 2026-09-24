Sys.setlocale('LC_CTYPE','English_United States.utf8')
source('scripts/validate_meta_input_status_i18n.R',encoding='UTF-8')
failures<-list(pairs=meta_parse_moderators(continuous='bad'),
 unique=meta_parse_moderators(categorical='a=x;a=y'),
 numeric=meta_parse_moderators(continuous='a=Inf'),
 conflict=meta_parse_moderators(categorical='a=x',continuous='a=2'))
detail_body<-NULL
walk_detail<-function(node) {
 if(missing(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('<-'))&&identical(node[[2]],quote(output$meta_validation_details)))detail_body<<-node[[3]][[2]]
 if(is.call(node)||is.expression(node))for(child in as.list(node))walk_detail(child)
}
walk_detail(parse('R/server_meta.R',encoding='UTF-8'));stopifnot(!is.null(detail_body))
raw<-'사용자 <&> %s = original';original_report<-meta_moderator_display(raw,raw,'en')
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 language<-function()lang;app_language_fn<-language
 stopifnot(meta_input_moderator_display(raw,raw,lang)==paste0(statedu_t('meta.input_review.categorical_prefix',lang),raw,' | ',statedu_t('meta.input_review.continuous_prefix',lang),raw),
   meta_input_moderator_display('','',lang)=='',meta_input_detail_text(raw,lang)==raw)
 for(key in names(failures)) {
   result<-failures[[key]];stopifnot(!result$valid,result$message==statedu_t(paste0('meta.input_error.',key),'en'))
   expected<-statedu_t(paste0('meta.input_error.',key),lang)
   stopifnot(meta_input_detail_text(result$message,lang)==expected)
   flagged<-data.frame(status=c('error','warning'),assumption=c('',raw),study_id=c('',raw),message=c(result$message,raw))
   snapshot<-flagged;current_family_effects<-function()flagged
   doc<-xml2::read_html(as.character(eval(detail_body)),encoding='UTF-8')
   stopifnot(xml2::xml_text(xml2::xml_find_first(doc,'//h4'))==statedu_t('meta.input_review.title',lang),
     xml2::xml_text(xml2::xml_find_first(doc,'//li/strong'))==paste0(statedu_t('meta.input_review.missing_id',lang),': '),
     grepl(expected,xml2::xml_text(doc),fixed=TRUE),grepl(raw,xml2::xml_text(doc),fixed=TRUE),identical(flagged,snapshot))
 }
 stopifnot(identical(meta_moderator_display(raw,raw,'en'),original_report))
 cat('PASS:',lang,'four actual parser errors; review HTML; moderator prefixes; raw user text and report helper preserved\n')
}
