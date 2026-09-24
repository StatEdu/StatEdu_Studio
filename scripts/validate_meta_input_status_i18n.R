Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
summary_body<-NULL;selector_body<-NULL
walk<-function(node) {
 if(missing(node))return()
 if(is.call(node)&&identical(node[[1]],as.name('<-'))&&identical(node[[2]],quote(output$meta_validation_summary)))summary_body<<-node[[3]][[2]]
 if(is.call(node)&&identical(node[[1]],as.name('<-'))&&identical(node[[2]],quote(output$meta_moderator_selector)))selector_body<<-node[[3]][[2]]
 if(is.call(node)||is.expression(node))for(child in as.list(node))walk(child)
}
walk(parse('R/server_meta.R',encoding='UTF-8'));stopifnot(!is.null(summary_body))
rows<-meta_empty_effects()[rep(NA_integer_,3),,drop=FALSE]
for(name in names(rows)) {
 if(is.character(rows[[name]]))rows[[name]]<-rep('',3)
 if(is.numeric(rows[[name]]))rows[[name]]<-rep(0,3)
 if(is.logical(rows[[name]]))rows[[name]]<-rep(TRUE,3)
}
rows$family<-'g';rows$status<-c('valid','warning','error');rows$row_id<-c(11L,22L,33L)
rows$study_name<-c('사용자 <&> %s','Study','研究');rows$input_type<-'g_se'
before<-rows
effects<-function()rows;current_family<-function()'g'
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 language<-function()lang;app_language_fn<-language
 table<-meta_effect_display_table(rows,'g',lang)
 columns<-c('include','study_id','study_name','year','outcome','predictor','moderators','format','values','effect','se','status')
 expected_headers<-vapply(columns,function(k)if(k=='effect')"Hedges' g" else statedu_t(paste0('meta.input_column.',k),lang),character(1))
 stopifnot(identical(names(table),unname(expected_headers)))
 for(family in c('r','or')) {
   other<-rows;other$family<-family
   stopifnot(names(meta_effect_display_table(other,family,lang))[10]==if(family=='r')'r' else 'OR')
 }
 stopifnot(names(meta_effect_display_table(meta_empty_effects(),'g',lang))==statedu_t('meta.input_column.message',lang))
 expected<-vapply(rows$status,function(s)statedu_t(paste0('meta.input_status.',s),lang),character(1))
 stopifnot(identical(unname(table[[12]]),unname(expected)),identical(table[[3]],rows$study_name),
   identical(attr(table,'meta_row_ids'),rows$row_id),identical(rows,before),meta_status_label('custom %s',lang)=='custom %s')
 for(counts in list(c(3,1,1,1),c(0,0,0,0))) {
   meta_effect_summary<-function(...)list(total=counts[1],valid=counts[2],warnings=counts[3],errors=counts[4])
   html<-as.character(eval(summary_body))
   doc<-xml2::read_html(html,encoding='UTF-8')
   labels<-xml2::xml_text(xml2::xml_find_all(doc,'//strong'))
   expected_labels<-vapply(c('total','ready','warnings','errors'),function(k)statedu_t(paste0('meta.input_summary.',k),lang),character(1))
   stopifnot(identical(labels,unname(expected_labels)))
   values<-xml2::xml_text(xml2::xml_find_all(doc,'//p/text()'))
   values<-values[nzchar(trimws(values))]
   stopifnot(identical(as.numeric(trimws(values)),counts))
 }
 catalog<-data.frame(name=c('publication_year','사용자 <&> %s'),type=c('continuous','categorical'),available=c(3L,2L),key=c('year_key','user_key'))
 meta_moderator_catalog<-function(...)catalog
 input<-list(meta_moderator_selection='user_key')
 selector<-xml2::read_html(as.character(eval(selector_body)),encoding='UTF-8')
 opts<-xml2::xml_find_all(selector,'//option')
 stopifnot(identical(tail(xml2::xml_attr(opts,'value'),2),catalog$key),
   identical(tail(xml2::xml_text(opts),2),c(paste0(statedu_t('meta.input_column.year',lang),' [',statedu_t('meta.input_type.continuous',lang),', k=3]'),
    paste0(catalog$name[2],' [',statedu_t('meta.input_type.categorical',lang),', k=2]'))),
   xml2::xml_attr(xml2::xml_find_first(selector,'//option[@selected]'),'value')=='user_key')
 cat('PASS:',lang,'input-table statuses; rendered summary; row IDs/user text/source preserved\n')
}
