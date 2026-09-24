Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
original_translate <- statedu_localized_text
seen <- character();unchanged <- character()
statedu_localized_text <- function(language,en,ko=en) {
 value <- original_translate(language,en,ko)
 seen <<- unique(c(seen,en))
 if(!language %in% c('en','ko')&&identical(value,en))unchanged <<- unique(c(unchanged,paste(language,en,sep=': ')))
 value
}
signature <- function(ui) {
 doc <- xml2::read_html(as.character(ui),encoding='UTF-8')
 nodes <- xml2::xml_find_all(doc,'//input|//select|//textarea')
 lapply(nodes,function(node) {
  attrs <- xml2::xml_attrs(node)
  attrs <- attrs[intersect(c('id','name','type','value','min','max','step','checked'),names(attrs))]
  list(tag=xml2::xml_name(node),attrs=attrs,options=xml2::xml_attr(xml2::xml_find_all(node,'.//option'),'value'))
 })
}
literal <- 'Review 사용자 <&> %s'
for(type in c('cfa','cbsem','sem','plssem')) {
 baseline <- NULL
 for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
  ui <- structural_analysis_options_panel(type,language,setNames('group_id',literal))
  if(language=='en')baseline <- signature(ui)else stopifnot(identical(signature(ui),baseline))
  prefix <- structural_analysis_prefix(type)
  values <- setNames(list(literal,literal,'group_id',TRUE),paste0(prefix,c('_common_method_procedural_controls','_common_method_marker_variable','_invariance_group','_common_method_enabled')))
  restored <- xml2::read_html(as.character(structural_canvas_restore_options(ui,values)),encoding='UTF-8')
  find <- function(suffix)xml2::xml_find_first(restored,paste0("//*[@id='",prefix,suffix,"']"))
  stopifnot(xml2::xml_attr(xml2::xml_find_first(find('_invariance_group'),'.//option[@selected]'),'value')=='group_id',
   xml2::xml_text(xml2::xml_find_first(find('_invariance_group'),'.//option[@selected]'))==literal)
  if(type!='plssem')stopifnot(xml2::xml_text(find('_common_method_procedural_controls'))==literal,
   xml2::xml_attr(find('_common_method_marker_variable'),'value')==literal,
   !is.na(xml2::xml_attr(find('_common_method_enabled'),'checked')))
  else stopifnot(inherits(find('_common_method_enabled'),'xml_missing'))
  cat('PASS:',type,language,'control IDs/values/bounds/defaults and restored user settings\n')
 }
}
# Verify real catalog entries, including legitimate words shared with English.
catalog <- statedu_translation_table()
for(source in seen)for(language in c('ja','zh','es','fr','de','vi')) {
 key <- paste0('analysis.ui.',gsub('^_+|_+$','',gsub('[^a-z0-9]+','_',tolower(trimws(source)))))
 alternatives <- names(Filter(function(row)isTRUE(unname(row['en'])==source),catalog))
 present <- any(vapply(unique(c(key,alternatives)),function(k) {
   value <- catalog[[k]][language];length(value)==1&&!is.na(value)&&nzchar(value)
 },logical(1)))
 stopifnot(present)
}
cat('PASS:',length(seen),'runtime translation sources have six additional language catalog entries\n')
cat('Shared-language spellings (not missing entries):\n',paste(unchanged,collapse='\n'),'\n')
