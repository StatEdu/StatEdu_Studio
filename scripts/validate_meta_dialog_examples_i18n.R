Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
doc<-function(ui)xml2::read_html(as.character(htmltools::renderTags(ui)$html),encoding='UTF-8')
state<-function(ui)lapply(xml2::xml_find_all(doc(ui),'//input|//select|//option'),function(x){a<-xml2::xml_attrs(x);a[intersect(c('id','type','value','checked','selected','min','max','step'),names(a))]})
for(lang in c('en','ko','ja','zh','es','fr','de','vi','ko')) {
 examples<-vapply(c('categorical_example','continuous_example'),function(k)statedu_t(paste0('meta.dialog.',k),lang),character(1))
 parsed<-meta_parse_moderators(examples[[1]],examples[[2]])
 stopifnot(parsed$valid,nrow(parsed$data)==4L,identical(parsed$data$numeric_value[3:4],c(42.5,60)))
 for(family in c('g','r','or'))for(type in names(meta_input_types(family))) {
  record<-as.data.frame(c(list(study_id='Status',study_name='연구 <&> %s',outcome='Normality',predictor='Yes',publication_year=2020,included=FALSE,direction='negative',input_type=type,moderator_categorical='사용자=원문',moderator_continuous='나이=42.5'),setNames(as.list(rep(12.5,length(meta_input_field_map(family)[[type]]))),meta_input_field_map(family)[[type]])),stringsAsFactors=FALSE)
  before<-record
  for(item in list(NULL,record)) {
   modal<-meta_effect_modal(family,item,lang)
   stopifnot(identical(state(modal),state(meta_effect_modal(family,item,'en'))))
   html<-doc(modal)
   for(i in 1:2) {
    id<-c('meta_field_moderator_categorical','meta_field_moderator_continuous')[i]
    node<-xml2::xml_find_first(html,paste0('//*[@id="',id,'"]'))
    stopifnot(xml2::xml_attr(node,'placeholder')==examples[[i]])
    stopifnot(xml2::xml_attr(node,'value')==if(is.null(item))'' else c('사용자=원문','나이=42.5')[i])
   }
  }
  fields<-meta_effect_fields_ui(family,type,record,lang)
  stopifnot(identical(state(fields),state(meta_effect_fields_ui(family,type,record,'en'))),identical(record,before))
  if(family=='or'&&type=='or_ci')stopifnot(xml2::xml_text(xml2::xml_find_first(doc(fields),'//label[@for="meta_field_or_value"]'))==statedu_t('meta.dialog.odds_ratio',lang))
 }
 cat('PASS dialog:',lang,'14 formats, add/edit fields, examples parsed, values unchanged\n')
}
