Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/structural-bootstrap-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=globalenv());if(file.exists('tmp/structural-bootstrap-before.R'))sys.source('tmp/structural-bootstrap-before.R',old)
contract<-function(doc)lapply(xml2::xml_find_all(doc,'//button|//input|//select|//option'),function(node){a<-xml2::xml_attrs(node);a[grepl('^(data-|id$|class$|value$|name$|checked$|selected$|type$)',names(a))]})
normalize_ids<-function(x)gsub('data-tabsetid="[0-9]+"','data-tabsetid="ID"',gsub('tab-[0-9]+-','tab-ID-',x))
for(type in c('cfa','cbsem','plssem')){
 base<-as.character(structural_analysis_options_panel(type,'en',c('사용자 집단 [g]'='g')))
 if(exists('structural_analysis_options_panel',old,inherits=FALSE))stopifnot(identical(normalize_ids(base),normalize_ids(as.character(old$structural_analysis_options_panel(type,'en',c('사용자 집단 [g]'='g'))))))
 base_doc<-xml2::read_html(base,encoding='UTF-8')
 base_panel<-xml2::xml_find_all(base_doc,'//*[contains(concat(" ",@class," ")," tab-pane ")]')[[2]]
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
  html<-as.character(structural_analysis_options_panel(type,lang,c('사용자 집단 [g]'='g')))
  doc<-xml2::read_html(html,encoding='UTF-8');stopifnot(identical(contract(doc),contract(base_doc)))
  panel<-xml2::xml_find_all(doc,'//*[contains(concat(" ",@class," ")," tab-pane ")]')[[2]]
  visible<-xml2::xml_text(panel)
  if(!lang %in% c('en','ko')){
   texts<-trimws(xml2::xml_text(xml2::xml_find_all(base_panel,'.//label|.//option|.//p|.//h5')))
   texts<-texts[nzchar(texts)&!grepl(' resamples$',texts)]
   dictionary<-jsonlite::fromJSON(file.path('i18n',paste0(lang,'.json')))$translations
   for(en in texts){
    key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(en))))
    if(is.null(dictionary[[key]]))stop(paste('Missing dictionary:',lang,en))
    if(!grepl(dictionary[[key]],visible,fixed=TRUE))stop(paste('Missing rendered translation:',lang,en))
   }
  }
  conditions<-xml2::xml_attr(xml2::xml_find_all(panel,'//*[@data-display-if]'),'data-display-if')
  stopifnot(identical(conditions,xml2::xml_attr(xml2::xml_find_all(base_panel,'//*[@data-display-if]'),'data-display-if')))
  # CI choices retain their computational values, including CFA's optional BCa.
  ci<-xml2::xml_find_all(panel,'.//select[contains(@id,"ci_method")]/option')
  stopifnot(all(xml2::xml_attr(ci,'value') %in% c('bias_corrected','percentile','bca')))
  writeLines(html,file.path(out,paste0(type,'-',lang,'.html')),useBytes=TRUE)
  cat('PASS:',type,lang,'bootstrap labels/notes/methods; IDs, values, defaults and conditional visibility preserved\n')
 }
}
