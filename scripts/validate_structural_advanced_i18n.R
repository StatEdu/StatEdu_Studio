Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/structural-advanced-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=globalenv());if(file.exists('tmp/structural-advanced-before.R'))sys.source('tmp/structural-advanced-before.R',old)
contract<-function(doc)lapply(xml2::xml_find_all(doc,'//button|//input|//select|//option|//textarea'),function(node){a<-xml2::xml_attrs(node);a[grepl('^(data-|id$|class$|value$|name$|checked$|selected$|type$|rows$)',names(a))]})
normalize_ids<-function(x)gsub('data-tabsetid="[0-9]+"','data-tabsetid="ID"',gsub('tab-[0-9]+-','tab-ID-',x))
for(type in c('cfa','cbsem','plssem')){
 base<-as.character(structural_analysis_options_panel(type,'en',c('사용자 집단 [g]'='g')))
 if(exists('structural_analysis_options_panel',old,inherits=FALSE))stopifnot(identical(normalize_ids(base),normalize_ids(as.character(old$structural_analysis_options_panel(type,'en',c('사용자 집단 [g]'='g'))))))
 base_doc<-xml2::read_html(base,encoding='UTF-8')
 base_panel<-xml2::xml_find_all(base_doc,'//*[contains(concat(" ",@class," ")," tab-pane ")]')[[3]]
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
  html<-as.character(structural_analysis_options_panel(type,lang,c('사용자 집단 [g]'='g')))
  doc<-xml2::read_html(html,encoding='UTF-8');stopifnot(identical(contract(doc),contract(base_doc)))
  panel<-xml2::xml_find_all(doc,'//*[contains(concat(" ",@class," ")," tab-pane ")]')[[3]]
  visible<-paste(xml2::xml_text(panel),paste(xml2::xml_attr(xml2::xml_find_all(panel,'.//*[@placeholder]'),'placeholder'),collapse=' '))
  if(!lang %in% c('en','ko')){
   texts<-c(trimws(xml2::xml_text(xml2::xml_find_all(base_panel,'.//label|.//option|.//p|.//h5'))),xml2::xml_attr(xml2::xml_find_all(base_panel,'.//*[@placeholder]'),'placeholder'))
   texts<-texts[nzchar(texts)&!grepl('^[0-9]+% CI$',texts)]
   dictionary<-jsonlite::fromJSON(file.path('i18n',paste0(lang,'.json')))$translations
   for(en in texts){
    key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(en))))
    if(is.null(dictionary[[key]]))stop(paste('Missing dictionary:',lang,en))
    if(!grepl(dictionary[[key]],visible,fixed=TRUE))stop(paste('Missing translation:',lang,en))
   }
  }
  stopifnot(identical(xml2::xml_attr(xml2::xml_find_all(panel,'.//*[@data-display-if]'),'data-display-if'),xml2::xml_attr(xml2::xml_find_all(base_panel,'.//*[@data-display-if]'),'data-display-if')))
  writeLines(html,file.path(out,paste0(type,'-',lang,'.html')),useBytes=TRUE)
  cat('PASS:',type,lang,'advanced labels/help/choices; input values, defaults and conditional rules preserved\n')
 }
}
