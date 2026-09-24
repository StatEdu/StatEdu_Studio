Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/structural-estimation-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
old<-new.env(parent=globalenv());if(file.exists('tmp/structural-options-before.R'))sys.source('tmp/structural-options-before.R',old)
contract<-function(doc)lapply(xml2::xml_find_all(doc,'//button|//input|//select|//option'),function(node){a<-xml2::xml_attrs(node);a[grepl('^(data-|id$|class$|value$|name$|checked$|selected$|type$)',names(a))]})
normalize_ids<-function(x)gsub('data-tabsetid="[0-9]+"','data-tabsetid="ID"',gsub('tab-[0-9]+-','tab-ID-',x))
for(type in c('cfa','cbsem','plssem')){
 base<-as.character(structural_analysis_options_panel(type,'en',c('사용자 집단 [g]'='g')))
 if(exists('structural_analysis_options_panel',old,inherits=FALSE))stopifnot(identical(normalize_ids(base),normalize_ids(as.character(old$structural_analysis_options_panel(type,'en',c('사용자 집단 [g]'='g'))))))
 base_doc<-xml2::read_html(base,encoding='UTF-8')
 for(lang in c('en','ko','ja','zh','es','fr','de','vi')){
  html<-as.character(structural_analysis_options_panel(type,lang,c('사용자 집단 [g]'='g')))
  doc<-xml2::read_html(html,encoding='UTF-8');stopifnot(identical(contract(doc),contract(base_doc)))
  panel<-xml2::xml_find_first(doc,'//*[contains(concat(" ",@class," ")," tab-pane ")]')
  visible<-paste(xml2::xml_text(panel),paste(xml2::xml_attr(xml2::xml_find_all(panel,'//*[@placeholder]'),'placeholder'),collapse=' '))
  if(!lang %in% c('en','ko')){
   base_panel<-xml2::xml_find_first(base_doc,'//*[contains(concat(" ",@class," ")," tab-pane ")]')
   texts<-c(trimws(xml2::xml_text(xml2::xml_find_all(base_panel,'.//label|.//option|.//p'))),xml2::xml_attr(xml2::xml_find_all(base_panel,'.//*[@placeholder]'),'placeholder'))
   dictionary<-jsonlite::fromJSON(file.path('i18n',paste0(lang,'.json')))$translations
   for(en in texts[!texts %in% c('ML','MLR','WLSMV','PLS','PLSc','FIML','')]){
    key<-paste0('analysis.ui.',gsub('^_|_$','',gsub('[^a-z0-9]+','_',tolower(en))))
    stopifnot(!is.null(dictionary[[key]]),grepl(dictionary[[key]],visible,fixed=TRUE))
   }
  }
  before_options<-xml2::xml_find_all(base_doc,'//option');after_options<-xml2::xml_find_all(doc,'//option')
  ids<-which(grepl(' resamples$',xml2::xml_text(before_options)))
  stopifnot(length(ids)>0)
  for(i in ids){
   count<-sub(' resamples$','',xml2::xml_text(before_options[i]))
   expected<-gsub('{count}',count,statedu_localized_text(lang,'{count} resamples','{count}회'),fixed=TRUE)
   stopifnot(identical(xml2::xml_text(after_options[i]),expected))
  }
  writeLines(html,file.path(out,paste0(type,'-',lang,'.html')),useBytes=TRUE)
  cat('PASS:',type,lang,'estimation labels/help, resampling counts, unchanged IDs/defaults/values\n')
 }
}
