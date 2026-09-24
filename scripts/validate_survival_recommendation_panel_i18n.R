Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
stopifnot(is.null(survival_design_recommendation_panel(NULL)))
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 for(status in c('ready','needs_confirmation','blocked','unsupported','Review')) {
  for(with_audit in c(FALSE,TRUE)) {
   r<-survival_recommend(list(objective='association',data_shape='single_record',event_structure='single'))
   r$status<-status;r$primary<-'Review Normality None 사용자 <&> %s';r$rule_ids<-c('A01','Custom<&>%s')
   if(with_audit)r$preflight<-list(counts=list(source_rows=137L,analysis_rows=112L,events=43L),issues=data.frame(code=character(),message=character()))
   before<-r
   doc<-xml2::read_html(as.character(survival_design_recommendation_panel(r,lang)),encoding='UTF-8')
   txt<-xml2::xml_text(doc)
   stopifnot(identical(r,before),grepl(r$primary,txt,fixed=TRUE))
   stopifnot(length(xml2::xml_find_all(doc,'//button[@id="open_recommended_survival_analysis"]'))==as.integer(status=='ready'))
   stopifnot(length(xml2::xml_find_all(doc,'//div[@class="survival-recommendation-explanation"]'))==as.integer(status=='ready'))
   rules<-xml2::xml_find_all(doc,'//p[@class="survival-recommendation-rule"]')
   stopifnot(length(rules)==as.integer(status!='blocked'))
   if(status!='blocked')stopifnot(grepl(paste(r$rule_ids,collapse=', '),xml2::xml_text(rules),fixed=TRUE))
   summary<-xml2::xml_find_all(doc,'//div[contains(@class,"survival-recommendation-data")]/p')
   stopifnot(length(summary)==as.integer(with_audit))
   if(with_audit) {
    numbers<-regmatches(xml2::xml_text(summary),gregexpr('[0-9]+',xml2::xml_text(summary)))[[1]]
    expected<-if(lang %in% c('ko','ja','zh'))c('137','112','25','43')else c('112','137','25','43')
    stopifnot(identical(numbers,expected))
   }
   if(lang!='en') {
    headings<-xml2::xml_text(xml2::xml_find_all(doc,'//h3|//h4|//button'))
    stopifnot(!any(headings %in% c('Input check','Recommendation','Why this analysis','Main results','When to choose the alternative','Data handling summary','Open recommended analysis')))
    if(status!='Review')stopifnot(!xml2::xml_text(xml2::xml_find_first(doc,'//strong')) %in% c('Ready: ','Confirmation needed: ','Blocked: ','Not supported: '))
   }
  }
 }
 cat('PASS:',lang,'5 statuses x audit present/absent; localized headings/status/button; count order; raw labels/rules preserved\n')
}
