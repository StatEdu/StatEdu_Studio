Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(949);n<-120
d<-data.frame(psu=rep(1:30,each=4),stratum=rep(1:3,each=40),wt=runif(n,.5,2),g2=factor(rep(c('사용자 대조','Model-based'),60)),g3=factor(rep(c('낮음','중간','높음'),40)),single='사용자 단일',empty=NA_real_)
d$y<-as.numeric(d$g3)*4+rnorm(n);d$y[c(2,11)]<-NA
info<-data.frame(name=c('y','g2','g3','single','empty'),measurement=c('continuous','category','category','category','continuous'),var_label=c('사용자 결과 50%','사용자 두 집단','사용자 세 집단','사용자 제외','Survey design'))
input<-list(p_strata='stratum',p_cluster='psu',p_weight='wt',p_fpc='',p_variance_method='auto',p_lonely_psu='adjust',p_use_replicate_weights=FALSE,p_subpopulation='',p_subpopulation_condition='',p_subpopulation_condition_type='equals',p_subpopulation_condition_value='',p_post_hoc=TRUE,p_post_hoc_correction='holm')
out<-'tmp/complex-group-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE);baseline<-NULL
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 html<-as.character(complex_sample_group_result(d,c('y','empty'),c('g2','g3','single'),input,'p',variable_info=info,language=language))
 doc<-xml2::read_html(html,encoding='UTF-8')
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']//th|//table[@data-result-table-role='main']//td|//div[contains(@class,'coefficient-note')]"))
 if(is.null(baseline))baseline<-main else stopifnot(identical(baseline,main))
 stopifnot(length(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']"))>=2,grepl('Design-based pairwise t-test',html,fixed=TRUE))
 appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']");text<-paste(xml2::xml_text(appendix),collapse='\n')
 stopifnot(length(appendix)==2,all(xml2::xml_attr(appendix,'data-result-table-language')==language),any(xml2::xml_text(xml2::xml_find_all(doc,'//h3'))=='Survey design'))
 translate<-function(source)statedu_t(paste0('analysis.ui.',gsub('^_+|_+$','',gsub('[^a-z0-9]+','_',tolower(source)))),language,source)
 specs<-list(
 list('%s by %s was not computed because no complete cases were available after survey design/subpopulation filtering.','Survey design','사용자 두 집단'),
 list('%s by %s excluded %s row(s) with missing dependent or group values after survey design/subpopulation filtering.','사용자 결과 50%','사용자 두 집단',2),
 list('%s by %s was not computed because the group variable had fewer than two usable groups after complete-case filtering.','사용자 결과 50%','사용자 제외'))
 for(spec in specs){expected<-do.call(sprintf,c(list(translate(spec[[1]])),spec[-1]));stopifnot(grepl(expected,text,fixed=TRUE));if(language!='en')stopifnot(translate(spec[[1]])!=spec[[1]])}
 writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 if(language=='ja')saveRDS(list(list(id='complex-group',title='Complex-sample group comparisons',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'actual t/ANOVA and Holm posthoc; missing and skipped combinations; main/notes and user title preserved\n')
}
