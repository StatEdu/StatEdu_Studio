Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(946);n<-72
d<-data.frame(g2=factor(rep(c('사용자 대조','Model-based'),each=36)),g3=factor(rep(c('낮음','중간','높음'),each=24)))
d$y<-rnorm(n)+rep(0:2,each=24)
for(i in 1:3){d[[paste0('t',i)]]<-rnorm(n)+i;d[[paste0('b',i)]]<-rbinom(n,1,.2*i)}
info<-data.frame(name=names(d),measurement=c('binary','category','continuous',rep(c('continuous','binary'),3)),var_label=paste('사용자 변수',seq_along(d)))
independent<-prepare_ttest_anova_results(d,'y',c('g2','g3'),variable_info=info,options=list(force_nonparametric=TRUE,normality_enabled=FALSE,normality_method='none',post_hoc=TRUE,nonparametric_post_hoc_method='holm',effect_size=TRUE,median_iqr=TRUE));independent$type<-'nonparametric'
paired<-prepare_nonparametric_paired_unified_results(d,list(c('t1','t2'),c('b1','b2'),c('t1','t2','t3'),c('b1','b2','b3')),variable_info=info,options=list(effect_size=TRUE,median_iqr=TRUE,posthoc_adjustment='holm',time_labels=c('사용자 전','Model-based','사용자 후')))
out<-'tmp/nonparametric-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
baseline<-NULL;baseline_sections<-NULL
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 options(statedu.app_language=language)
 html<-paste(as.character(ttest_anova_results_ui(independent)),as.character(nonparametric_paired_results_ui(paired)),sep='\n')
 doc<-xml2::read_html(html,encoding='UTF-8')
 main<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']//th|//table[@data-result-table-role='main']//td"))
 main_sections<-xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='main']/ancestor::div[contains(concat(' ',normalize-space(@class),' '),' result-section ')][1]"))
 if(is.null(baseline_sections))baseline_sections<-main_sections else stopifnot(identical(main_sections,baseline_sections))
 generated<-paste(main_sections,collapse='\n')
 for(label in c(info$var_label,levels(d$g2),levels(d$g3),'사용자 전','사용자 후'))generated<-gsub(label,'',generated,fixed=TRUE)
 stopifnot(!grepl('[가-힣]',generated))
 if(is.null(baseline))baseline<-main else stopifnot(identical(main,baseline))
 appendix<-xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']")
 stopifnot(length(main)>0,length(appendix)>0,all(xml2::xml_attr(appendix,'data-result-table-language')==language))
 if(language!='en'){
  appendix_text<-paste(xml2::xml_text(appendix),collapse='\n')
  for(source in c('Post-hoc included','Pairwise Wilcoxon rank-sum test with Holm Bonferroni','Wilcoxon','McNemar','Friedman')){
   expected<-result_appendix_ui_text(source,language)
   stopifnot(!identical(expected,source),grepl(expected,appendix_text,fixed=TRUE))
  }
  reason<-sprintf(statedu_localized_text(language,'%s nonparametric repeated-measures test'),paste(vapply(c('Continuous','Binary'),paired_appendix_text,character(1),language=language),collapse=', '))
  stopifnot(grepl(reason,appendix_text,fixed=TRUE),!grepl('Continuous, Binary nonparametric repeated-measures test',appendix_text,fixed=TRUE))
 }
 writeLines(html,file.path(out,paste0(language,'.html')),useBytes=TRUE)
 if(language=='ja')saveRDS(list(list(id='nonparametric',title='Nonparametric tests',html=html)),file.path(out,'entries.rds'))
 cat('PASS:',language,'independent and paired actual results; main cells invariant; appendix roles\n')
}
saveRDS(list(independent=independent,paired=paired),file.path(out,'models.rds'))
doc<-xml2::read_html(file.path(out,'en.html'),encoding='UTF-8')
values<-unique(xml2::xml_text(xml2::xml_find_all(doc,"//table[@data-result-table-role='appendix']//th|//table[@data-result-table-role='appendix']//td")))
for(value in values)if(grepl('[A-Za-z]{3}',value)&&identical(paired_appendix_text(value,'ja'),value))cat('REVIEW:',value,'\n')
