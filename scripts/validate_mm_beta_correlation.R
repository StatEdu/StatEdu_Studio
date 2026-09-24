Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/mm-beta-correlation';dir.create(out,recursive=TRUE,showWarnings=FALSE)
path<-'C:/SynologyDrive/StatEdu/Analysis/2026/58_0916-노영숙_박사과정/간호오류 통계분석 및 자료/(통계분석)김세민박사_학술통계분석/김세민박사_한글.sav'
d<-as.data.frame(haven::read_sav(path))
roles<-list(x='M직무요구',mediators=c('M환자안전역량','M업무몰입'),y='M환자안전문화',w=character(),covariates=character())
stopifnot(all(unlist(roles)%in%names(d)))
r<-run_mediation_moderation_analysis(d,roles,'parallel',character(),200L,1234L,analysis_method='process_ols',auto_method=FALSE)
for(p in r$path_results){
 t<-mediation_moderation_display_coefficient_table(p)
 mm<-model.matrix(p$model);y<-model.response(model.frame(p$model))
 expected<-coef(p$model)*apply(mm,2,sd)/sd(y);expected['(Intercept)']<-NA_real_
 stopifnot(isTRUE(all.equal(unname(t$beta),unname(expected[match(t$Term,mediation_moderation_clean_term(names(expected)))]),tolerance=1e-10)))
 print(t[,c('Term','B','beta')]);cat('PASS independent standardized OLS coefficients\n')
 b<-p;b$use_hc3<-TRUE;b$coef_table[["HC3 SE"]]<-b$coef_table$SE;stopifnot(!'beta'%in%names(mediation_moderation_display_coefficient_table(b)))
}
a<-r$correlation_appendix;vars<-a$variables
stopifnot(a$n==412, isTRUE(all.equal(a$correlations,cor(d[,vars]))))
print(a$table)
# Pearson p-values independently checked with cor.test, and listwise cases retained.
for(i in 2:length(vars))for(j in seq_len(i-1))stopifnot(grepl(format_p(cor.test(d[[vars[i]]],d[[vars[j]]])$p.value),a$table[i,j+1],fixed=TRUE))
small<-data.frame(x=c(1:7,NA),y=c(2,3,1,4,6,5,8,9),z=c(1,2,3,4,5,NA,7,8),group=c(1,2,1,2,1,2,1,2))
x<-mediation_moderation_correlation_appendix(small,list(x='x',y='y',mediators='z',covariates='group'),data.frame(name='group',measurement='category'))
stopifnot(x$n==6,!('group'%in%x$variables))
for(style in c('standard','wide','compact')){
 html<-mediation_moderation_saved_results_html(r,language='ko',output_table_style=style)
 stopifnot(grepl('β',html,fixed=TRUE),grepl('mm-correlation-appendix',html,fixed=TRUE),!grepl('Standardized coefficients are not reported',html,fixed=TRUE))
 writeLines(html,file.path(out,paste0(style,'.html')),useBytes=TRUE)
}
saveRDS(r,file.path(out,'result.rds'))
# Reuse representative displayed tables for current and accumulated format checks.
doc<-xml2::read_html(as.character(mediation_moderation_result_ui(r,language='ko')))
parts<-xml2::xml_find_all(doc,"//*[contains(concat(' ',normalize-space(@class),' '),' mm-model4-path-section ')]|//*[contains(concat(' ',normalize-space(@class),' '),' mm-correlation-appendix ')]")
if(length(parts)<2){parts<-xml2::xml_find_all(doc,"//div[contains(@class,'hierarchical-standard-model-block')]|//div[contains(@class,'mm-correlation-appendix')]")}
stopifnot(length(parts)>=2)
html<-paste(vapply(parts,as.character,character(1)),collapse='\n')
stopifnot(grepl('β',html,fixed=TRUE))
saveRDS(list(list(id='ols',title='OLS beta and correlations',html=html)),file.path(out,'entries.rds'))
cat('PASS Pearson r/p, complete cases, categorical exclusions, standard/wide/compact renderers\n')
