Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/bootstrap-ci';dir.create(out,recursive=TRUE,showWarnings=FALSE)
examples<-list()
add<-function(name,ui){html<-as.character(ui);doc<-xml2::read_html(html);tables<-xml2::xml_find_all(doc,'//table');stopifnot(length(tables)>0);stopifnot(grepl('95% CI',html,fixed=TRUE),grepl('95% CI = 95% confidence interval',html,fixed=TRUE));examples[[name]]<<-paste0('<h3>',name,'</h3>',html);cat('PASS',name,'\n')}
t<-data.frame(Term=c('X','Y'),B=c(.3,.4),`Boot SE`=c(.1,.2),LLCI=c(.1,-.1),ULCI=c(.5,.9),`Boot p`=c(.003,.04),check.names=FALSE)
add('regression',coefficient_html_table(t,note_line='Boot SE = bootstrap standard error; LLCI = lower bootstrap limit; ULCI = upper bootstrap limit.'))
add('mixed-models',hierarchical_coefficient_html_table(list(t,data.frame(Term=c('X','Y'),B=c(.4,.5),SE=c(.1,.2),t=c(4,2.5),p=c(.001,.02))),c('Model 1','Model 2'),list(list(),list()),note_line='Boot SE = bootstrap standard error.'))
add('mixed-wide',hierarchical_coefficient_html_table(list(t,data.frame(Term=c('X','Y'),B=c(.4,.5),SE=c(.1,.2),t=c(4,2.5),p=c(.001,.02))),c('Model 1','Model 2'),list(list(),list()),note_line='Boot SE = bootstrap standard error.',output_table_style='wide'))
stopifnot(length(gregexpr('95% CI =',examples[['mixed-models']],fixed=TRUE)[[1]])==1L)
for(case in c('simple','moderation','moderated_mediation','wide')){
 r<-readRDS(file.path('outputs/spss_phase36_20260907',case,'analysis.rds'))
 add(paste0('mediation-',case),mediation_moderation_result_ui(r,language='ko',output_table_style=if(case=='wide')'wide' else 'standard'))
}
add('SEM-specific-indirect',structural_canvas_specific_indirect_html_table(data.frame(Path='X -> M -> Y',B='.25',`Boot SE`='.03',`Boot 95% CI lower`='.15',`Boot 95% CI upper`='.36',p='.002',`BH-adjusted p`='.004',check.names=FALSE)))
add('PLS-measurement',structural_canvas_pls_measurement_main_html_table(data.frame(Construct='A',`Construct type`='Common factor',Indicator='a1',`loading/weight`='.80',`Boot SE`='.04',`Boot 95% CI lower`='.70',`Boot 95% CI upper`='.89',`Boot t`='20',`Boot p`='<.001',`Boot BH-adjusted p`='<.001',`Item VIF`='1.3',Mode='A',check.names=FALSE)))
add('PLS-MGA',structural_canvas_basic_html_table(data.frame(Path='A -> B',`Bootstrap SE`='.05',`Bootstrap difference 95% CI`='-.20, .45',check.names=FALSE),role='main'))
add('PLS-conditional',structural_canvas_basic_html_table(data.frame(Path='X -> M -> Y',`Bootstrap SE`='.05',`2.5% CI`='-.20',`97.5% CI`='.45',check.names=FALSE),role='main'))
add('interrater-bootstrap',result_table_with_notes(interrater_agreement_html_table(data.frame(Method='ICC',Estimate='.82',`95% CI`='.71, .90',Note='Bootstrap percentile 95% CI is reported.',check.names=FALSE))))
add('survival-bootstrap',coefficient_html_table(data.frame(Group='A',Time=12,`Adjusted survival`='.9',LLCI='.85',ULCI='.95',check.names=FALSE),note_line='Confidence intervals are pointwise percentile-bootstrap intervals.'))
add('SEM-effects',structural_canvas_effect_main_html_table(data.frame(Predictor='X',Outcome='Y',Effect=c('Direct','Indirect','Total'),B=c('.2','.3','.5'),`B 95% CI`=c('.1, .3','.2, .4','.4, .6'),check.names=FALSE),ci=TRUE))
# Transformation must be idempotent, including pre-existing multi-row headers.
raw<-tags$table(tags$thead(tags$tr(tags$th(rowspan=2,'Variable'),tags$th(colspan=2,'95% CI')),tags$tr(tags$th('Lower'),tags$th('Upper'))),tags$tbody(tags$tr(tags$td('x'),tags$td('.1'),tags$td('.3'))))
stopifnot(identical(as.character(result_ci_header(raw)),as.character(result_ci_header(result_ci_header(raw)))))
writeLines(result_snapshot_document_html('Bootstrap CI',paste(examples,collapse='\n')),file.path(out,'screen.html'),useBytes=TRUE)
saveRDS(examples,file.path(out,'examples.rds'))


