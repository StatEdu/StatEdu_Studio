Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
out<-'tmp/meta-warning-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
rows<-do.call(rbind,lapply(1:2,function(i)meta_normalize_effect(list(study_id=paste0('study',i),family='g',input_type='g_se',g=i/10,se=.1))))
result<-meta_fit_model(rows,'g',model='fixed')
capture<-function(expr)tryCatch(expr,error=identity)
errors<-list(dependency=capture(meta_aggregate_study_effects(result,rho=1)),leave_one=capture(meta_leave_one_study_out(result)),trimfill=capture(meta_trimfill(result)))
stopifnot(all(vapply(errors,inherits,logical(1),'error')))
rows<-do.call(rbind,lapply(1:4,function(i)meta_normalize_effect(list(study_id=paste0('study',i),family='g',input_type='g_se',g=i/10,se=.1,moderator_continuous=if(i==4)'' else paste0('user_variable=',20+i)))))
result<-meta_fit_model(rows,'g',model='fixed')
result$moderator<-meta_fit_moderator(result,'continuous::user_variable')
stopifnot(result$moderator$omitted==1L,result$moderator$center==22)
raw<-'External diagnostic: user %s <&>'
result$notes<-c(vapply(names(errors),function(k)paste0(statedu_t(paste0('meta.warning.',k),'en'),conditionMessage(errors[[k]])),character(1)),paste0(statedu_t('meta.warning.cr2','en'),raw))
interpretation_keys<-paste0('meta.warning.',c('independent','small_cr2','cr_comparison','trim_interpretation','funnel_min'))
interpretations<-vapply(interpretation_keys,function(key)statedu_t(key,'en'),character(1))
source_text<-paste(readLines('R/server_meta.R',encoding='UTF-8'),collapse='\n')
stopifnot(all(vapply(interpretations,function(note)grepl(note,source_text,fixed=TRUE),logical(1))))
result$notes<-c(result$notes,unname(interpretations))
result$notes<-c(result$notes,sprintf(statedu_t('meta.warning.multi_count','en'),'2'),sprintf(statedu_t('meta.warning.omitted_count','en'),result$moderator$omitted))
cr_errors<-list(
 cr_complete=capture(meta_cluster_robust_wls(1:2,c(1,1),matrix(1,2,1),1:2)),
 cr_tau=capture(meta_cluster_robust_wls(1:4,rep(1,4),matrix(1,4,1),1:4,tau2=-1)),
 cr_clusters=capture(meta_cluster_robust_wls(1:4,rep(1,4),matrix(1,4,1),c(1,1,2,2))),
 cr_parameters=capture(meta_cluster_robust_wls(1:3,rep(1,3),diag(3),1:3)),
 cr_rank=capture(meta_cluster_robust_wls(1:4,rep(1,4),cbind(rep(1,4),rep(1,4)),1:4)),
 cr_square=capture(meta_symmetric_matrix_power(matrix(1,2,3),-.5)),
 cr_psd=capture(meta_symmetric_matrix_power(diag(c(1,-1)),-.5)),
 cr_type=capture(meta_cluster_robust_wls(1:4,rep(1,4),matrix(1,4,1),1:4,type='invalid')),
 cr_lengths=capture(meta_cluster_robust_wls(1:4,rep(1,3),matrix(1,4,1),1:4)),
 cr_confidence=capture(meta_cluster_robust_wls(1:4,rep(1,4),matrix(1,4,1),1:4,conf_level=1)),
 cr_singular=capture(meta_cluster_robust_wls(1:4,rep(1,4),cbind(1,c(1,0,0,0)),1:4,type='CR3')),
 cr_variance=capture(meta_cluster_robust_wls(rep(0,4),rep(1,4),matrix(1,4,1),1:4)))
# Force only the degrees-of-freedom calculation boundary to exercise its defensive guard.
invalid_df<-meta_cluster_robust_wls
environment(invalid_df)<-list2env(list(vapply=function(X,FUN,FUN.VALUE,...)rep(NA_real_,length(X))),parent=environment(meta_cluster_robust_wls))
cr_errors$cr_df<-capture(invalid_df(1:4,rep(1,4),matrix(1,4,1),1:4))
for(key in names(cr_errors))stopifnot(inherits(cr_errors[[key]],'error'),conditionMessage(cr_errors[[key]])==statedu_t(paste0('meta.warning.',key),'en'))
result$notes<-c(result$notes,vapply(cr_errors,function(error)paste0(statedu_t('meta.warning.cr2','en'),conditionMessage(error)),character(1)))
result$dependency_models<-list(three_level=simpleError('A three-level model requires at least one study with multiple effects.'))
result$show_egger<-TRUE
result$egger<-meta_egger_test(result)
result$dependency_sensitivity<-meta_dependency_sensitivity(result)
result$leave_one_study_out<-meta_leave_one_study_out(result)
result$trimfill<-meta_trimfill(result)
before<-result
norm<-function(x)gsub('[[:space:]\u00a0]+','',paste(x,collapse=''),perl=TRUE)
png(file.path(out,'forest.png'),width=1000,height=700);draw_meta_forest_plot(result,'en');dev.off()
image<-paste0('<img src="data:image/png;base64,',base64enc::base64encode(file.path(out,'forest.png')),'"/>')
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 panel<-xml2::read_html(as.character(meta_analysis_results_ui(result,lang)),encoding='UTF-8')
 # Exercise all Egger display variants through the actual table renderer in the captured export.
 for(reason in c('minimum','intercept_error')) {
  unavailable<-modifyList(result$egger,list(available=FALSE,message=statedu_t(paste0('meta.egger.',reason),'en')))
  section<-meta_result_section('Egger',meta_egger_results_table(unavailable,lang),'appendix',lang)
  xml2::xml_add_child(xml2::xml_find_first(panel,'//body/div'),xml2::read_xml(as.character(section)))
 }
 for(p in c(.01,.5)) {
  available<-list(available=TRUE,intercept=.4,standard_error=.1,ci=c(.2,.6),statistic=4,df=8,p_value=p)
  section<-meta_result_section('Egger',meta_egger_results_table(available,lang),'appendix',lang)
  xml2::xml_add_child(xml2::xml_find_first(panel,'//body/div'),xml2::read_xml(as.character(section)))
 }
 stopifnot(xml2::xml_text(xml2::xml_find_first(panel,'//h2'))==statedu_t('meta.egger.title',lang))
 notes<-xml2::xml_text(xml2::xml_find_all(panel,'//*[contains(@class,"meta-analysis-notes")]//li'))
 expected<-c(paste0(statedu_t('meta.warning.dependency',lang),statedu_t('meta.warning.rho',lang)),paste0(statedu_t('meta.warning.leave_one',lang),statedu_t('meta.warning.leave_min',lang)),paste0(statedu_t('meta.warning.trimfill',lang),statedu_t('meta.warning.trim_min',lang)),paste0(statedu_t('meta.warning.cr2',lang),raw))
 expected<-c(expected,unname(vapply(interpretation_keys,function(key)statedu_t(key,lang),character(1))))
 expected<-c(expected,sprintf(statedu_t('meta.warning.multi_count',lang),'2'),sprintf(statedu_t('meta.warning.omitted_count',lang),'1'))
 expected<-c(expected,unname(vapply(names(cr_errors),function(key)paste0(statedu_t('meta.warning.cr2',lang),statedu_t(paste0('meta.warning.',key),lang)),character(1))))
 centered<-xml2::xml_text(xml2::xml_find_first(panel,'//*[contains(@class,"meta-moderator-centering-note")]'))
 stopifnot(centered==sprintf(statedu_t('meta.warning.centered',lang),'22.000'))
 for(count in c('0','1','12345'))for(key in c('multi_count','omitted_count'))stopifnot(meta_result_warning_text(sprintf(statedu_t(paste0('meta.warning.',key),'en'),count),lang)==sprintf(statedu_t(paste0('meta.warning.',key),lang),count))
 unknown<-paste0(sprintf(statedu_t('meta.warning.omitted_count','en'),'1'),' User %s');stopifnot(meta_result_warning_text(unknown,lang)==unknown)
 stopifnot(identical(unname(notes),expected),identical(result,before))
 main<-xml2::xml_text(xml2::xml_find_all(panel,'//*[contains(@class,"meta-result-section--main")]'))
 if(lang=='en')english_main<-main else stopifnot(identical(main,english_main))
 cat('PASS render:',lang,'localized warnings; English main tables; unchanged result\n');flush.console()
 if(!lang %in% c('ja','ko'))next
 options(statedu.app_language=lang)
 xml2::xml_replace(xml2::xml_find_first(panel,'//*[@id="meta_forest_plot"]'),xml2::read_xml(image))
 html<-as.character(xml2::xml_find_first(panel,'//body/div'))
 entry<-list(id=paste0('meta-',lang),title='Meta-analysis',html=html)
 for(mode in c('current','accumulated')) {
  entries<-if(mode=='current')list(entry)else list(entry,modifyList(entry,list(id='second')))
  stem<-file.path(out,paste(lang,mode,sep='-'))
  write_result_collection_html(entries,paste0(stem,'.html'))
  write_result_collection_docx(entries,paste0(stem,'.docx'))
  save_result_collection_excel_file(entries,paste0(stem,'.xlsx'))
  write_result_collection_pdf(entries,paste0(stem,'.pdf'))
  write_result_collection_hwpx(entries,paste0(stem,'.hwpx'))
  word<-norm(xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.docx'),'word/document.xml'))))
  members<-unzip(paste0(stem,'.hwpx'),list=TRUE)$Name
  hwpx<-norm(vapply(members[grepl('Contents/section[0-9]+[.]xml$',members)],function(s)xml2::xml_text(xml2::read_xml(unz(paste0(stem,'.hwpx'),s))),character(1)))
  wb<-openxlsx::loadWorkbook(paste0(stem,'.xlsx'))
  excel<-norm(unlist(lapply(seq_along(names(wb)),function(i)as.matrix(openxlsx::read.xlsx(wb,sheet=i,colNames=FALSE)))))
  saved<-norm(xml2::xml_text(xml2::read_html(paste0(stem,'.html'))))
  expected_text<-c(xml2::xml_text(xml2::xml_find_all(panel,'//th|//td|//h2|//h3|//h4|//li')),centered)
  for(k in c('CI','τ²'))expected_text<-c(expected_text,statedu_t(paste0('meta.note.',k),lang))
  for(value in expected_text[nzchar(trimws(expected_text))])for(actual in list(word,hwpx,excel,saved))if(!grepl(norm(value),actual,fixed=TRUE))stop('Missing content: ',value)
  stopifnot(file.info(paste0(stem,'.pdf'))$size>1000,identical(result,before))
  jsonlite::write_json(expected_text,paste0(stem,'-expected.json'))
  cat('PASS export:',lang,mode,'HTML/Word/HWPX/Excel content; PDF generated\n');flush.console()
 }
}
