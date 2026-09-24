Sys.setenv(STATEDU_MODULE_CACHE_DIR=file.path(tempdir(),'four-block-modules'))
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
options(statedu.app_language='ko')
set.seed(916); d<-data.frame(x1=rnorm(100),x2=rnorm(100),x3=rnorm(100),x4=rnorm(100));d$y<-d$x1+d$x4+rnorm(100)
d$x4[1:5]<-NA
info<-data.frame(name=names(d),var_label=names(d),measurement='continuous')
four<-prepare_hierarchical_analysis_results(d,'y','x1','x2','x3',block4='x4',variable_info=info,residual_diagnostics=FALSE,auto_method=FALSE)$results
stopifnot(length(four)==4L,all(vapply(four,function(x)x$n==95L,logical(1))))
for(i in 1:4)stopifnot(isTRUE(all.equal(unname(coef(four[[i]]$model)),unname(coef(lm(reformulate(paste0('x',1:i),'y'),data=d[complete.cases(d),]))))))
legacy<-compact_analysis_blocks('x1',character(0),'x3');stopifnot(identical(names(legacy),paste0('block',1:3)))
spec<-list(VERSION=1L,STUDIO_VERSION='1.3.0',DATA='test',DATA_HASH=regression_syntax_data_hash(d),DEPENDENTS='y',PREDICTORS=paste0('x',1:4),MEASUREMENTS=regression_syntax_measurements(info),REFERENCES=list(),MISSING='LISTWISE',CI_METHOD='bias_corrected',BOOTSTRAP=1000L,SEED=916L,RESIDUAL_DIAGNOSTICS=FALSE,AUTO_METHOD=FALSE,SHOW_SR2=FALSE,SHOW_F2=FALSE,SHOW_VIF=FALSE,OUTPUT_STYLE='wide',BLOCK1='x1',BLOCK2='x2',BLOCK3='x3',BLOCK4='x4')
stopifnot(identical(spec,parse_regression_syntax(regression_syntax_text(spec))))
stopifnot(length(prepare_regression_syntax(spec,d,info)$results)==4L)
scoped <- d
attr(scoped,'statedu_scope_excluded') <- 'x4'
scoped_results <- prepare_hierarchical_analysis_results(scoped,'y','x1','x2','x3',block4='x4',variable_info=info,residual_diagnostics=FALSE,auto_method=FALSE)$results
stopifnot(length(scoped_results)==3L,all(vapply(scoped_results,function(x)x$n==100L,logical(1))))
setup<-hierarchical_setup_state(names(d),'y','x1','x2','x3',info,block4='x4',active_block='block4')
stopifnot(!length(setup$available),hierarchical_active_block_setup(setup)$index==4L)
shiny::testServer(function(input,output,session){
 dep<-reactiveVal('y');ind<-reactiveVal(c('x2','x3'));con<-reactiveVal('x1');b3<-reactiveVal('x3');b4<-reactiveVal(character(0));act<-reactiveVal('block3');dirty<-reactiveVal(0L)
 selected<-function()names(d);sy<-function(...){dep()}
 register_hierarchical_block_observers(input,session,dep,ind,con,ind,selected,function()names(d),function()names(d),b3,b3,act,sy,function()dirty(isolate(dirty())+1L),hierarchical_block4_current_fn=b4,hierarchical_block4_names=b4)
},{
 session$flushReact()
 session$setInputs(hierarchical_block_next=1);stopifnot(act()=='block4')
 session$setInputs(hierarchical_available='x4',hierarchical_available_active=1)
 session$setInputs(hierarchical_block4_move=1);stopifnot(identical(b4(),'x4'),all(c('x2','x3','x4')%in%ind()))
 session$setInputs(analysis_transfer_drop=list(source='hierarchical_block3',target='hierarchical_block4',values='x3'))
 stopifnot(!length(b3()),identical(b4(),c('x4','x3')))
 session$setInputs(hierarchical_block4='x3',move_hierarchical_block4_up=1);stopifnot(identical(b4(),c('x3','x4')))
 session$setInputs(hierarchical_block4_reorder=list(order=c('x4','x3'),selected='x3'));stopifnot(identical(b4(),c('x4','x3')))
 session$setInputs(hierarchical_block4_reorder=list(order=c('x4','x4'),selected='x4'));stopifnot(identical(b4(),c('x4','x3')))
 session$setInputs(hierarchical_block4_doubleclick=list(value='x4'));stopifnot(identical(b4(),'x3'),!('x4'%in%ind()))
 session$setInputs(analysis_transfer_drop=list(source='hierarchical_available',target='hierarchical_block4',values='not_in_data'));stopifnot(identical(b4(),'x3'))
})
cat('FOUR_BLOCK_STATISTICS_SYNTAX_UI_STATE_PASSED\n')
line_fixture <- xml2::read_html('<table><thead><tr><th>Value</th></tr></thead><tbody><tr><td><span class="coefficient-cell-break"><span>-.03~.26</span><span>.110</span></span></td></tr></tbody></table>')
stopifnot(identical(result_html_table_cells(xml2::xml_find_first(line_fixture,'//table'))$values[2,1],'-.03~.26\n.110'))
out<-'D:/Program/output/janghana_20260916/four_block_patch/exports';dir.create(out,recursive=TRUE,showWarnings=FALSE)
args<-list(results=four,variable_table=info,show_sr2=FALSE,show_f2=FALSE,show_vif=FALSE,output_table_style='wide')
html<-as.character(htmltools::renderTags(do.call(hierarchical_results_panel,args))$html)
stopifnot(grepl('Model 4',html,fixed=TRUE))
e<-list(id='four-block-test',title='Four blocks',html=html,saved_at='2026-09-16')
saveRDS(list(entry=e,args=args),file.path(out,'fixture.rds'))
for(scope in c('current','accumulated')) {
 entries<-if(scope=='current')list(e) else list(e,within(e,{id<-'second';title<-'Second four blocks'}))
 write_result_collection_html(entries,file.path(out,paste0(scope,'.html')))
 save_result_collection_excel_file(entries,file.path(out,paste0(scope,'.xlsx')))
 write_result_collection_docx(entries,file.path(out,paste0(scope,'.docx')))
 cat(scope,'HTML_EXCEL_WORD_WRITTEN\n')
}

