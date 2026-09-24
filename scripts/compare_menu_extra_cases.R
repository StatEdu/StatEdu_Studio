struct <- new.env(parent=.GlobalEnv)
active <- FALSE
for(e in parse(file.path(host_root,'scripts/validate_sem_canvas.R'))) {
 if(is.call(e)&&identical(e[[1]],as.name('set.seed'))) active<-TRUE
 if(active) eval(e,struct)
 if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('snapshot'))) break
}
add('CFA ML',function() run_structural_canvas_analysis(struct$snapshot,struct$data,'cfa'))
add('SEM ML',function() run_structural_canvas_analysis(struct$snapshot,struct$data,'cbsem'))
add('PLS-SEM',function() run_structural_canvas_analysis(struct$snapshot,struct$data,'plssem'))
add('Mediation 5000',function() run_mediation_moderation_analysis(reg_data,list(y='y',x='x1',mediators='x2',w=character(),covariates=character()),'parallel',character(),5000L,20260915L))
add('Moderation 5000',function() run_mediation_moderation_analysis(reg_data,list(y='y',x='x1',mediators=character(),w='x2',covariates=character()),'parallel','xy',5000L,20260915L))
add('Custom mediation 5000',function() run_mediation_moderation_analysis(reg_data,list(y='y',x='x1',mediators='x2',w=character(),covariates=character()),'parallel',character(),5000L,20260915L,custom_path_model=TRUE,direct_x_to_y='x1',x_to_m=list(x2='x1'),m_to_y='x2'))
add('Ridge LASSO ElasticNet 30',function() {
 fit<-lm(y~x1+x2,data=reg_data)
 fit_penalized_models(list(list(formula=y~x1+x2,coef_table=data.frame(Term=names(coef(fit)),B=unname(coef(fit))))),reg_data,seed=20260915L,alpha_grid=.5,selection_bootstrap_resamples=30L)
})
add('Meta-analysis random DL',function() {
 effects<-do.call(rbind,lapply(1:12,function(i) meta_normalize_effect(list(included=TRUE,study_id=paste0('S',i),study_name=paste0('Study ',i),family='g',input_type='g_se',direction='positive',g=sin(i)*.3+.4,se=.1+i*.01),row_id=i)))
 meta_fit_model(effects,'g',model='random',tau_method='DL')
})
add('Complex mediation moderation',function() {
 d<-survey_data;d$M<-.6*d$x1+.2*d$x2+y;d$Y<-.2*d$x1+.7*d$M+.15*d$M*d$x2+y
 node<-function(id,v,role)list(id=id,variableId=v,role=role,x=0,y=0)
 snap<-list(nodes=list(node('x','x1','independent'),node('m','M','mediator'),node('w','x2','moderator'),node('y','Y','dependent')),
 edges=list(list(id='xm',from='x',to='m'),list(id='my',from='m',to='y'),list(id='xy',from='x',to='y')),
 moderations=list(list(id='w_my',from='w',toEdge='my')))
 design<-complex_sample_normalize_design_state(list(cluster='psu',weight='wt'))
 complex_sample_run_custom_model(d,snap,design,confidence=.95)
})
