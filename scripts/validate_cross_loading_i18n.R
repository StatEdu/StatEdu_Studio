Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
factor_names<-c('Review 사용자 <&> %s','Factor.2, 특수')
snapshot<-list(nodes=list(list(id='a',role='latent',name=factor_names[1]),list(id='b',role='latent',name=factor_names[2]),list(id='x',role='indicator',name='지표 <&>')),
 edges=list(list(id='ax',from='a',to='x'),list(id='bx',from='b',to='x')))
issues<-structural_canvas_identification_diagnostics(snapshot)
cross<-issues[issues$Code=='cross_loading',,drop=FALSE];stopifnot(nrow(cross)==1L)
for(language in c('en','ko','ja','zh','es','fr','de','vi')){
 translated<-structural_canvas_identification_issue_message(cross$Code,cross$Message,language)
 if(language=='en')stopifnot(translated==cross$Message) else stopifnot(translated!=cross$Message)
 stopifnot(grepl(paste(factor_names,collapse=', '),translated,fixed=TRUE))
 combined<-structural_canvas_identification_issue_text(cross,language)
 stopifnot(grepl('지표 <&>',combined,fixed=TRUE),grepl(translated,combined,fixed=TRUE))
 for(unknown in c('Review','The indicator loads on multiple factors: incomplete','custom %s <&> message'))stopifnot(structural_canvas_identification_issue_message('cross_loading',unknown,language)==unknown)
 cat('PASS:',language,'actual shared-indicator diagnostic, literal factor list and indicator, unknown format fallback\n')
}
