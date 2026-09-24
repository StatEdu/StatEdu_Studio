Sys.setlocale('LC_CTYPE','English_United States.utf8');Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
model <- 'F1 =~ x1+x2+x3\nF2 =~ x4+x5+x6\nF3 =~ x7+x8+x9\nG =~ F1+F2+F3'
fit <- lavaan::cfa(model,data=lavaan::HolzingerSwineford1939)
snapshot <- list(nodes=lapply(c('G','F1','F2','F3'),function(x)list(id=x,name=x)),edges=lapply(c('F1','F2','F3'),function(x)list(from='G',to=x,pathType='higherOrder')))
stopifnot(structural_canvas_omega_h(snapshot,fit)$available)
reasons <- c('No higher-order loading paths are specified.','Omega-h requires exactly one higher-order general factor.','Omega-h is not reported when a lower-order factor has multiple higher-order loadings.','No observed indicators were found under the lower-order factors.','Omega-h is not reported with cross-loaded observed indicators.','The model-implied indicator covariance matrix is unavailable.','Omega-h denominator is not positive and finite.')
expr <- body(structural_canvas_register_local_fit_outputs)[[4]][[3]][[2]]
out <- 'tmp/omega-reason-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
entries <- list()
for(i in seq_along(reasons))for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 reason <- reasons[[i]];translated <- structural_canvas_omega_h_reason_text(reason,language)
 if(language=='en')stopifnot(identical(translated,reason)) else stopifnot(translated!=reason)
 env <- new.env(parent=globalenv());env$fit_result <- function()list(fit=fit,snapshot=snapshot)
 env$app_language_fn <- function()language;env$display_name_for <- function(bundle)identity
 env$structural_canvas_omega_h <- function(snapshot,fit)list(available=FALSE,reason=reason)
 render <- function()NULL;body(render) <- expr;environment(render) <- env
 html <- as.character(render());doc <- xml2::read_html(html,encoding='UTF-8')
 notes <- xml2::xml_text(xml2::xml_find_all(doc,'//p'))
 expected <- sprintf(statedu_localized_text(language,'Hierarchical omega was not reported: %s','위계적 omega를 보고하지 않았습니다: %s'),translated)
 stopifnot(expected %in% notes)
 if(language=='ja')entries[[paste0('reason',i)]] <- list(id=paste0('reason',i),title=paste('Reason',i),html=html)
 cat('PASS:',language,'unavailable reason',i,'actual panel\n')
}
for(language in c('en','ko','ja','zh','es','fr','de','vi')) {
 unknown <- 'External detail: Review 사용자 <&> %s'
 stopifnot(identical(structural_canvas_omega_h_reason_text(unknown,language),unknown),identical(structural_canvas_omega_h_reason_text('',language),''))
}
saveRDS(unname(entries),file.path(out,'entries.rds'))
