out <- 'tmp/cluster-method-notes-i18n';dir.create(out,recursive=TRUE,showWarnings=FALSE)
base <- list(effect_size_cluster_effect='-.5',effect_size_cluster_p1='.5',effect_size_cluster_p2='.65',effect_size_cluster_size='20',effect_size_cluster_periods='5')
cases <- expand.grid(design=c('parallel_binary','stepped_wedge','parallel_continuous'),icc=c(.01,.05),stringsAsFactors=FALSE)
designs <- paste(cases$design,cases$icc)
results <- lapply(seq_len(nrow(cases)),function(i)effect_size_cluster_calculate(modifyList(base,list(effect_size_cluster_design=cases$design[i],effect_size_cluster_icc=as.character(cases$icc[i])))))
keys <- c(parallel_binary='note_cluster_binary',stepped_wedge='note_cluster_stepped',parallel_continuous='cluster_continuous')
formula_keys <- paste0('sample_size.result.',keys[cases$design])
h <- 2*asin(sqrt(.5))-2*asin(sqrt(.65))
for(i in seq_along(results)) {
 r <- results[[i]]; de <- (1+19*cases$icc[i])*if(cases$design[i]=='stepped_wedge')5/4 else 1
 effect <- if(cases$design[i]=='parallel_binary')h else -.5
 stopifnot(is.null(r$error),identical(r$method_note,statedu_t(formula_keys[i],'en')),isTRUE(all.equal(r$design_effect,de)),isTRUE(all.equal(r$primary_effect_size,effect)),isTRUE(all.equal(r$planning_effect_size,effect/sqrt(de))))
}
for(lang in c('en','ko','ja','zh','es','fr','de','vi')) {
 stopifnot(grepl('h / sqrt(1 + (m - 1)ICC)',statedu_t(formula_keys[1],lang),fixed=TRUE),grepl('periods / (periods - 1)',statedu_t(formula_keys[2],lang),fixed=TRUE),grepl('d / sqrt(1 + (m - 1)ICC)',statedu_t(formula_keys[3],lang),fixed=TRUE))
}
for(design in unique(cases$design)) {
 invalid <- effect_size_cluster_calculate(modifyList(base,list(effect_size_cluster_design=design,effect_size_cluster_icc='0')))
 stopifnot(identical(invalid$error,'ICC must be greater than 0.00 and less than 1.00.'))
}
cat('PASS six actual cluster cases across ICC .01/.05 and three unchanged zero-ICC rejections; signed effect/design-effect references and eight-language expressions\n')
