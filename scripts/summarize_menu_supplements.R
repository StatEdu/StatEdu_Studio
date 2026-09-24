.libPaths(R.home('library'))
for(e in parse('scripts/summarize_menu_versions.R')) {
 if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('times')))break
 eval(e,.GlobalEnv)
}
survey_b<-readRDS(file.path(root,'survey-audit-before.rds'));survey_a<-readRDS(file.path(root,'survey-audit-after.rds'))
survey<-do.call(rbind,lapply(names(survey_b),function(id){
 b<-survey_b[[id]];a<-survey_a[[id]]
 data.frame(id=as.integer(id),before_calls=length(b$calls),after_calls=length(a$calls),instrumentation_unchanged=b$unchanged&&a$unchanged,
   raw_calls_equal=identical(canonical(b$calls),canonical(a$calls),num.eq=FALSE))
}))
write.csv(survey,file.path(root,'survey-values-summary.csv'),row.names=FALSE);print(survey)
rows<-list();details<-list()
compare_pair<-function(kind,b,a,bseconds,aseconds,iteration){
 bn<-leaves(b);an<-leaves(a);p<-intersect(names(bn),names(an))
 diff<-p[!vapply(p,function(k)identical(bn[[k]],an[[k]],num.eq=FALSE),logical(1))]
 value_diff<-p[!vapply(p,function(k)identical(as.numeric(bn[[k]]),as.numeric(an[[k]]),num.eq=FALSE),logical(1))]
 eq<-identical(canonical(b),canonical(a),num.eq=FALSE)
 details[[paste(kind,iteration)]]<<-list(all_equal=all.equal(canonical(b),canonical(a),tolerance=0),numeric_differences=diff,numeric_value_differences=value_diff)
 rows[[length(rows)+1L]]<<-data.frame(kind,iteration,before_seconds=bseconds,after_seconds=aseconds,reduction_percent=100*(bseconds-aseconds)/bseconds,canonical_equal=eq,numeric_common=length(p),numeric_differences=length(diff),numeric_value_differences=length(value_diff),before_only=length(setdiff(names(bn),names(an))),after_only=length(setdiff(names(an),names(bn))))
}
for(r in 1:2){
 b<-readRDS(file.path(root,paste0('sem-long-before-',r,'.rds')));a<-readRDS(file.path(root,paste0('sem-long-after-',r,'.rds')))
 compare_pair('SEM 5000',b$value,a$value,b$summary$elapsed_seconds,a$summary$elapsed_seconds,r)
 cat('SEM',r,'valid',b$summary$valid,a$summary$valid,'conditions',identical(b$conditions,a$conditions),'RNG',identical(b$rng,a$rng),'\n')
 for(kind in c('CFA','PLS')) {
  bp<-file.path(root,paste0('extra-boot-',kind,'-before-',r,'.rds'));ap<-file.path(root,paste0('extra-boot-',kind,'-after-',r,'.rds'))
  if(!file.exists(bp)||!file.exists(ap))next
  b<-readRDS(bp);a<-readRDS(ap)
  stopifnot(is.null(b$result$error),is.null(a$result$error),!is.null(b$result$value),!is.null(a$result$value))
  compare_pair(paste(kind,if(kind=='CFA')1000 else 5000),b$result$value,a$result$value,b$seconds,a$seconds,r)
  cat(kind,r,'diagnostics',identical(b$result$diagnostics,a$result$diagnostics),'stdout',identical(b$result$stdout,a$result$stdout),'RNG',identical(b$result$rng,a$result$rng),'\n')
 }
}
write.csv(do.call(rbind,rows),file.path(root,'bootstrap-summary.csv'),row.names=FALSE)
saveRDS(details,file.path(root,'bootstrap-comparison-details.rds'));print(do.call(rbind,rows))
for(family in c('g','r','or'))stopifnot(identical(readRDS(file.path(root,'meta',paste0('result-',family,'-1.rds'))),readRDS(file.path(root,'meta',paste0('result-',family,'-2.rds'))),num.eq=FALSE))
cat('PASS: meta cross-process exact comparison for g/r/or\n')
