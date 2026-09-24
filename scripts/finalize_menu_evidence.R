.libPaths(R.home('library'))
for(e in parse('scripts/summarize_menu_versions.R')) {
 if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('times')))break
 eval(e,.GlobalEnv)
}
records<-list()
for(r in 1:2){
 b<-readRDS(file.path(root,paste0('sem-long-before-',r,'.rds')));a<-readRDS(file.path(root,paste0('sem-long-after-',r,'.rds')))
 common<-intersect(names(b$value),names(a$value))
 eq<-vapply(common,function(n)identical(b$value[[n]],a$value[[n]],num.eq=FALSE),logical(1))
 stopifnot(all(eq),nrow(b$value)==nrow(a$value))
 records[[paste0('sem-',r)]]<-list(common_columns=length(common),all_columns_identical=all(eq),rows=nrow(a$value),new_columns=setdiff(names(a$value),names(b$value)),valid_before=b$summary$valid,valid_after=a$summary$valid)
}
for(kind in c('SEM','CFA','PLS'))for(v in c('before','after')){
 read<-function(r){if(kind=='SEM')readRDS(file.path(root,paste0('sem-long-',v,'-',r,'.rds')))$value else readRDS(file.path(root,paste0('extra-boot-',kind,'-',v,'-',r,'.rds')))$result$value}
 stopifnot(identical(canonical(read(1)),canonical(read(2)),num.eq=FALSE))
}
for(i in 1:38){
 b1<-readRDS(file.path(root,paste0('before-1-',i,'.rds')))$result;b2<-readRDS(file.path(root,paste0('before-2-',i,'.rds')))$result
 a1<-readRDS(file.path(root,paste0('after-1-',i,'.rds')))$result;a2<-readRDS(file.path(root,paste0('after-2-',i,'.rds')))$result
 stopifnot(identical(canonical(b1),canonical(b2),num.eq=FALSE),identical(canonical(a1),canonical(a2),num.eq=FALSE))
}
saveRDS(records,file.path(root,'final-evidence.rds'));print(records)
for(v in c('before','after')){
 cfa<-readRDS(file.path(root,paste0('extra-boot-CFA-',v,'-1.rds')))$result$value
 cat('CFA',v,'valid',sum(vapply(cfa$estimates,function(x)is.data.frame(x)&&nrow(x)>0,logical(1))),'\n')
 pls<-readRDS(file.path(root,paste0('extra-boot-PLS-',v,'-1.rds')))$result$value
 keep<-setdiff(grep('fail|valid|requested|status',names(pls),value=TRUE),'valid_positions')
 print(pls[keep])
}
cat('PASS: 76 menu repeat comparisons, 6 long-bootstrap repeat comparisons, and all 21 SEM summary columns in both before/after pairs\n')
