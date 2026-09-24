.libPaths(R.home('library'))
root<-'output/menu-before-after-20260915'
for(e in parse('scripts/summarize_menu_versions.R')) {
 if(is.call(e)&&identical(e[[1]],as.name('<-'))&&identical(e[[2]],as.name('times')))break
 eval(e,.GlobalEnv)
}
rows<-list();notes<-list()
for(i in 1:38){
 b<-readRDS(file.path(root,paste0('before-1-',i,'.rds')))$result
 a<-readRDS(file.path(root,paste0('after-1-',i,'.rds')))$result
 bn<-leaves(b$value);an<-leaves(a$value);common<-intersect(names(bn),names(an))
 unequal<-common[!vapply(common,function(p)identical(bn[[p]],an[[p]],num.eq=FALSE),logical(1))]
 for(p in unequal){
  bv<-bn[[p]];av<-an[[p]]
  rows[[length(rows)+1L]]<-data.frame(id=i,path=p,before_type=typeof(bv),after_type=typeof(av),values_equal=identical(as.numeric(bv),as.numeric(av),num.eq=FALSE),before_head=paste(head(as.numeric(bv),3),collapse=' / '),after_head=paste(head(as.numeric(av),3),collapse=' / '))
 }
 notes[[as.character(i)]]<-capture.output(print(all.equal(canonical(b$value),canonical(a$value),tolerance=0)))
 if(i%in%c(17,24:29)) {
  value_b<-if(i==17)b$value$results[[1]] else tables(b$value)
  value_a<-if(i==17)a$value$results[[1]] else tables(a$value)
  writeLines(c('BEFORE',capture.output(dput(value_b)),'AFTER',capture.output(dput(value_a))),file.path(root,paste0('display-detail-',i,'.txt')))
 }
}
write.csv(do.call(rbind,rows),file.path(root,'numeric-difference-types.csv'),row.names=FALSE)
saveRDS(notes,file.path(root,'canonical-difference-details.rds'))
cat('Detailed comparison records written\n')
