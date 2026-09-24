.libPaths(R.home('library'))
root <- 'output/menu-before-after-20260915'
ignored <- c('.Environment','timing','timings')
canonical <- function(x,depth=0L) {
 if(depth>60L) stop('Unsupported nesting depth')
 if(is.environment(x)) return(list(excluded='environment reference'))
 if(typeof(x)=='externalptr') return(list(excluded='external pointer'))
 if(is.function(x)) return(list(function_text=paste(deparse(x),collapse='\n')))
 if(isS4(x)) return(list(S4=class(x),slots=setNames(lapply(setdiff(slotNames(x),ignored),function(s)canonical(slot(x,s),depth+1L)),setdiff(slotNames(x),ignored))))
 if(is.language(x)||is.pairlist(x)) return(list(language=paste(deparse(x),collapse='\n')))
 a<-attributes(x); a<-a[setdiff(names(a),ignored)]
 if(is.list(x)) {
   keep<-if(is.null(names(x)))seq_along(x)else which(!names(x)%in%ignored)
   v<-lapply(keep,function(i) canonical(x[[i]],depth+1L));names(v)<-names(x)[keep]
 } else {v<-x;attributes(v)<-NULL}
 list(type=typeof(x),value=v,attributes=lapply(a,canonical,depth=depth+1L))
}
leaves <- function(x,path='result',depth=0L) {
 if(depth>60L || is.environment(x)||is.function(x)||is.language(x)||is.pairlist(x)||typeof(x)=='externalptr') return(list())
 if(is.numeric(x)) return(setNames(list(x),path))
 if(isS4(x)) {n<-setdiff(slotNames(x),ignored);return(unlist(lapply(n,function(s)leaves(slot(x,s),paste0(path,'@',s),depth+1L)),recursive=FALSE))}
 if(!is.list(x)) return(list())
 n<-names(x);if(is.null(n))n<-as.character(seq_along(x))
 unlist(lapply(which(!n%in%ignored),function(i)leaves(x[[i]],paste0(path,'/',n[i]),depth+1L)),recursive=FALSE)
}
tables <- function(x) {
 if(is.null(x))return(character())
 if(inherits(x,c('shiny.tag','shiny.tag.list'))) x<-htmltools::renderTags(x)$html
 if(!is.character(x)||length(x)!=1L)return(character())
 doc<-xml2::read_html(x)
 trimws(gsub('[[:space:]]+',' ',xml2::xml_text(xml2::xml_find_all(doc,'//th|//td'))))
}
frames <- function(x,path='result',depth=0L) {
 if(depth>60L||is.environment(x)||is.function(x)||is.language(x)||is.pairlist(x)||isS4(x))return(list())
 if(is.data.frame(x))return(setNames(list(x),path))
 if(!is.list(x))return(list())
 n<-names(x);if(is.null(n))n<-as.character(seq_along(x))
 unlist(lapply(which(!n%in%ignored),function(i)frames(x[[i]],paste0(path,'/',n[i]),depth+1L)),recursive=FALSE)
}
times<-do.call(rbind,lapply(list.files(root,pattern='^(before|after)-[12]-times.csv$',full.names=TRUE),read.csv))
times$error[is.na(times$error)]<-''
rows<-list();diffs<-list()
for(i in sort(unique(times$id))) {
 path<-function(v,r)file.path(root,paste0(v,'-',r,'-',i,'.rds'))
 if(!file.exists(path('before',1))||!file.exists(path('after',1)))next
 b<-readRDS(path('before',1));a<-readRDS(path('after',1))
 ok<-is.null(b$result$error)&&is.null(a$result$error)
 bn<-leaves(b$result$value);an<-leaves(a$result$value);common<-intersect(names(bn),names(an))
 unequal<-common[!vapply(common,function(p)identical(bn[[p]],an[[p]],num.eq=FALSE),logical(1))]
 details<-lapply(unequal,function(p) {
  bv<-bn[[p]];av<-an[[p]];d<-if(length(bv)==length(av)) abs(as.numeric(bv)-as.numeric(av)) else NA_real_
  data.frame(id=i,path=p,before_length=length(bv),after_length=length(av),max_abs=if(any(is.finite(d)))max(d[is.finite(d)])else NA_real_)
 });diffs<-c(diffs,details)
 canonical_equal<-ok&&identical(canonical(b$result$value),canonical(a$result$value),num.eq=FALSE)
 bf<-frames(b$result$value);af<-frames(a$result$value);common_frames<-intersect(names(bf),names(af))
 frame_differences<-sum(!vapply(common_frames,function(p)identical(canonical(bf[[p]]),canonical(af[[p]]),num.eq=FALSE),logical(1)))
 bt<-tables(if(!is.null(b$ui$value))b$ui$value else b$result$value)
 at<-tables(if(!is.null(a$ui$value))a$ui$value else a$result$value)
 current_repeat<-if(file.exists(path('after',2))) identical(canonical(a$result),canonical(readRDS(path('after',2))$result),num.eq=FALSE)else NA
 t<-times[times$id==i,];tb<-t[t$version=='before',];ta<-t[t$version=='after',]
 before<-median(tb$seconds);after<-median(ta$seconds)
 status<-if(!ok)'execution unavailable'else if(canonical_equal)'result equal after exclusions'else if(length(common)&&!length(unequal))'common numeric equal; other differences'else if(length(unequal))'numeric differences'else if(length(bt)&&identical(bt,at))'displayed cells equal'else 'structure/display differs; no numeric comparison'
 rows[[length(rows)+1L]]<-data.frame(id=i,menu=t$menu[1],before_seconds=if(ok)before else NA,after_seconds=if(ok)after else NA,reduction_percent=if(ok)100*(before-after)/before else NA,status,canonical_equal,numeric_common=length(common),numeric_differences=length(unequal),numeric_before_only=length(setdiff(names(bn),names(an))),numeric_after_only=length(setdiff(names(an),names(bn))),diagnostics_equal=identical(b$result$diagnostics,a$result$diagnostics),stdout_equal=identical(b$result$stdout,a$result$stdout),rng_equal=identical(b$result$rng,a$result$rng),displayed_cells_equal=length(bt)>0&&identical(bt,at),before_cells=length(bt),after_cells=length(at),current_repeat,before_error=paste(unique(tb$error[nzchar(tb$error)]),collapse='; '),after_error=paste(unique(ta$error[nzchar(ta$error)]),collapse='; '),before_render=median(tb$render_seconds),after_render=median(ta$render_seconds))
 rows[[length(rows)]]$common_frames<-length(common_frames)
 rows[[length(rows)]]$frame_differences<-frame_differences
}
summary<-do.call(rbind,rows)
write.csv(summary,file.path(root,'summary.csv'),row.names=FALSE)
if(length(diffs))write.csv(do.call(rbind,diffs),file.path(root,'numeric-differences.csv'),row.names=FALSE)
print(summary[,c('id','menu','before_seconds','after_seconds','reduction_percent','status','numeric_differences')],row.names=FALSE)
