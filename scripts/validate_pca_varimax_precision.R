Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('scripts/validate_factor_pca.R',encoding='UTF-8')
out<-Sys.getenv('PCA_VALIDATION_OUTPUT',unset=tempdir())
dir.create(out,recursive=TRUE,showWarnings=FALSE)
original_body<-body(psych::principal)
set.seed(4608)
z<-matrix(rnorm(2400),400,6);z[,2]<-z[,1]+z[,2]/3;z[,4]<-z[,3]+z[,4]/3
x<-as.data.frame(z);names(x)<-paste0('q',1:6);x$q6<-x$q6*4
info<-data.frame(name=names(x),measurement='continuous')
checks<-list()
for(matrix_type in c('correlation','covariance'))for(k in c(1,3)){
 xx<-x;xx[3,1]<-NA;xx[7,4]<-NA
 a<-prepare_pca_results(xx,names(xx),variable_info=info,options=list(matrix_type=matrix_type,rotation='varimax',criterion='fixed',n_components=k))
 stopifnot(max(abs(a$analysis_matrix-a$fit$r))<1e-10,max(abs(a$eigenvalues-a$fit$values))<1e-10)
 expected<-scale(a$complete,scale=matrix_type!='covariance')%*%solve(a$analysis_matrix,a$loadings)
 cat('CASE',matrix_type,k,'score error',max(abs(expected-as.matrix(a$scores))),'matrix error',max(abs(a$analysis_matrix-a$fit$r)),'\n')
saved<-pca_saved_score_outputs(a,'TEST')
 stopifnot(max(abs(expected-as.matrix(a$scores)))<1e-9,nrow(saved)==400,all(is.na(saved[c(3,7),])),max(abs(as.matrix(saved[-c(3,7),,drop=FALSE])-expected))<1e-9,max(abs(colSums(a$loadings^2)-a$fit$Vaccounted['SS loadings',]))<1e-9)
 checks[[length(checks)+1]]<-data.frame(case=paste(matrix_type,k),pass=TRUE)
}
ordinal<-as.data.frame(lapply(x,function(v)as.integer(cut(v,breaks=quantile(v,seq(0,1,length.out=6)),include.lowest=TRUE))))
oi<-data.frame(name=names(ordinal),measurement='ordinal')
for(k in c(1,3)){
 a<-prepare_pca_results(ordinal,names(ordinal),variable_info=oi,options=list(matrix_type='polychoric',rotation='varimax',criterion='fixed',n_components=k,save_component_scores=TRUE))
 stopifnot(a$matrix_type=='polychoric',is.null(a$scores),inherits(try(pca_saved_score_outputs(a),silent=TRUE),'try-error'),max(abs(rowSums(a$loadings^2)-a$communality))<1e-9,max(abs(colSums(a$loadings^2)-a$fit$Vaccounted['SS loadings',]))<1e-9)
 checks[[length(checks)+1]]<-data.frame(case=paste('polychoric',k),pass=TRUE)
}
for(rot in c('none','oblimin')){
 a<-psych::principal(x,nfactors=3,rotate=rot);b<-pca_principal(x,nfactors=3,rotate=rot)
 stopifnot(identical(a$loadings,b$loadings),identical(a$scores,b$scores))
 checks[[length(checks)+1]]<-data.frame(case=rot,pass=TRUE)
}
art<-readRDS('outputs/spss_phase45_20260907/pca_artifacts.rds');rows<-list()
for(nm in names(art)){
 t<-art[[nm]]$trial
 p<-pca_principal(t$complete,nfactors=t$n_components,rotate='varimax',scores=TRUE)
 err<-max(abs(p$loadings-t$loadings));se<-max(abs(p$scores-as.matrix(t$scores)))
 stopifnot(err<1e-12,se<1e-12)
 rows[[length(rows)+1]]<-data.frame(case=nm,loading_error=err,score_error=se)
}
stopifnot(identical(body(psych::principal),original_body),length(rows)==50)
write.csv(do.call(rbind,checks),file.path(out,'edge_checks.csv'),row.names=FALSE)
write.csv(do.call(rbind,rows),file.path(out,'archive_checks.csv'),row.names=FALSE)
cat('PASS: production PCA adapter; 50 archive cases and 8 edge/control cases; psych namespace unchanged\n')
