Sys.setenv(STATEDU_MODULE_CACHE='false')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
set.seed(42);d<-data.frame(x=rnorm(180),g=factor(rep(c('A','B','C'),60)),gg=factor(rep(c('N','Y'),90)))
d$y<-d$x+3*(d$g=='B')-2*(d$g=='C')+2*(d$gg=='Y')+rnorm(180)
f<-y~x+g+gg;frame<-model.frame(f,d);mm<-model.matrix(f,frame);groups<-penalized_factor_groups(f,frame,mm);x<-mm[,-1,drop=FALSE]
stopifnot(identical(names(groups),c('g','gg')),identical(lengths(groups),c(g=2L,gg=1L)),!any(groups$g%in%groups$gg))
selected<-match(c('x','gB','ggY'),colnames(x));test<-91:180
single<-penalized_factor_split_test(x,d$y,test,selected,groups)
full<-lm(y~x+g+gg,d[test,]);without_g<-lm(y~x+gg,d[test,]);without_gg<-lm(y~x+g,d[test,])
stopifnot(single$status=='Tested',all(groups$g%in%single$expanded))
stopifnot(isTRUE(all.equal(unname(single$tests$g['p']),anova(without_g,full)$`Pr(>F)`[2],tolerance=1e-10)))
stopifnot(isTRUE(all.equal(unname(single$tests$gg['F']),summary(full)$coefficients['ggY','t value']^2,tolerance=1e-10)))
# Reparameterizing a fully included factor preserves the partial F statistic.
d2<-d;d2$g<-relevel(d2$g,'C');m2<-model.matrix(f,d2);g2<-penalized_factor_groups(f,model.frame(f,d2),m2)
s2<-penalized_factor_split_test(m2[,-1],d2$y,test,seq_len(ncol(m2)-1L),g2)
stopifnot(isTRUE(all.equal(unname(single$tests$g['F']),unname(s2$tests$g['F']),tolerance=1e-10)))
# A missing category is not silently dropped and relabelled as an omnibus test.
missing<-penalized_factor_split_test(x,d$y,which(d$g!='C'),selected,groups)
stopifnot(missing$status=='Insufficient residual degrees of freedom or rank deficiency',all(is.na(missing$raw_p)))
for(method in c('LASSO','Elastic Net')){
 r<-penalized_multisplit(x,d$y,method,88,20,groups=groups)
 plain<-penalized_multisplit(x,d$y,method,88,20)
 stopifnot(identical(r$p,plain$p),identical(r$adjusted_split_p,plain$adjusted_split_p),all(r$groups$p<.05))
 for(b in seq_len(20)){
  a<-r$groups$audit[[b]]
  if(a$status=='Tested')stopifnot(isTRUE(all.equal(unname(r$groups$adjusted_split_p[b,a$selected]),unname(pmin(1,a$raw_p[a$selected]*length(a$selected))))))
 }
 stopifnot(identical(r$groups$p,penalized_multisplit_aggregate(r$groups$adjusted_split_p)))
}
info<-data.frame(name=names(d),measurement=c('continuous','nominal','binary','continuous'))
r<-prepare_penalized_menu(d,'y',c('x','g','gg'),'LASSO',info,resamples=2,post_selection=TRUE,inference_splits=20)
stopifnot(nrow(r$publication_factor_inference)==2,setequal(r$publication_factor_inference$Variable,c('g','gg')),
 grepl('Table 5. Categorical',as.character(penalized_result_block(r)),fixed=TRUE))
message('PASS: categorical assignment, full-factor expansion, reference F tests and binary t², recoding invariance of partial F, missing-level failure, unchanged individual tests, group correction and aggregation, main table')
