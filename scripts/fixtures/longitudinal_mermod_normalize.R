normalize<-function(x){
 if(inherits(x,'merMod'))return(normalize(list(fixed=lme4::fixef(x),random=lme4::ranef(x),vcov=as.matrix(vcov(x)),fitted=fitted(x),residuals=residuals(x),logLik=logLik(x),theta=lme4::getME(x,'theta'),devcomp=lme4::getME(x,'devcomp'),optinfo=x@optinfo,call=x@call,frame=x@frame)))
 if(inherits(x,'formula')){environment(x)<-.GlobalEnv;return(x)}
 if(is.call(x)){if(is.function(x[[1L]]))x[[1L]]<-as.name('.normalized_call_function');for(i in seq_along(x))if(!identical(x[[i]],quote(expr=)))x[i]<-list(normalize(x[[i]]));return(x)}
 if(is.function(x))return(list(formals=formals(x),body=body(x)))
 if(!is.null(attr(x,'terms')))attr(x,'terms')<-normalize(attr(x,'terms'))
 if(!is.null(attr(x,'formula')))attr(x,'formula')<-normalize(attr(x,'formula'))
 if(!is.null(attr(x,'.Environment')))attr(x,'.Environment')<-.GlobalEnv
 if(is.list(x))for(i in seq_along(x))x[i]<-list(normalize(x[[i]]))
 x
}

