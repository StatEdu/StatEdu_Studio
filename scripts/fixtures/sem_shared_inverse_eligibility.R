# Conservative research policy, separate from numerical options and draw generation.
sem_shared_inverse_eligible <- function(prepared, reps, workers) {
 isTRUE(tryCatch({
  if(length(reps)!=1L || !is.numeric(reps) || !is.finite(reps) || reps<200 || reps!=trunc(reps) ||
     length(workers)!=1L || !is.numeric(workers) || !is.finite(workers) || workers<2 || workers!=trunc(workers))return(FALSE)
  if(!is.list(prepared) || !is.data.frame(prepared$data) || anyNA(prepared$data) ||
     !all(vapply(prepared$data,function(x)is.numeric(x)&&is.null(attributes(x))&&all(is.finite(x)),logical(1))) ||
     length(prepared$product_specs)!=1L)return(FALSE)
  spec<-prepared$product_specs[[1L]]
  if(!spec$method %in% c('all_pairs_dmc','matched_pair_dmc') || !is.data.frame(spec$pairs) || !nrow(spec$pairs))return(FALSE)
  fit<-prepared$fit_template;model<-fit@Model
  inherits(fit,'lavaan') && model@estimator=='ML' && model@representation=='LISREL' &&
   model@nblocks==1L && fit@Data@nlevels==1L && isFALSE(fit@Options$std.lv) &&
   !model@ceq.simple.only && !model@categorical && !model@composites && !model@conditional.x &&
   !model@group.w.free && !length(model@rv.ov) && !length(model@rv.lv)
 },error=function(e)FALSE))
}
