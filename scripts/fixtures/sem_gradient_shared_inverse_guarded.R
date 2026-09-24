# Research-only compatibility and scope gates; no package-file or namespace writes.
source('scripts/fixtures/sem_gradient_shared_inverse_candidate.R', local = TRUE)
make_guarded_shared_inverse_gradient <- function(count_calls = FALSE) {
 ns <- asNamespace('lavaan'); original <- get('lav_model_grad', ns)
 fallback <- function(reason) list(gradient = original, available = FALSE, reason = reason)
 if (!identical(as.character(utils::packageVersion('lavaan')), '0.7.2')) return(fallback('version'))
 expected <- c(
  lav_model_grad='329157f0aeaf6c966820d33df0d014a57aef4a560b73b591717a19867a298d27',
  lav_lisrel_ibinv='0d02d68248f0c741c0a655f4030e74a2cff9f5234130c93631fd91fbdc5562fa',
  lav_model_sigma='8a965a57b1e877e22872a30c28d8b27e385571533672d125b13e61877490c019',
  lav_lisrel_sigma='e3512a23e4211a69c0d1c8e232c1af17ab0fc2fae5020b7d7cdddf6c9af4fbc7',
  lav_model_mu='e5692d124050b7d43b76352f353471069920ef229b166f50e019b468740e2476',
  lav_lisrel_mu='8798194608f810eb2d29dfffcc63efedd6d56c2214695c11243a172be626280a',
  lav_lisrel_df_dmlist='2f65a50726954246c57ee692e417106542b4aad0b9a689a1a6206891165e7faa')
 compatible <- vapply(names(expected), function(name) {
  if (bindingIsActive(name, ns)) return(FALSE)
  fun <- get(name, ns, inherits = FALSE)
  is.function(fun) && identical(environment(fun), ns) &&
   identical(digest::digest(list(formals(fun), body(fun)), algo = 'sha256'), expected[[name]])
 }, logical(1))
 if (!all(compatible)) return(fallback(paste(names(expected)[!compatible], collapse = ',')))
 shared <- make_shared_inverse_gradient(count_calls)
 scope <- environment(shared$gradient)
 scope$.original_gradient <- original
 old_body <- body(shared$gradient)
 original_call <- as.call(c(list(as.name('.original_gradient')),
  setNames(lapply(names(formals(original)), as.name), names(formals(original)))))
 body(shared$gradient) <- bquote({
  .supported <- tryCatch(isS4(lavmodel) && lavmodel@estimator == 'ML' &&
   lavmodel@representation == 'LISREL' && type == 'free' && !ceq_simple &&
   !lavmodel@ceq.simple.only && !lavmodel@categorical && !lavmodel@composites &&
   !lavmodel@conditional.x && !lavmodel@group.w.free &&
   length(lavmodel@rv.ov) == 0L && length(lavmodel@rv.lv) == 0L &&
   isS4(lavdata) && lavdata@nlevels == 1L &&
   (!lavsamplestats@missing.flag ||
    (length(lavdata@Mp) == lavmodel@nblocks && all(vapply(lavdata@Mp, function(p)
     identical(p$npatterns, 1L) && is.matrix(p$pat) && is.logical(p$pat) &&
      all(p$pat) && !length(p$empty.idx), logical(1))))),
   error = function(e) FALSE)
  if (!isTRUE(.supported)) return(.(original_call))
  .(old_body)
 })
 shared$available <- TRUE; shared$reason <- 'compatible complete single-level ML/LISREL'
 shared
}
