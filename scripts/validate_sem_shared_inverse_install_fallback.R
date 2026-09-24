.libPaths(R.home('library'))
source('R/setup_custom_model_canvas_structural_bootstrap.R')
ns<-asNamespace('lavaan');saved<-get('lav_model_grad',ns);locked<-bindingIsLocked('lav_model_grad',ns)
for(factory in list(function()list(available=FALSE,reason='incompatible'),
 function()stop('factory failure'),function()list(available=TRUE,gradient=1))) {
 state<-structural_canvas_sem_shared_inverse_install(factory)
 stopifnot(!state$applied,identical(get('lav_model_grad',ns),saved),
  identical(bindingIsLocked('lav_model_grad',ns),locked),
  !exists('.statedu_sem_shared_inverse_state',.GlobalEnv,inherits=FALSE))
}
unlockBinding('lav_model_grad',ns)
tryCatch({
 state<-structural_canvas_sem_shared_inverse_install(structural_canvas_sem_shared_inverse_gradient)
 stopifnot(state$applied,!bindingIsLocked('lav_model_grad',ns))
 structural_canvas_effect_bootstrap_worker_cleanup()
 structural_canvas_effect_bootstrap_worker_cleanup()
 stopifnot(identical(get('lav_model_grad',ns),saved),!bindingIsLocked('lav_model_grad',ns))
},finally=if(locked)lockBinding('lav_model_grad',ns))
cat('PASS: incompatible/factory-error/compile-error bypass and unlocked-binding/idempotent cleanup\n')
