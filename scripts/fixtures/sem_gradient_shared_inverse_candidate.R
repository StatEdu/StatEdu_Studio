# Research-only factory. Does not modify the lavaan namespace or package files.
make_shared_inverse_gradient <- function(count_calls = FALSE) {
 ns <- asNamespace('lavaan')
 scope <- new.env(parent = ns)
 state <- new.env(parent = emptyenv())
 state$key <- NULL; state$value <- NULL; state$calls <- 0L; state$hits <- 0L
 original_inverse <- get('lav_lisrel_ibinv', ns)
 scope$lav_lisrel_ibinv <- function(mlist = NULL) {
  if (count_calls) state$calls <- state$calls + 1L
  if (!is.null(state$key) && identical(mlist, state$key, num.eq = FALSE)) {
   if (count_calls) state$hits <- state$hits + 1L
   return(state$value)
  }
  diagnostic <- FALSE
  value <- withCallingHandlers(original_inverse(mlist),
   warning = function(w) diagnostic <<- TRUE, message = function(m) diagnostic <<- TRUE)
  if (!diagnostic) { state$key <- mlist; state$value <- value }
  value
 }
 for (name in c('lav_model_sigma', 'lav_lisrel_sigma', 'lav_model_mu',
                'lav_lisrel_mu', 'lav_lisrel_df_dmlist')) {
  fun <- get(name, ns); environment(fun) <- scope; scope[[name]] <- fun
 }
 scope$.inverse_state <- state
 gradient <- get('lav_model_grad', ns)
 previous_body <- body(gradient)
 body(gradient) <- bquote({
  .inverse_state$key <- NULL
  .inverse_state$value <- NULL
  .(previous_body)
 })
 environment(gradient) <- scope
 list(gradient = gradient, state = state)
}
