# Evaluate only the timing helpers and UI factory, without starting a server.
expressions <- parse('app.R', encoding = 'UTF-8')
assignment <- function(name) {
  hits <- Filter(function(x) is.call(x) && identical(x[[1]], as.name('<-')) &&
    identical(x[[2]], as.name(name)), as.list(expressions))
  stopifnot(length(hits) == 1L)
  hits[[1L]]
}
env <- new.env(parent = .GlobalEnv)
eval(assignment('startup_time'), env)
env$logs <- character()
env$startup_log <- function(message) env$logs <- c(env$logs, message)
env$app_version <- 'test-version'
env$calls <- 0L
env$app_ui <- function(version, request) {
  env$calls <- env$calls + 1L
  list(version = version, request = request, random = runif(1))
}
eval(assignment('ui'), env)
stopifnot(env$calls == 0L, length(env$logs) == 0L)
request <- new.env(parent = emptyenv())
set.seed(651)
expected <- env$app_ui(env$app_version, request)
rng <- .Random.seed
env$calls <- 0L
set.seed(651)
actual <- env$ui(request)
stopifnot(identical(actual, expected, num.eq = FALSE), identical(.Random.seed, rng),
          env$calls == 1L, length(env$logs) == 1L,
          grepl('^build ui [0-9.]+s$', env$logs[[1]]))
env$app_ui <- function(version, request) stop('UI failure', call. = FALSE)
failure <- tryCatch(env$ui(request), error = conditionMessage)
stopifnot(identical(failure, 'UI failure'), length(env$logs) == 1L)
env$app_ui <- function(version, request) { warning('UI warning', call. = FALSE); NULL }
warnings <- character()
result <- withCallingHandlers(env$ui(request), warning = function(w) {
  warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning')
})
stopifnot(is.null(result), identical(warnings, 'UI warning'), length(env$logs) == 2L)
cat('PASS: deferred UI execution, request/value/RNG preservation, timing emission and conditions.\n')
