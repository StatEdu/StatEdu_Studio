source('R/utils.R', encoding = 'UTF-8')
make_table <- function(reference = FALSE, overlays = NULL) {
  env <- new.env(parent = .GlobalEnv)
  sys.source('R/labels.R', env)
  if (!is.null(overlays)) env$statedu_locale_overlays <- function() overlays
  if (reference) {
    closure <- environment(env$statedu_translation_table)
    init <- closure$initialize
    loop <- which(vapply(as.list(init), function(x) is.call(x) && identical(x[[1]], as.name('for')), logical(1)))
    stopifnot(length(loop) == 1L)
    init[[loop]] <- quote(for (language in names(overlays)) {
      translations <- overlays[[language]]
      if (!is.list(translations) || length(translations) == 0) next
      for (key in names(translations)) {
        value <- translations[[key]]
        if (is.null(value) || length(value) == 0) next
        current <- cache[[key]]
        if (is.null(current) && grepl('^language[.][A-Za-z0-9_-]+$', key)) {
          code <- sub('^language[.]', '', key)
          spec <- statedu_language_registry()[[code]]
          if (!is.null(spec)) {
            current <- vapply(statedu_supported_languages(), function(target_language) {
              names <- spec$names %||% list(en = code)
              as.character(names[[target_language]] %||% names[['en']] %||% code)
            }, character(1))
          }
        }
        current <- current %||% c(en = NA_character_, ko = NA_character_)
        current[[language]] <- as.character(value[[1]])
        cache[[key]] <- current
      }
    })
    closure$initialize <- init
  }
  env$statedu_translation_table
}
capture <- function(fn) {
  warnings <- messages <- character()
  result <- withCallingHandlers(tryCatch(fn(), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning') },
    message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart('muffleMessage') })
  list(value = result, warnings = warnings, messages = messages, rng = .Random.seed)
}
cases <- list(NULL, list(), list(ko = list(ui.data = 'changed', new_key = 'new')),
  list(ko = setNames(list('first', 'second', 'existing', 'duplicate'), c('new', 'new', 'ui.data', 'ui.data'))),
  list(ko = list(ui.data = NULL, empty = character(), number = 4, missing = NA_character_)),
  list(en = list(new = 'first'), ko = list(new = 'second'), zz = list(new = 'third')),
  list(ko = 'invalid', en = list('unnamed')),
  list(ko = list(language.xx = 'Unknown', language.en = 'English override')))
set.seed(281)
for (overlays in cases) {
  before <- make_table(TRUE, overlays)
  after <- make_table(FALSE, overlays)
  stopifnot(identical(capture(before), capture(after), num.eq = FALSE))
  stopifnot(identical(capture(before), capture(after), num.eq = FALSE))
}
cat('PASS:', length(cases), 'overlay scenarios, initial/cached values, conditions and RNG identical.\n')
