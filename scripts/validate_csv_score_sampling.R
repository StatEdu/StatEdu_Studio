source('R/utils.R', encoding = 'UTF-8')
source('R/data_io.R', encoding = 'UTF-8')
reference <- new.env(parent = .GlobalEnv)
sys.source('R/data_io.R', reference)
b <- body(reference$csv_encoding_score)
b[[4]][[3]][[2]] <- quote(samples <- unlist(lapply(data[character_columns], function(column)
  utils::head(stats::na.omit(as.character(column)), 20)), use.names = FALSE))
body(reference$csv_encoding_score) <- b
args <- commandArgs(TRUE)
if (length(args)) sys.source(args[[1]], reference)
capture <- function(expr) {
  warnings <- messages <- character()
  value <- withCallingHandlers(tryCatch(force(expr), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) { warnings <<- c(warnings, conditionMessage(w)); invokeRestart('muffleWarning') },
    message = function(m) { messages <<- c(messages, conditionMessage(m)); invokeRestart('muffleMessage') })
  # readr creates a fresh parser pointer per call; compare its problem table.
  if (is.data.frame(value) && !is.null(attr(value, 'problems'))) {
    problems <- readr::problems(value)
    attr(value, 'problems') <- NULL
    value <- list(data = value, problems = problems)
  }
  list(value = value, warnings = warnings, messages = messages, rng = .Random.seed)
}
set.seed(522)
checks <- 0L
for (n in c(0, 1, 19, 20, 21, 39, 40, 41, 79, 80, 81, 1000)) for (missing in c(0, 0.5, 1)) {
  values <- sample(c('plain', '한글', '', '\uFFFD', 'é'), n, TRUE)
  values[seq_len(floor(n * missing))] <- NA_character_
  stopifnot(identical(unname(csv_encoding_text_sample(values)),
    as.vector(utils::head(stats::na.omit(as.character(values)), 20))))
  frame <- data.frame(x = values, number = seq_len(n))
  stopifnot(identical(capture(reference$csv_encoding_score(frame)), capture(csv_encoding_score(frame)), num.eq = FALSE))
  checks <- checks + 1L
}
for (positions in list(c(1, 20, 21, 39, 40, 41, 79, 80, 81), seq(1, 1000, 23),
                      981:1000, 1:20)) {
  values <- rep(NA_character_, 1000)
  values[positions] <- paste0('value-', positions)
  stopifnot(identical(csv_encoding_text_sample(values),
    as.vector(utils::head(stats::na.omit(values), 20))))
  frame <- data.frame(x = values)
  stopifnot(identical(capture(reference$csv_encoding_score(frame)), capture(csv_encoding_score(frame)), num.eq = FALSE))
  checks <- checks + 1L
}
root <- tempfile('csv-sample-');dir.create(root)
texts <- c('a,b\n1,hello\n2,world', 'a,b\n1,한글\n2,값', 'a,b\n1,"two\nlines"\n2,',
           'a,b\n1,NA\n2,NA', '', '1,2\n3,4')
for (i in seq_along(texts)) for (encoding in c('UTF-8', 'CP949')) {
  path <- file.path(root, paste0(i, '-', encoding, '.csv'))
  bytes <- iconv(texts[[i]], from = 'UTF-8', to = encoding, toRaw = TRUE)[[1]]
  writeBin(bytes, path)
  for (header in c(TRUE, FALSE)) {
    stopifnot(identical(capture(reference$read_csv_robust(path, header)),
                        capture(read_csv_robust(path, header)), num.eq = FALSE))
    checks <- checks + 1L
  }
}
cat('PASS:', checks, 'exact score/import/condition/RNG comparisons and sample checks.\n')
