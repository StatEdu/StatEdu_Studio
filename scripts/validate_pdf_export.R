source('R/result_export_files.R', encoding = 'UTF-8')
`%||%` <- function(x, y) if (is.null(x)) y else x
# Exercise failure modes without depending on a locally installed browser.
env <- new.env(parent = environment(write_pdf_from_html))
export <- write_pdf_from_html
environment(export) <- env
env$find_pdf_chromium <- function() 'mock browser.exe'
target <- tempfile(fileext = '.pdf')
original <- charToRaw('previous report')
cases <- c('failed', 'missing', 'empty', 'invalid', 'success')
for (case in cases) {
  writeBin(original, target)
  env$system2 <- function(command, args, stdout, stderr) {
    stopifnot(all(args == shQuote(sub('^"(.*)"$', '\\1', args))))
    decoded <- sub('^"(.*)"$', '\\1', args)
    path <- sub('^--print-to-pdf=', '', decoded[grepl('^--print-to-pdf=', decoded)])
    stopifnot(length(path) == 1L, path != target)
    if (case == 'failed') return(structure('renderer failed', status = 1L))
    if (case == 'empty') file.create(path)
    if (case == 'invalid') writeBin(charToRaw('not a PDF'), path)
    if (case == 'success') writeBin(charToRaw('%PDF-1.7 test fixture'), path)
    character(0)
  }
  result <- try(export('<html>test</html>', target), silent = TRUE)
  if (case == 'success') {
    stopifnot(!inherits(result, 'try-error'), rawToChar(readBin(target, 'raw', 5)) == '%PDF-')
  } else {
    stopifnot(inherits(result, 'try-error'), identical(readBin(target, 'raw', 100), original))
  }
  cat(case, 'passed\n')
}
unlink(target)
