if (.Platform$OS.type == 'windows') Sys.setlocale('LC_CTYPE','Korean_Korea.utf8')
source('scripts/validate_hwpx_geometry.R', encoding='UTF-8')
expressions <- parse('tmp/result_saved_ui_before_word_geometry.R', encoding='UTF-8')
definition <- Filter(function(x) is.call(x) && identical(x[[1L]], as.name('<-')) &&
  identical(x[[2L]], as.name('result_document_table')), as.list(expressions))[[1L]]
legacy <- eval(definition[[3L]])
current <- result_document_table
serializer <- get('gen_raw_wml', asNamespace('flextable'))
document <- officer::read_docx()
check <- function(info) {
  before <- legacy(info); after <- current(info)
  difference <- all.equal(before, after, tolerance=1e-12)
  if (!isTRUE(difference)) print(list(title=info$title,difference=difference))
  stopifnot(isTRUE(all.equal(before, after, tolerance=1e-12)),
    identical(serializer(before, doc=document), serializer(after, doc=document)))
}
for(info in cases) check(info)
flextable::set_flextable_defaults(theme_fun=function(x) flextable::italic(x, part='all'))
check(plain)
do.call(flextable::set_flextable_defaults, saved_defaults)
cat('PASS 35 Word table objects and serialized XML; custom theme fallback\n')
