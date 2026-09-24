source('output/excel-row-batch-20260914/common.R')
tables<-list(
 strings='<table><thead><tr><th>항목</th><th>Value</th><th>p</th></tr></thead><tbody><tr><td>0012</td><td>1.230</td><td>&lt; .001</td></tr><tr><td>=1+1</td><td>2026-09-14</td><td></td></tr><tr><td>α &amp; β</td><td>1e-05</td><td>NA</td></tr></tbody></table>',
 merged='<table><thead><tr><th rowspan="2">Group</th><th colspan="3">Effects</th></tr><tr><th>A</th><th>B</th><th>C</th></tr></thead><tbody><tr><td rowspan="2">한글</td><td colspan="2" style="font-weight:bold;text-align:right;">.123</td><td>tail</td></tr><tr><td></td><td colspan="2">merged tail</td></tr></tbody></table>',
 sparse='<table><tr><td>A</td><td>B</td><td>C</td></tr><tr><td>one</td></tr><tr></tr><tr><td colspan="2">two</td><td>three</td></tr></table>',
 single='<table><tr><td>00001</td></tr><tr><td></td></tr><tr><td style="font-style:italic;vertical-align:top;">多言語<br/>two lines</td></tr></table>',
 header_only='<table><thead><tr><th>A</th><th>B</th></tr></thead></table>')
entries<-lapply(names(tables),function(name)list(title=name,html=paste0('<html><body><h2>',name,'</h2>',tables[[name]],'<p>CI = confidence interval.</p></body></html>')))
for(index in seq_along(entries)) {
  paths<-list();conditions<-list()
  for(v in c('baseline','current')) {
    select_variant(v);warnings<-character();paths[[v]]<-file.path(root,paste0(v,'-case-',index,'.xlsx'))
    withCallingHandlers(save_screen_excel_file(entries[[index]]$html,paths[[v]]),
      warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})
    conditions[[v]]<-warnings
  }
  stopifnot(identical(conditions$baseline,conditions$current))
  compare_packages(paths$baseline,paths$current,paste0('case-',index))
}
for(v in c('baseline','current')) {
  select_variant(v);save_result_collection_excel_file(entries,file.path(root,paste0(v,'-collection.xlsx')))
}
compare_packages(file.path(root,'baseline-collection.xlsx'),file.path(root,'current-collection.xlsx'),'collection')
select_variant('current')
cat('PASS: five individual exports and one accumulated collection; all sheets/cells and every non-core package part identical.\n')
