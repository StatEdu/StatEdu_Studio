source('output/excel-paragraph-reuse-20260914/common.R')
tables<-list(
 strings='<table><thead><tr><th>항목</th><th>Value</th><th>p</th></tr></thead><tbody><tr><td>0012</td><td>1.230</td><td>&lt; .001</td></tr><tr><td>=1+1</td><td>2026-09-14</td><td></td></tr><tr><td>α &amp; β</td><td>1e-05</td><td>NA</td></tr></tbody></table>',
 merged='<table><thead><tr><th rowspan="2">Group</th><th colspan="3">Effects</th></tr><tr><th>A</th><th>B</th><th>C</th></tr></thead><tbody><tr><td rowspan="2">한글</td><td colspan="2" style="font-weight:bold;text-align:right;">.123</td><td>tail</td></tr><tr><td></td><td colspan="2">merged tail</td></tr></tbody></table>',
 sparse='<table><tr><td>A</td><td>B</td><td>C</td></tr><tr><td>one</td></tr><tr></tr><tr><td colspan="2">two</td><td>three</td></tr></table>',
 single='<table><tr><td>00001</td></tr><tr><td></td></tr><tr><td style="font-style:italic;vertical-align:top;">多言語<br/>two lines</td></tr></table>',
 header_only='<table><thead><tr><th>A</th><th>B</th></tr></thead></table>')
specs<-expand.grid(align=c('left','center','right'),valign=c('top','middle'),
  bold=c(FALSE,TRUE),italic=c(FALSE,TRUE),stringsAsFactors=FALSE)
style_cells<-vapply(seq_len(nrow(specs)),function(i) {
  css<-paste0('text-align:',specs$align[i],';vertical-align:',specs$valign[i],';',
    if(specs$bold[i])'font-weight:bold;'else'',if(specs$italic[i])'font-style:italic;'else'')
  paste0('<td style="',css,'">',i,'</td>')
},character(1))
style_rows<-vapply(split(style_cells,rep(1:6,each=4)),function(cells)paste0('<tr>',paste(cells,collapse=''),'</tr>'),character(1))
tables$styles<-paste0('<table><thead><tr><th>A</th><th>B</th><th>C</th><th>D</th></tr></thead><tbody>',paste(style_rows,collapse=''),'</tbody></table>')
tables$styles_no_header<-paste0('<table>',paste(style_rows,collapse=''),'</table>')
tables$runs <- paste0('<table><thead><tr>',paste(rep('<th>Header</th>',12),collapse=''),
  '</tr></thead><tbody>',paste(vapply(1:8,function(r)paste0('<tr>',
    paste(vapply(1:12,function(c)paste0('<td style="text-align:',
      if(r %% 2L == 0L && c %% 2L == 0L)'right'else'left',';">',r,':',c,'</td>'),character(1)),collapse=''),
    '</tr>'),character(1)),collapse=''),'</tbody></table>')
tables$merge_boundaries <- paste0('<table><tr><td>A</td><td>B</td><td colspan="2">C</td><td>D</td><td>E</td></tr>',
  '<tr><td rowspan="2">F</td><td>G</td><td>H</td><td rowspan="2">I</td><td>J</td><td>K</td></tr>',
  '<tr><td>L</td><td>M</td><td>N</td><td>O</td></tr>',
  '<tr><td style="text-align:left;">P</td><td style="text-align:start;">Q</td><td>R</td></tr></table>')
entries<-lapply(names(tables),function(name)list(title=name,html=paste0('<html><body><h2>',name,'</h2>',tables[[name]],'<p>CI = confidence interval.</p></body></html>')))
for(index in seq_along(entries)) {
  select_variant('baseline');before<-result_entry_tables(entries[[index]])
  select_variant('current');after<-result_entry_tables(entries[[index]])
  stopifnot(identical(before,after,num.eq=FALSE))
  lean<-result_entry_tables(entries[[index]],include_docx=FALSE)
  expected<-lapply(after,function(x){x['docx']<-list(NULL);x})
  stopifnot(identical(expected,lean,num.eq=FALSE),
    identical(vapply(after,result_docx_wide_table,logical(1)),vapply(lean,result_docx_wide_table,logical(1))))
  paths<-list();conditions<-list();rng<-list()
  for(v in c('baseline','current')) {
    select_variant(v);warnings<-character();paths[[v]]<-file.path(root,paste0(v,'-case-',index,'.xlsx'))
    set.seed(941)
    withCallingHandlers(save_screen_excel_file(entries[[index]]$html,paths[[v]]),
      warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')})
    conditions[[v]]<-warnings;rng[[v]]<-.Random.seed
  }
  stopifnot(identical(conditions$baseline,conditions$current),identical(rng$baseline,rng$current))
  compare_packages(paths$baseline,paths$current,paste0('case-',index))
}
for(v in c('baseline','current')) {
  select_variant(v);save_result_collection_excel_file(entries,file.path(root,paste0(v,'-collection.xlsx')))
}
compare_packages(file.path(root,'baseline-collection.xlsx'),file.path(root,'current-collection.xlsx'),'collection')
select_variant('current')
cat('PASS: nine individual exports and one accumulated collection; all sheet/cell/package contents identical except created/modified timestamps.\n')

