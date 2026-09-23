Sys.setlocale('LC_ALL','Korean_Korea.utf8')
source('R/app_bootstrap.R',encoding='UTF-8');load_app_packages(check=FALSE);source_app_modules()
r<-list(use_hc3=TRUE,use_bootstrap=TRUE,residual_diagnostics=TRUE,show_f2=TRUE,auto_method=TRUE,bootstrap_ci_method='bias_corrected')
note<-mediation_moderation_hierarchical_note_line(list(r,r))
symbols<-c('HC3 SE =','LLCI =','ULCI =','VIF =','d(dᵤ-4-dᵤ) =','z(p) =','χ²(p) =','f² =')
pos<-vapply(symbols,function(x)regexpr(x,note,fixed=TRUE)[[1]],integer(1))
stopifnot(all(pos>0),all(diff(pos)>0),!grepl('Note.',note,fixed=TRUE))
for(case in c('simple','parallel','serial','moderation','moderated_mediation','percentile','missing','wide')) {
 result<-readRDS(file.path('outputs/spss_phase36_20260907',case,'analysis.rds'))
 html<-mediation_moderation_saved_results_html(result,language='ko',output_table_style=if(case=='wide')'wide' else 'standard')
 doc<-xml2::read_html(html)
 notes<-xml2::xml_text(xml2::xml_find_all(doc,".//*[contains(concat(' ',normalize-space(@class),' '),' coefficient-note ')]"))
 stopifnot(length(notes)>0,!any(grepl('^Note[.]',notes)),any(grepl('SE =',notes,fixed=TRUE)))
 cat(case,'PASS\n')
 if(case=='moderated_mediation') {
  dir.create('tmp/pdfs/cover-preview',recursive=TRUE,showWarnings=FALSE)
  writeLines(html,'tmp/pdfs/cover-preview/mm-notes.html',useBytes=TRUE)
 }
}
cat('PASS: ordered abbreviations, no prefix, 8 existing result fixtures.\n')
