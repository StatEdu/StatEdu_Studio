.libPaths(R.home('library'))
source('R/app_bootstrap.R');load_app_packages(check=FALSE);source_app_modules();statedu_apply_preferences()
root <- 'output/excel-export-profile-20260914'
dir.create(root,recursive=TRUE,showWarnings=FALSE)
args<-commandArgs(TRUE);id<-if(length(args))args[[1]]else '1'
html<-paste(readLines('output/table-render-combined-20260914/current-correlation.html',warn=FALSE,encoding='UTF-8'),collapse='\n')
records<-list()
for(name in c('result_entry_tables','result_html_table_cells','result_docx_table_payload',
              'result_entry_paragraphs','add_screen_excel_table','result_entry_document')) {
  wrapper<-local({
    original<-get(name,.GlobalEnv);key<-name
    function(...) {
      start<-proc.time()[['elapsed']]
      on.exit({records[[length(records)+1L]]<<-data.frame(helper=key,elapsed=proc.time()[['elapsed']]-start)},add=TRUE)
      original(...)
    }
  })
  assign(name,wrapper,.GlobalEnv)
}
warnings<-character();path<-file.path(root,paste0('profile-',id,'.xlsx'))
profile<-file.path(root,paste0('profile-',id,'.Rprof'))
Rprof(profile,interval=.01)
elapsed<-system.time(withCallingHandlers(save_screen_excel_file(html,path),warning=function(w){warnings<<-c(warnings,conditionMessage(w));invokeRestart('muffleWarning')}))[['elapsed']]
Rprof(NULL)
records<-do.call(rbind,records);write.csv(records,file.path(root,paste0('helpers-',id,'.csv')),row.names=FALSE)
totals<-aggregate(elapsed~helper,records,sum);print(totals);cat('Total elapsed:',elapsed,'seconds\n')
saveRDS(list(elapsed=elapsed,warnings=warnings),file.path(root,paste0('run-',id,'.rds')))
p<-summaryRprof(profile);write.csv(p$by.total,file.path(root,paste0('total-',id,'.csv')));write.csv(p$by.self,file.path(root,paste0('self-',id,'.csv')))
reference<-'output/table-render-combined-20260914/current-correlation_30.xlsx'
sheets<-openxlsx::getSheetNames(reference);stopifnot(identical(sheets,openxlsx::getSheetNames(path)))
for(s in sheets)stopifnot(identical(openxlsx::read.xlsx(reference,sheet=s),openxlsx::read.xlsx(path,sheet=s),num.eq=FALSE))
# Compare package parts as bytes; creation/modification metadata is time dependent.
before<-file.path(root,paste0('reference-',id));after<-file.path(root,paste0('actual-',id))
dir.create(before);dir.create(after);unzip(reference,exdir=before);unzip(path,exdir=after)
files<-list.files(before,recursive=TRUE);stopifnot(identical(files,list.files(after,recursive=TRUE)))
for(file in setdiff(files,'docProps/core.xml')) {
  a<-file.path(before,file);b<-file.path(after,file)
  stopifnot(identical(readBin(a,'raw',n=file.info(a)$size),readBin(b,'raw',n=file.info(b)$size)))
}
cat('PASS:',length(sheets),'sheet values and',length(files)-1L,'non-core package parts identical; warnings:',length(warnings),'\n')
